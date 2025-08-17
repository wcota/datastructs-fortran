module samplers_btree_mod
    use kinds_mod
    use samplers_base_mod
    implicit none
    private

    interface weighted_sampler
        module procedure weighted_sampler_new
    end interface

    type, extends(sampler_base_t) :: weighted_sampler_t
        real(dp), allocatable :: tree(:)         ! soma das subárvores
    contains
        procedure :: init_n => sampler_init
        procedure :: init_w => sampler_init_w
        procedure :: init_w2 => sampler_init_w2
        procedure :: reset        => sampler_reset
        procedure :: set_weight   => sampler_set_weight
        procedure :: set_weight_array => sampler_set_weight_array
        procedure :: add_weight => sampler_add_weight
        procedure :: sample       => sampler_sample
        procedure :: remove => sampler_remove
        procedure :: sum => sampler_sum

        final :: sampler_finalize
    end type weighted_sampler_t

    public :: weighted_sampler, weighted_sampler_t

contains

    !> Cria um novo weighted_sampler com N pesos
    function weighted_sampler_new(n) result(this)
        type(weighted_sampler_t) :: this
        integer(i4), intent(in) :: n

        call this%init(n)

    end function weighted_sampler_new

    !> Inicializa a estrutura com N pesos todos nulos
    subroutine sampler_init(this, n)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n

        this%n = n
        allocate(this%weights(n))
        allocate(this%tree(2*n - 1))  ! árvore binária completa com n folhas
        this%weights = 0.0_dp
        this%tree = 0.0_dp
    end subroutine sampler_init

    !> Placeholder for 1D initialization (compat mode), maps to original init
    subroutine sampler_init_w(this, n, w)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w

        call this%init(n)  ! call original init
    end subroutine sampler_init_w

    !> Placeholder for 1D initialization (compat mode), maps to original init
    subroutine sampler_init_w2(this, n, w1,w2)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w1,w2

        call this%init(n)  ! call original init
    end subroutine sampler_init_w2

    ! !> Inicializa a estrutura com N x M pesos todos nulos
    ! subroutine sampler_init_2d(this, n, m)
    !     class(weighted_sampler_t), intent(inout) :: this
    !     integer(i4), intent(in) :: n, m

    !     this%n = n * m
    !     allocate(this%weights(this%n))
    !     allocate(this%tree(2*this%n - 1))  ! árvore binária completa com n folhas
    !     this%weights = 0.0_dp
    !     this%tree = 0.0_dp

    !     allocate(this%dim_1_n)  ! dimensão 1
    !     allocate(this%dim_2_m)  ! dimensão 2

    !     this%dim_1_n = n
    !     this%dim_2_m = m
    ! end subroutine sampler_init_2d

    !> Retorna a soma total dos pesos

    !> Reseta os pesos para zero
    subroutine sampler_reset(this)
        class(weighted_sampler_t), intent(inout) :: this
        
        ! DEBUG
        !if (this%n <= 0) error stop "reset: sampler not initialized"

        this%weights = 0.0_dp
        this%tree = 0.0_dp
    end subroutine sampler_reset

    !> Atualiza o peso de um índice e ajusta a árvore
    subroutine sampler_set_weight(this, index, weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: weight
        integer(i4) :: i
        real(dp) :: delta

        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_set_weight'

        delta = weight - this%weights(index)
        !if (delta == 0.0_dp) return
        this%weights(index) = weight

        i = index + this%n - 1
        do while (i >= 1)
            this%tree(i) = this%tree(i) + delta
            i = i / 2
        end do
    end subroutine sampler_set_weight

    !> Atualiza os pesos de um array e ajusta a árvore
    subroutine sampler_set_weight_array(this, weights)
        class(weighted_sampler_t), intent(inout) :: this
        real(dp), intent(in) :: weights(:)
        integer(i4) :: i

        ! DEBUG
        !if (size(weights) /= this%n) error stop 'Weights array size does not match sampler size'

        ! Copia os pesos
        this%weights = weights

        ! Preenche as folhas da árvore
        do i = 1, this%n
            this%tree(i + this%n - 1) = weights(i)
        end do

        ! Constrói os nós internos
        do i = this%n - 1, 1, -1
            this%tree(i) = this%tree(2 * i) + this%tree(2 * i + 1)
        end do
    end subroutine sampler_set_weight_array

    ! !> Atualiza o peso de um índice e ajusta a árvore para 2D
    ! !> First, map the 2D index to 1D, and use the 1D version
    ! subroutine sampler_set_weight_2d(this, index_1, index_2, weight)
    !     class(weighted_sampler_t), intent(inout) :: this
    !     integer(i4), intent(in) :: index_1, index_2
    !     real(dp), intent(in) :: weight
    !     integer(i4) :: index

    !     if (index_1 < 1 .or. index_1 > this%dim_1_n .or. &
    !         index_2 < 1 .or. index_2 > this%dim_2_m) stop "set_weight: invalid indices"

    !     index = (index_1 - 1) * this%dim_2_m + index_2  ! mapeia 2D para 1D

    !     call this%set_weight(index, weight)  ! chama o set_weight para 1D
    ! end subroutine sampler_set_weight_2d

    !> Adiciona um peso ao índice e ajusta a árvore
    subroutine sampler_add_weight(this, index, delta_weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: delta_weight
        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_add_weight'

        call this%set_weight(index, this%weights(index) + delta_weight)  ! chama o set_weight para atualizar o peso
    end subroutine sampler_add_weight

    !> Remove um índice e ajusta a árvore
    subroutine sampler_remove(this, index)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index

        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_remove'

        call this%set_weight(index, 0.0_dp)  ! chama o set_weight para zerar o peso
    end subroutine sampler_remove

    !> Retorna um índice proporcional aos pesos
    function sampler_sample(this, gen) result(index)
        use rndgen_mod
        class(weighted_sampler_t), intent(in) :: this
        class(rndgen), intent(inout) :: gen
        integer(i4) :: index
        real(dp) :: r
        integer(i4) :: i

        ! DEBUG
        ! if (this%tree(1) <= 0.0_dp) error stop "sample: total weight is zero"

        r = gen%rnd() * this%tree(1)
        i = 1

        do while (i < this%n)
            if (r <= this%tree(2*i)) then
                i = 2*i
            else
                r = r - this%tree(2*i)
                i = 2*i + 1
            end if
        end do

        index = i - this%n + 1
    end function sampler_sample

    function sampler_sum(this) result(total)
        class(weighted_sampler_t), intent(in) :: this
        real(dp) :: total

        ! DEBUG
        !if (this%n <= 0) error stop "sum: sampler not initialized"

        total = this%tree(1)  ! a raiz da árvore contém a soma total
    end function sampler_sum

    ! !> Retorna um índice proporcional aos pesos para 2D, array of size 2
    ! function sampler_sample_2d(this, gen) result(indexes)
    !     use rndgen_mod
    !     class(weighted_sampler_t), intent(in) :: this
    !     class(rndgen), intent(inout) :: gen
    !     integer(i4), dimension(2) :: indexes
    !     integer(i4) :: original_index

    !     original_index = this%sample(gen)  ! chama o sample 1D

    !     ! Mapeia o índice 1D de volta para 2D
    !     indexes(1) = (original_index - 1) / this%dim_2_m + 1  ! linha
    !     indexes(2) = mod(original_index - 1, this%dim_2_m) + 1  ! coluna

    ! end function sampler_sample_2d

    !> Libera a memória alocada
    subroutine sampler_finalize(this)
        type(weighted_sampler_t), intent(inout) :: this

        if (allocated(this%weights)) deallocate(this%weights)
        if (allocated(this%tree)) deallocate(this%tree)

        this%n = 0
    end subroutine sampler_finalize

end module
