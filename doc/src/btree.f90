!> This modules implements a binary tree sampler
!> It provides a way to sample elements based on their weights using a binary tree structure.
module samplers_btree_mod
    use kinds_mod
    use samplers_base_mod
    implicit none
    private

    !> Constructor
    interface weighted_sampler
        module procedure weighted_sampler_new
    end interface

    !> Derived type, extending from the base
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

    !> Create a new binary tree sampler with N weights
    !> Input: n - number of weights
    function weighted_sampler_new(n) result(this)
        type(weighted_sampler_t) :: this
        integer(i4), intent(in) :: n

        call this%init(n)

    end function weighted_sampler_new

    !> Initializes the structure with N weights
    !> Input: n - number of weights
    subroutine sampler_init(this, n)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n

        this%n = n
        allocate(this%weights(n))
        allocate(this%tree(2*n - 1))  ! complete binary tree with n leaves
        this%weights = 0.0_dp
        this%tree = 0.0_dp
    end subroutine sampler_init

    !> Placeholder for 1D initialization (compat mode), maps to original init
    !> Input: n - number of weights
    !>        w - any real number, not used
    subroutine sampler_init_w(this, n, w)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w

        call this%init(n)  ! call original init
    end subroutine sampler_init_w

    !> Placeholder for 1D initialization (compat mode), maps to original init
    !> Input: n - number of weights
    !>        w1 - any real number, not used
    !>        w2 - any real number, not used
    subroutine sampler_init_w2(this, n, w1,w2)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w1,w2

        call this%init(n)  ! call original init
    end subroutine sampler_init_w2

    !> Resets the sampler: clears the list
    subroutine sampler_reset(this)
        class(weighted_sampler_t), intent(inout) :: this

        ! DEBUG
        !if (this%n <= 0) error stop "reset: sampler not initialized"

        this%weights = 0.0_dp
        this%tree = 0.0_dp
    end subroutine sampler_reset

    !> Sets the weight for a given index
    !> Input: index - index of the element with a given weight
    !>        weight - weight of the element
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

        ! we perform a bottom-up update of the tree
        i = index + this%n - 1
        do while (i >= 1)
            this%tree(i) = this%tree(i) + delta
            i = i / 2
        end do
    end subroutine sampler_set_weight

    !> Sets the weights from an array (full), using its indexes
    !> Weights should be larger than zero
    !> It assumes it was initialized before, and has the same size
    !> Input: weights - array with the weights
    subroutine sampler_set_weight_array(this, weights)
        class(weighted_sampler_t), intent(inout) :: this
        real(dp), intent(in) :: weights(:)
        integer(i4) :: i

        ! DEBUG
        !if (size(weights) /= this%n) error stop 'Weights array size does not match sampler size'

        ! Copy the weights
        this%weights = weights

        ! Define the weights
        do i = 1, this%n
            this%tree(i + this%n - 1) = weights(i)
        end do

        ! Perform a bottom-up update of the tree
        do i = this%n - 1, 1, -1
            this%tree(i) = this%tree(2 * i) + this%tree(2 * i + 1)
        end do
    end subroutine sampler_set_weight_array

    !> Adds a weight to the sampler at a given index
    !> Input: index - index of the element
    !>        delta_weight - difference to add to its weight
    subroutine sampler_add_weight(this, index, delta_weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: delta_weight
        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_add_weight'

        call this%set_weight(index, this%weights(index) + delta_weight)  ! call set_weight for each weight
    end subroutine sampler_add_weight

    !> Remove an index from the sampler
    !> Important: the index is the original one, not the index used internally by the sampler
    !> Input: index - index of the element to be removed
    subroutine sampler_remove(this, index)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index

        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_remove'

        call this%set_weight(index, 0.0_dp) ! we just update weight to zero
    end subroutine sampler_remove

    !> Samples an index from the sampler
    !> Input: gen - random number generator (rndgen-fortran module)
    !> Output: index - sampled index
    function sampler_sample(this, gen) result(index)
        use rndgen_mod
        class(weighted_sampler_t), intent(in) :: this
        class(rndgen), intent(inout) :: gen
        integer(i4) :: index
        real(dp) :: r
        integer(i4) :: i

        ! DEBUG
        ! if (this%tree(1) <= 0.0_dp) error stop "sample: total weight is zero"

        ! for that, we calculate a random value from the top (sum of weights)
        r = gen%rnd() * this%tree(1)

        ! we start to go down, performing a binary search
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

    !> Get the sum of all weights
    !> Output: total - sum of all weights
    function sampler_sum(this) result(total)
        class(weighted_sampler_t), intent(in) :: this
        real(dp) :: total

        ! DEBUG
        !if (this%n <= 0) error stop "sum: sampler not initialized"

        total = this%tree(1)  ! the root contains the total sum
    end function sampler_sum

    !> Finalize the sampler, deallocate resources
    subroutine sampler_finalize(this)
        type(weighted_sampler_t), intent(inout) :: this

        if (allocated(this%weights)) deallocate(this%weights)
        if (allocated(this%tree)) deallocate(this%tree)

        this%n = 0
    end subroutine sampler_finalize

end module samplers_btree_mod
