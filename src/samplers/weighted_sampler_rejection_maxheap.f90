module hoga_weighted_sampler_rejection_maxheap_mod
    use hoga_kinds_mod
    use hOGA_lists_mod
    use hoga_weighted_sampler_base_mod
    implicit none
    private

    real(kind=dp), parameter :: EPSILON = 0.0_dp !1.0e-16_dp

    interface weighted_sampler
        module procedure weighted_sampler_new
    end interface

    type, extends(weighted_sampler_base_t) :: weighted_sampler_t
        type(dynamical_list_t), allocatable :: indices
        type(maxheap_t), allocatable :: heap ! max-heap for efficient sampling
        integer(i4), allocatable :: position_of(:) ! index positions
        real(kind=dp) :: current_sum = 0.0_dp ! current sum of the weights
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
    end type

    public :: weighted_sampler, weighted_sampler_t

contains

    !> Create a new rejection sampler with N weights
    function weighted_sampler_new(n) result(this)
        type(weighted_sampler_t) :: this
        integer(i4), intent(in) :: n

        call this%init(n)

    end function weighted_sampler_new

    !> Initializes the structure with N weights
    subroutine sampler_init(this, n)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n

        this%n = n
        allocate(this%weights(n))
        allocate(this%indices)
        allocate(this%position_of(n))
        call this%indices%init(n)
        this%position_of = 0
        this%current_sum = 0.0_dp

        this%heap = maxheap(n) ! Initialize the max-heap with size n
        this%weights = 0.0_dp ! Initialize weights to zero
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
    
    !> Resets the sampler: clear the list
    subroutine sampler_reset(this)
        class(weighted_sampler_t), intent(inout) :: this

        call this%indices%reset()    
        this%position_of = 0
        this%current_sum = 0.0_dp
        this%weights = 0.0_dp

        deallocate(this%heap)
        this%heap = maxheap(this%n) ! Reinitialize the max-heap with size n
    end subroutine sampler_reset

    !> Sets the weight for a given index
    subroutine sampler_set_weight(this, index, weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: weight

        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_set_weight'

        ! if the weight is zero, remove the index from the list
        if (weight <= EPSILON) then
            call this%remove(index)
            return
        end if

        ! check if the index is already added to the list
        if (this%position_of(index) /= 0) then
            ! remove the current sum
            this%current_sum = this%current_sum - this%weights(index)
            call this%heap%remove(index)
        else
            call this%indices%add(index)
            this%position_of(index) = this%indices%n_used            
        end if

        this%weights(index) = weight

        ! update the current sum
        this%current_sum = this%current_sum + weight ! we do not remove the last weight since it is a new one

        ! update the maximum weight
        call this%heap%add(weight, index) ! Add to the max-heap
    end subroutine sampler_set_weight

    !> Sets the weights from an array (full), assuming all larger than zero (IMPORTANT!)
    subroutine sampler_set_weight_array(this, weights)
        class(weighted_sampler_t), intent(inout) :: this
        real(dp), intent(in) :: weights(:)
        integer(i4) :: i

        ! DEBUG
        !if (size(weights) /= this%n) error stop 'Weights array size does not match sampler size'

        this%weights = weights
        call this%indices%reset() ! Reset the indices list
        this%current_sum = 0.0_dp
        do i = 1, this%n
            call this%indices%add(i)
            call this%heap%add(weights(i), i) ! Add to the max-heap
            this%position_of(i) = this%indices%n_used
            this%current_sum = this%current_sum + weights(i)
        end do
       
    end subroutine sampler_set_weight_array

    !> Adds a weight to the sampler at a given index
    subroutine sampler_add_weight(this, index, delta_weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: delta_weight

        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_add_weight'

        call this%set_weight(index, this%weights(index) + delta_weight)
        
    end subroutine sampler_add_weight

    !> Remove an index from the sampler
    subroutine sampler_remove(this, index)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        integer(i4) :: pos

        ! DEBUG
        !if (index < 1 .or. index > this%n) error stop 'Index out of bounds in sampler_remove'

        pos = this%position_of(index)
        if (pos /= 0) then
            ! Update the current sum
            this%current_sum = this%current_sum - this%weights(index)

            ! Update the position_of the last element of the list
            this%position_of(this%indices%list(this%indices%n_used)) = pos

            ! Remove the weight from the list
            call this%indices%remove(pos)
            this%position_of(index) = 0 ! Mark as removed

            this%weights(index) = 0.0_dp ! Set the weight to zero
            
            call this%heap%remove(index) ! Remove from the max-heap
        end if
        
    end subroutine sampler_remove

    !> Retorna um índice proporcional aos pesos
    function sampler_sample(this, gen) result(index)
        use rndgen_mod
        class(weighted_sampler_t), intent(in) :: this
        class(rndgen), intent(inout) :: gen
        integer(i4) :: index, index_pos
        real(dp) :: weight

        ! We will use rejection sampling to select an index
        do
            ! Select a random index from the list of indices
            index_pos = gen%int(1, this%indices%n_used)
            index = this%indices%list(index_pos)

            weight = this%weights(index)

            ! Check if the rate is sufficient to accept this index
            if (gen%rnd() < weight / this%heap%max_value()) return
            ! else, we reject and try again
        end do
        
    end function

    !> Get the sum of all weights
    function sampler_sum(this) result(total_weight)
        class(weighted_sampler_t), intent(in) :: this
        real(dp) :: total_weight
        ! DEBUG     
        !integer(i4) :: i   
        !real(dp), parameter :: tol = 1.0e-12_dp
        !total_weight = 0.0_dp
        !do i = 1, this%indices%n_used
        !    total_weight = total_weight + this%weights(this%indices%list(i))            
        !end do
        !if (abs(total_weight - this%current_sum) > tol * max(abs(total_weight), abs(this%current_sum), 1.0_dp)) then
        !    print*, total_weight, this%current_sum
        !    error stop 'Inconsistency in total weight calculation'
        !end if

        total_weight = this%current_sum

    end function sampler_sum

    !> Finalize the sampler, deallocate resources
    subroutine sampler_finalize(this)
        type(weighted_sampler_t), intent(inout) :: this

        if (allocated(this%indices)) then
            deallocate(this%indices)
        end if

        if (allocated(this%weights)) then
            deallocate(this%weights)
        end if

        if (allocated(this%position_of)) then
            deallocate(this%position_of)
        end if

    end subroutine sampler_finalize

end module
