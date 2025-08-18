!> This module implements rejection sampling with two classes
!> We split the weights into two classes based on a threshold
module samplers_rejection_maxheap_two_classes_mod
    use kinds_mod
    use lists_mod
    use samplers_rejection_maxheap_mod, only : rejection_maxheap_t => weighted_sampler_t
    use samplers_base_mod
    implicit none
    private

    !> Constructor
    interface weighted_sampler
        module procedure weighted_sampler_new
    end interface weighted_sampler

    !> Derived type, extending from the base
    type, extends(sampler_base_t) :: weighted_sampler_t
        type(rejection_maxheap_t) :: samplers(2)
        real(dp) :: threshold = huge(dp) ! threshold for the weights
        integer(i4), allocatable :: sampler_of_index(:) ! maps index to sampler (1 or 2)
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

        this%n = n ! Set the number of weights

        ! init each sampler with size n
        call this%samplers(1)%init(n)
        call this%samplers(2)%init(n)

        allocate(this%sampler_of_index(n))
        allocate(this%weights(n))
        this%sampler_of_index = 0 ! Initialize to zero (no sampler assigned yet)

        this%weights = 0.0_dp ! Initialize weights to zero
    end subroutine sampler_init

    !> Initializes with size n and threshold w
    !> Input: n - number of weights
    !>        w - threshold for the weights
    subroutine sampler_init_w(this, n, w)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w

        call this%init(n)  ! call original init
        this%threshold = w ! set the threshold for the weights
    end subroutine sampler_init_w

    !> Placeholder for 1D initialization (compat mode), maps to original init
    !> Input: n - number of weights
    !>        w1 - any real number, not used
    !>        w2 - any real number, not used
    subroutine sampler_init_w2(this, n, w1,w2)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w1,w2

        call this%init(n,w1)  ! call original init
    end subroutine sampler_init_w2

    !> Resets the sampler: clears the list
    subroutine sampler_reset(this)
        class(weighted_sampler_t), intent(inout) :: this

        call this%samplers(1)%reset() ! Reset the first sampler
        call this%samplers(2)%reset() ! Reset the second sampler
        this%sampler_of_index = 0 ! Reset the index mapping
        this%weights = 0.0_dp ! Reset the weights
    end subroutine sampler_reset

    !> Sets the weight for a given index
    !> Input: index - index of the element with a given weight
    !>        weight - weight of the element
    subroutine sampler_set_weight(this, index, weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: weight
        integer(i4) :: old_sampler_pos

        ! get the sampler pos
        old_sampler_pos = this%sampler_of_index(index)

        ! We select which sampler will receive the weight based on the threshold
        ! The first sampler will receive weights below the threshold
        ! The second sampler will receive weights above the threshold
        if (weight < this%threshold) then
            call this%samplers(1)%set_weight(index, weight) ! Set the weight in the first sampler
            this%sampler_of_index(index) = 1 ! Map index to first sampler

            ! If it was in the second sampler, we need to remove it
            if (old_sampler_pos == 2) call this%samplers(2)%remove(index)
        else
            call this%samplers(2)%set_weight(index, weight) ! Set the weight in the second sampler
            this%sampler_of_index(index) = 2 ! Map index to second sampler

            ! If it was in the first sampler, we need to remove it
            if (old_sampler_pos == 1) call this%samplers(1)%remove(index)
        end if

        ! update the weights array
        this%weights(index) = weight
    end subroutine sampler_set_weight

    !> Sets the weights from an array (full), using its indexes
    !> Weights should be larger than zero
    !> It assumes it was initialized before, and has the same size
    !> Input: weights - array with the weights
    subroutine sampler_set_weight_array(this, weights)
        class(weighted_sampler_t), intent(inout) :: this
        real(dp), intent(in) :: weights(:)
        integer(i4) :: i

        ! We select which sampler will receive the weights based on the threshold
        ! The first sampler will receive weights below the threshold
        ! The second sampler will receive weights above the threshold
        do i = 1, size(weights)
            call this%set_weight(i, weights(i))
        end do

    end subroutine sampler_set_weight_array

    !> Adds a weight to the sampler at a given index
    !> Input: index - index of the element
    !>        delta_weight - difference to add to its weight
    subroutine sampler_add_weight(this, index, delta_weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: delta_weight
        real(dp) :: weight

        weight = this%weights(index) + delta_weight ! Calculate the new weight

        ! we first remove the element
        call this%remove(index) ! Remove the index from the sampler

        ! Now we can add the weight again
        call this%set_weight(index, weight) ! Set the new weight

    end subroutine sampler_add_weight

    !> Remove an index from the sampler
    !> Important: the index is the original one, not the index used internally by the sampler
    !> Input: index - index of the element to be removed
    subroutine sampler_remove(this, index)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        integer(i4) :: sampler_pos

        sampler_pos = this%sampler_of_index(index) ! Get the sampler position for the index

        if (sampler_pos == 0) return ! It is not mapped to any sampler, just ignore

        call this%samplers(sampler_pos)%remove(index) ! Remove weight in the first sampler
        this%sampler_of_index(index) = 0 ! Unmap index

        ! update the weights array
        this%weights(index) = 0.0_dp ! Set the weight to zero

    end subroutine sampler_remove

    !> Samples an index from the sampler
    !> Input: gen - random number generator (rndgen-fortran module)
    !> Output: index - sampled index
    function sampler_sample(this, gen) result(index)
        use rndgen_mod
        class(weighted_sampler_t), intent(in) :: this
        class(rndgen), intent(inout) :: gen
        integer(i4) :: index
        real(dp) :: total_weight

        total_weight = this%sum()

        ! We will use rejection sampling to select an index
        ! First, we see which sampler to use based on the total weight
        if (gen%rnd() < this%samplers(1)%sum() / total_weight) then
            ! Use the first sampler
            index = this%samplers(1)%sample(gen)
        else
            ! Use the second sampler
            index = this%samplers(2)%sample(gen)
        end if

    end function

    !> Get the sum of all weights
    !> Output: total - sum of all weights
    function sampler_sum(this) result(total_weight)
        class(weighted_sampler_t), intent(in) :: this
        real(dp) :: total_weight

        total_weight = this%samplers(1)%sum() + this%samplers(2)%sum()

    end function sampler_sum

    !> Finalize the sampler, deallocate resources
    subroutine sampler_finalize(this)
        type(weighted_sampler_t), intent(inout) :: this

        if (allocated(this%sampler_of_index)) then
            deallocate(this%sampler_of_index)
        end if
        if (allocated(this%weights)) then
            deallocate(this%weights)
        end if

    end subroutine sampler_finalize

end module samplers_rejection_maxheap_two_classes_mod
