!> This module implements a rejection composition sampler
!> It is based on "St-Onge, G., Young, J. G., Hébert-Dufresne, L., & Dubé, L. J. (2019).
!> Efficient sampling of spreading processes on complex networks using a composition and rejection algorithm.
!> Computer physics communications, 240, 30-37"
!> <https://doi.org/10.1016/j.cpc.2019.02.008>
module samplers_rejection_maxheap_composition_mod
    use kinds_mod
    use lists_mod
    use samplers_rejection_maxheap_mod, only : rejection_maxheap_t => weighted_sampler_t
    use samplers_btree_mod, only : btree_t => weighted_sampler_t
    use samplers_base_mod
    implicit none
    private

    !> Constructor
    interface weighted_sampler
        module procedure weighted_sampler_new
    end interface weighted_sampler

    !> Derived type, extending from the base
    type, extends(sampler_base_t) :: weighted_sampler_t
        type(rejection_maxheap_t), allocatable :: samplers(:)
        integer(i4) :: q = 0 ! number of groups
        type(btree_t) :: btree ! btree for the samplers selection (size q)
        integer(i4), allocatable :: sampler_of_index(:) ! maps index to sampler
        real(dp) :: wmin, wmax
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
        procedure :: sampler_pos => sampler_sampler_pos

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
        integer(i4) :: i

        this%n = n ! Set the number of weights

        ! DEBUG
        if (this%q == 0) then
            error stop 'Error: q must be set before initializing the sampler.'
        end if

        allocate(this%samplers(this%q))

        call this%btree%init(this%q) ! Initialize the btree for sampler selection

        ! init each sampler with size n
        do i = 1, this%q
            call this%samplers(i)%init(n) ! Initialize each sampler with n weights
        end do

        allocate(this%sampler_of_index(n))
        allocate(this%weights(n))
        this%sampler_of_index = 0 ! Initialize to zero (no sampler assigned yet)

        this%weights = 0.0_dp ! Initialize weights to zero
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

    !> Initializes the sampler giving its minimum and maximum weights
    !> This calculates the number of rejection samplers to use
    !> Input: n - number of weights
    !>        w1 - minimum weight
    !>        w2 - maximum weight
    subroutine sampler_init_w2(this, n, w1,w2)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: n
        real(dp), intent(in) :: w1, w2 ! omega min and max

        ! calculate the number q (see https://arxiv.org/pdf/1808.05859)

        ! ensure that w1 is larger than zero
        if (w1 <= 0.0_dp) then
            error stop 'Error: min weight must be larger than zero.'
        end if

        this%wmin = w1 ! Set the minimum weight
        this%wmax = w2 ! Set the maximum weight

        this%q = max(1, ceiling(log(this%wmax / this%wmin) / log(2.0_dp))) ! number of groups

        !write(*, fmt_general) 'Number of groups (q): ', this%q

        call this%init(n)  ! call original init
    end subroutine sampler_init_w2

    !> Resets the sampler: clears the list
    subroutine sampler_reset(this)
        class(weighted_sampler_t), intent(inout) :: this
        integer(kind=i4) :: sampler_pos

        ! reset all samplers
        do sampler_pos = 1, this%q
            call this%samplers(sampler_pos)%reset()
        end do
        this%sampler_of_index = 0 ! Reset the index mapping
        this%weights = 0.0_dp ! Reset the weights
        call this%btree%reset() ! Reset the btree
    end subroutine sampler_reset

    !> Sets the weight for a given index
    !> Input: index - index of the element with a given weight
    !>        weight - weight of the element
    subroutine sampler_set_weight(this, index, weight)
        class(weighted_sampler_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        real(dp), intent(in) :: weight
        integer(i4) :: sampler_pos

        ! first we remove the weight, just to be sure
        call this%remove(index) ! Remove the index from the sampler if it exists

        if (weight <= 0.0_dp) then
            return
        end if

        sampler_pos = this%sampler_pos(weight) ! Get the sampler position based on the weight

        call this%samplers(sampler_pos)%set_weight(index, weight) ! Set the weight in the first sampler
        this%sampler_of_index(index) = sampler_pos ! Map index to the corresponding sampler

        ! add weight to the btree
        call this%btree%add_weight(sampler_pos, weight)

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

        call this%reset() ! Reset the sampler before setting weights

        ! loop over all elements and set the weights
        do i = 1, size(weights)
            call this%set_weight(i, weights(i)) ! Set the weight for each index
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
        real(dp) :: weight

        sampler_pos = this%sampler_of_index(index) ! Get the sampler position for the index

        if (sampler_pos == 0) return ! It is not mapped to any sampler, just ignore

        call this%samplers(sampler_pos)%remove(index) ! Remove weight in the first sampler
        this%sampler_of_index(index) = 0 ! Unmap index from first sampler

        weight = this%weights(index) ! Get the weight for the index

        ! update the weights array
        this%weights(index) = 0.0_dp ! Set the weight to zero

        ! remove weight from the btree
        call this%btree%add_weight(sampler_pos, -weight)

    end subroutine sampler_remove

    !> Samples an index from the sampler
    !> Input: gen - random number generator (rndgen-fortran module)
    function sampler_sample(this, gen) result(index)
        use rndgen_mod
        class(weighted_sampler_t), intent(in) :: this
        class(rndgen), intent(inout) :: gen
        integer(i4) :: index
        integer(i4) :: sampler_pos

        sampler_pos = this%btree%sample(gen) ! Get a sampler position from the btree

        ! samples from the corresponding rejection sampler
        index = this%samplers(sampler_pos)%sample(gen)

    end function

    !> Get the sum of all weights
    function sampler_sum(this) result(total_weight)
        class(weighted_sampler_t), intent(in) :: this
        real(dp) :: total_weight

        total_weight = this%btree%sum()

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

    !> Auxiliary function (private) to get the sampler position for a given weight
    !> Input: weight - the weight to find the sampler position for
    function sampler_sampler_pos(this, weight) result(pos)
        class(weighted_sampler_t), intent(in) :: this
        real(dp), intent(in) :: weight
        integer(i4) :: pos

        if (weight < this%wmin) then
            pos = 1
        else
            ! Calculate the sampler position based on the weight
            pos = min(this%q, ceiling(log(weight / this%wmin) / log(2.0_dp)) + 1)
        end if

    end function sampler_sampler_pos

end module samplers_rejection_maxheap_composition_mod
