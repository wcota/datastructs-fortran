!> Composition and rejection weighted sampler with geometric binning.
!>
!> Implements the composition-rejection algorithm of St-Onge et al. (2019):
!> > St-Onge, G., Young, J. G., Hébert-Dufresne, L., & Dubé, L. J. (2019).
!> > *Efficient sampling of spreading processes on complex networks using a composition and rejection algorithm.*
!> > Comput. Phys. Commun., 240, 30–37. <https://doi.org/10.1016/j.cpc.2019.02.008>
!>
!> Partitions weights geometrically into \( q = \lceil \log_2(w_{\text{max}} / w_{\text{min}}) \rceil + 1 \)
!> bins such that in each bin \( B_k \), all weights satisfy \( w_i \in [w_{\text{min}} 2^{k-1}, w_{\text{min}} 2^k) \).
!> A small binary tree (`btree_t`) of size \( q \) selects the active bin in \( O(\log q) \),
!> and rejection sampling inside the bin accepts with efficiency \( \ge 50\% \).
!>
!> Complexity:
!> - **Sampling**: \( O(\log q) \) expected time (effectively \( O(1) \) for typical physical rate ranges).
!> - **Weight update**: \( O(\log q + \log N_k) \).
!> - **Memory**: \( O(N) \).
module datastructs_samplers_rejection_maxheap_composition_mod
    use datastructs_kinds_mod
    use datastructs_lists_mod
    use datastructs_samplers_rejection_maxheap_mod, only : rejection_maxheap_t => weighted_sampler_t
    use datastructs_samplers_btree_mod, only : btree_t => weighted_sampler_t
    use datastructs_samplers_base_mod
    implicit none
    private

    real(kind=dp), parameter :: EPSILON = 0.0_dp

    !> Constructor
    interface weighted_sampler
        module procedure weighted_sampler_new
    end interface weighted_sampler

    !> Derived type implementing the composition-rejection sampler
    type, extends(sampler_base_t) :: weighted_sampler_t
        type(rejection_maxheap_t), allocatable :: samplers(:) !! Array of sub-samplers for each geometric bin (size q)
        integer(i4) :: q = 0 !! Total number of geometric weight bins
        type(btree_t) :: btree !! Binary tree of size q selecting active bins proportionally to bin totals
        integer(i4), allocatable :: sampler_of_index(:) !! Map from element index to its current bin (1..q)
        real(dp) :: wmin = 0.0_dp !! Minimum anticipated weight bound
        real(dp) :: wmax = 0.0_dp !! Maximum anticipated weight bound
    contains
        procedure :: init_n => sampler_init !! Initializes with capacity N (default wmin=1, wmax=N)
        procedure :: init_w => sampler_init_w !! Initializes with capacity N and known wmax
        procedure :: init_w2 => sampler_init_w2 !! Initializes with capacity N, wmin, and wmax
        procedure :: reset        => sampler_reset !! Clears all bins and the selection tree
        procedure :: set_weight   => sampler_set_weight !! Assigns weight to an index and places it in the proper bin
        procedure :: set_weight_array => sampler_set_weight_array !! Sets all weights
        procedure :: add_weight => sampler_add_weight !! Adds delta weight and re-bins if necessary
        procedure :: sample       => sampler_sample !! Samples an index via composition then rejection
        procedure :: remove => sampler_remove !! Removes element from its bin in O(log q + log N_k)
        procedure :: sum => sampler_sum !! Returns total sum of weights across all bins in O(1)
        procedure :: sampler_pos => sampler_sampler_pos !! Helper calculating target bin index for a given weight

        final :: sampler_finalize !! Finalizer releasing sub-samplers and trees
    end type weighted_sampler_t

    public :: weighted_sampler, weighted_sampler_t

contains

    !> Create a new sampler with N weights
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

        if (n <= 0) error stop "sampler_init: n must be positive"
        if (allocated(this%sampler_of_index)) call sampler_cleanup(this)

        this%n = n ! Set the number of weights

        ! If q was not set via init_w2, provide a robust default range (e.g. 1e-6 to 1e+6)
        if (this%q == 0) then
            this%wmin = 1.0e-6_dp
            this%wmax = 1.0e+6_dp
            this%q = max(1, int(ceiling(log(this%wmax / this%wmin) / log(2.0_dp))))
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
        integer(i4) :: old_sampler_pos

        ! if the weight is zero, remove the index from the list
        if (weight <= EPSILON) then
            call this%remove(index)
            return
        end if

        ! get the sampler pos
        old_sampler_pos = this%sampler_of_index(index)

        ! if the weight is the same and is already in the list, just return
        if ((old_sampler_pos /= 0) .and. (weight == this%weights(index))) return

        sampler_pos = this%sampler_pos(weight) ! Get the sampler position based on the weight

        call this%samplers(sampler_pos)%set_weight(index, weight) ! Set the weight in the first sampler
        this%sampler_of_index(index) = sampler_pos ! Map index to the corresponding sampler

        ! remove from the old sampler if it was assigned
        if ((old_sampler_pos /= 0) .and. (old_sampler_pos /= sampler_pos)) then
            call this%samplers(old_sampler_pos)%remove(index)
        end if

        ! set weight of the btree
        call this%btree%set_weight(sampler_pos, this%samplers(sampler_pos)%sum())

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

        ! update the btree
        call this%btree%set_weight(sampler_pos, this%samplers(sampler_pos)%sum())

    end subroutine sampler_remove

    !> Samples an index from the sampler
    !> Input: gen - random number generator (rndgen-fortran module)
    !> Output: index - sampled index
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
    !> Output: total - sum of all weights
    function sampler_sum(this) result(total_weight)
        class(weighted_sampler_t), intent(in) :: this
        real(dp) :: total_weight

        total_weight = this%btree%sum()

    end function sampler_sum

    subroutine sampler_cleanup(this)
        class(weighted_sampler_t), intent(inout) :: this

        if (allocated(this%sampler_of_index)) deallocate(this%sampler_of_index)
        if (allocated(this%weights)) deallocate(this%weights)
        if (allocated(this%samplers)) deallocate(this%samplers)
        this%n = 0
        this%q = 0
    end subroutine sampler_cleanup

    !> Finalize the sampler, deallocate resources
    subroutine sampler_finalize(this)
        type(weighted_sampler_t), intent(inout) :: this
        call sampler_cleanup(this)
    end subroutine sampler_finalize

    !> Auxiliary function (private) to get the sampler position for a given weight
    !> Input: weight - the weight to find the sampler position for
    function sampler_sampler_pos(this, weight) result(pos)
        class(weighted_sampler_t), intent(in) :: this
        real(dp), intent(in) :: weight
        integer(i4) :: pos

        if (weight <= this%wmin) then
            pos = 1
        else
            ! Partition into power-of-2 bins: [2^(k-1)*wmin, 2^k*wmin)
            pos = max(1, min(this%q, int(floor(log(weight / this%wmin) / log(2.0_dp))) + 1))
        end if

    end function sampler_sampler_pos

end module datastructs_samplers_rejection_maxheap_composition_mod
