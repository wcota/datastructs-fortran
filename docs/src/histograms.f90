module datastructs_histograms_mod
    use datastructs_kinds_mod
    implicit none
    private

    type :: weighted_histogram_t
        real(kind=dp), allocatable :: bins(:)
        integer(kind=i4) :: min_index, max_index
        real(kind=dp) :: total_weight

    contains

        procedure :: init => weighted_histogram_init
        procedure :: reset => weighted_histogram_reset
        procedure :: add_value => weighted_histogram_add_value

        procedure :: get_mean => weighted_histogram_get_mean
        procedure :: get_variance => weighted_histogram_get_variance
        procedure :: get_moment => weighted_histogram_get_moment

    end type weighted_histogram_t

    public :: weighted_histogram_t

contains

    subroutine weighted_histogram_init(this, min_index, max_index)
        class(weighted_histogram_t), intent(inout) :: this
        integer, intent(in) :: min_index, max_index

        if (allocated(this%bins)) deallocate(this%bins)

        allocate(this%bins(min_index:max_index))

        this%min_index = min_index
        this%max_index = max_index
        
        call this%reset()
    end subroutine weighted_histogram_init

    subroutine weighted_histogram_reset(this)
        class(weighted_histogram_t), intent(inout) :: this

        this%bins = 0.0_dp
        this%total_weight = 0.0_dp
    end subroutine weighted_histogram_reset

    subroutine weighted_histogram_add_value(this, index, weight)
        class(weighted_histogram_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: index
        real(kind=dp), intent(in) :: weight

        ! DEBUG
        !if (.not. allocated(this%bins)) then
        !    print *, "Error: Histogram not initialized."
        !    return
        !end if

        !if (index < lbound(this%bins) .or. index > ubound(this%bins)) then
        !    print *, "Error: Index out of bounds."
        !    return
        !end if

        this%bins(index) = this%bins(index) + weight
        this%total_weight = this%total_weight + weight

    end subroutine weighted_histogram_add_value

    function weighted_histogram_get_moment(this, k, normalization_factor) result(res)
        ! this function will calculate the n-th moment <n^k>
        class(weighted_histogram_t), intent(in) :: this
        integer(kind=i4), intent(in) :: k
        real(kind=dp), intent(in), optional :: normalization_factor

        real(kind=dp) :: norm_factor
        integer(kind=i4) :: i
        
        real(kind=dp) :: res

        norm_factor = 1.0_dp
        if (present(normalization_factor)) norm_factor = normalization_factor

        if (k < 0) error stop "Error: Invalid moment order."
        if (k == 0) then
            res = 1.0_dp
            return
        end if

        if (this%total_weight <= 0.0_dp) then
            error stop "Error: Histogram is empty."
        end if

        res = 0.0_dp
        do i = this%min_index, this%max_index
            res = res + ((real(i,dp) / norm_factor)**k) * (this%bins(i) / this%total_weight)
        end do
    end function weighted_histogram_get_moment

    function weighted_histogram_get_mean(this, normalization_factor) result(res)
        class(weighted_histogram_t), intent(in) :: this
        real(kind=dp), intent(in), optional :: normalization_factor

        real(kind=dp) :: res

        res = weighted_histogram_get_moment(this, 1, normalization_factor)
    end function weighted_histogram_get_mean

    function weighted_histogram_get_variance(this, normalization_factor) result(res)
        class(weighted_histogram_t), intent(in) :: this
        real(kind=dp), intent(in), optional :: normalization_factor

        real(kind=dp) :: mean, mean_2, res

        mean = weighted_histogram_get_moment(this, 1, normalization_factor)
        mean_2 = weighted_histogram_get_moment(this, 2, normalization_factor)

        res = mean_2 - mean**2
    end function weighted_histogram_get_variance

end module datastructs_histograms_mod
