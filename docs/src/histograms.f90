!> Module for weighted histograms with statistical operations.
!>
!> Provides a data structure to accumulate weighted values in bins and compute
!> statistical properties such as mean, variance, and higher-order moments.
!>
!> Features:
!> - Initialize and reset histograms with user-defined bin ranges
!> - Add weighted values to bins
!> - Compute mean, variance, and moments of any order
module datastructs_histograms_mod
    use datastructs_kinds_mod
    implicit none
    private

    !> Weighted histogram type
    type :: weighted_histogram_t
        real(kind=dp), allocatable :: bins(:) !! Bin weights
        integer(kind=i4) :: min_index !! Lower bound of bins
        integer(kind=i4) :: max_index !! Upper bound of bins
        real(kind=dp) :: total_weight !! Sum of all weights

    contains
        procedure :: init         => weighted_histogram_init
        procedure :: reset        => weighted_histogram_reset
        procedure :: add_value    => weighted_histogram_add_value
        procedure :: get_mean     => weighted_histogram_get_mean
        procedure :: get_variance => weighted_histogram_get_variance
        procedure :: get_moment   => weighted_histogram_get_moment
    end type weighted_histogram_t

    public :: weighted_histogram_t

contains

    !> Initialize the histogram with a given range of bins.
    subroutine weighted_histogram_init(this, min_index, max_index)
        class(weighted_histogram_t), intent(inout) :: this !! Histogram object
        integer, intent(in) :: min_index !! Lower bound index
        integer, intent(in) :: max_index !! Upper bound index

        if (allocated(this%bins)) deallocate(this%bins)
        allocate(this%bins(min_index:max_index))

        this%min_index = min_index
        this%max_index = max_index

        call this%reset()
    end subroutine weighted_histogram_init

    !> Reset all bin weights and total weight to zero.
    subroutine weighted_histogram_reset(this)
        class(weighted_histogram_t), intent(inout) :: this !! Histogram object

        this%bins = 0.0_dp
        this%total_weight = 0.0_dp
    end subroutine weighted_histogram_reset

    !> Add a weighted value to a given bin index.
    subroutine weighted_histogram_add_value(this, index, weight)
        class(weighted_histogram_t), intent(inout) :: this !! Histogram object
        integer(kind=i4), intent(in) :: index !! Bin index
        real(kind=dp), intent(in) :: weight !! Weight to add

        this%bins(index) = this%bins(index) + weight
        this%total_weight = this%total_weight + weight
    end subroutine weighted_histogram_add_value

    !> Compute the k-th statistical moment of the histogram.
    !> The moment is normalized by the total weight.
    function weighted_histogram_get_moment(this, k, normalization_factor) result(res)
        class(weighted_histogram_t), intent(in) :: this !! Histogram object
        integer(kind=i4), intent(in) :: k !! Moment order (k ≥ 0)
        real(kind=dp), intent(in), optional :: normalization_factor !! Normalization factor for index scaling

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

        if (this%total_weight <= 0.0_dp) error stop "Error: Histogram is empty."

        res = 0.0_dp
        do i = this%min_index, this%max_index
            res = res + ((real(i,dp) / norm_factor)**k) * (this%bins(i) / this%total_weight)
        end do
    end function weighted_histogram_get_moment

    !> Compute the mean value of the histogram.
    function weighted_histogram_get_mean(this, normalization_factor) result(res)
        class(weighted_histogram_t), intent(in) :: this !! Histogram object
        real(kind=dp), intent(in), optional :: normalization_factor !! Normalization factor for index scaling

        real(kind=dp) :: res
        res = weighted_histogram_get_moment(this, 1, normalization_factor)
    end function weighted_histogram_get_mean

    !> Compute the variance of the histogram.
    function weighted_histogram_get_variance(this, normalization_factor) result(res)
        class(weighted_histogram_t), intent(in) :: this !! Histogram object
        real(kind=dp), intent(in), optional :: normalization_factor !! Normalization factor for index scaling

        real(kind=dp) :: mean, mean_2, res

        mean = weighted_histogram_get_moment(this, 1, normalization_factor)
        mean_2 = weighted_histogram_get_moment(this, 2, normalization_factor)
        res = mean_2 - mean**2
    end function weighted_histogram_get_variance

end module datastructs_histograms_mod
