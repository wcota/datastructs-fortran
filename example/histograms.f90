program test_weighted_histogram
    use datastructs_kinds_mod
    use datastructs_histograms_mod
    implicit none

    type(weighted_histogram_t) :: hist
    integer(kind=i4) :: i

    ! Initializes histogram from 0 to 10
    call hist%init(0, 10)

    write(*, fmt_general) "Histogram from", hist%min_index, "to", hist%max_index
    write(*, fmt_general) "Initial weights:"
    write(*, fmt_general) hist%bins

    ! Adds some values with different weights
    call hist%add_value(0, 1.5_dp)
    call hist%add_value(1, 2.0_dp)
    call hist%add_value(3, 4.0_dp)
    call hist%add_value(10, 2.5_dp)
    call hist%add_value(3, 1.0_dp)

    write(*, fmt_general) "After adding values:"
    do i = hist%min_index, hist%max_index
        if (hist%bins(i) /= 0.0_dp) write(*, fmt_general) "Index", i, "-> Weight =", hist%bins(i)
    end do
    write(*, fmt_general) "Total weight =", hist%total_weight

    ! Calculates statistics
    write(*, fmt_general) "Mean =", hist%get_mean()
    write(*, fmt_general) "Mean (normalized by 10) =", hist%get_mean(10.0_dp)
    write(*, fmt_general) "Variance =", hist%get_variance()
    write(*, fmt_general) "Standard deviation =", sqrt(hist%get_variance())
    write(*, fmt_general) "Variance (normalized by 10) =", hist%get_variance(10.0_dp)
    write(*, fmt_general) "Standard deviation (normalized by 10) =", sqrt(hist%get_variance(10.0_dp))
    write(*, fmt_general) "Moment of order 2 =", hist%get_moment(2)
    write(*, fmt_general) "Moment of order 2 (normalized by 10) =", hist%get_moment(2, 10.0_dp)
    write(*, fmt_general) "Moment of order 3 =", hist%get_moment(3)
    write(*, fmt_general) "Moment of order 3 (normalized by 10) =", hist%get_moment(3, 10.0_dp)

end program test_weighted_histogram
