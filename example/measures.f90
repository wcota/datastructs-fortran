program example_measures
    use datastructs_mod
    use datastructs_kinds_mod
    use rndgen_mod
    implicit none
    class(sampler_base_t), allocatable :: my_sampler
    type(statistical_measure_t) :: temporal_measure
    type(statistical_measure_t) :: spatial_measure
    type(rndgen) :: gen

    real(kind=dp), parameter :: rate_to_left = (0.1_dp)**(-1)
    real(kind=dp), parameter :: rate_to_right = (0.1_dp)**(-1)
    real(kind=dp), parameter :: total_rate = rate_to_left + rate_to_right

    real(kind=dp) :: t, x

    integer(kind=i4), parameter :: n_samples = 10000
    integer(kind=i4) :: i, time_pos, sample_pos
    integer(kind=i4) :: last_position_added
    integer(kind=i4) :: file_unit

    real(kind=dp), parameter :: tmax = 100.0_dp
    real(kind=dp), parameter :: x_step_left_size = 1.0_dp
    real(kind=dp), parameter :: x_step_right_size = 1.0_dp

    call choose_sampler(my_sampler, 'btree')

    call my_sampler%init(2)
    call my_sampler%set_weight(1, rate_to_left)
    call my_sampler%set_weight(2, rate_to_right)

    call gen%init(12345)

    call temporal_measure%init(int(tmax))
    call spatial_measure%init(int(tmax))

    do sample_pos = 1, n_samples
        t = 0.0_dp
        x = 0.0_dp
        last_position_added = 0
        do while (t <= tmax)

            ! measure stuff
            if (t > last_position_added) then
                do time_pos = last_position_added + 1, int(t)
                    call temporal_measure%add_point(time_pos, t)
                    call spatial_measure%add_point(time_pos, x)
                end do
                last_position_added = int(t)
            end if

            t = t + (-log(1.0_dp - gen%rnd())/total_rate)

            select case (my_sampler%sample(gen))
            case (1)
                x = x - x_step_left_size
            case (2)
                x = x + x_step_right_size
            end select

        end do
    end do

    ! write the results
    open(newunit=file_unit, file='/tmp/temporal_measure.txt', status='replace', action='write')

    do i = 1, temporal_measure%n_size
        if (temporal_measure%n_samples(i) > 0) write(file_unit, fmt_general) temporal_measure%get_mean(i), spatial_measure%get_mean(i), &
            spatial_measure%get_variance(i), spatial_measure%get_stddev(i), &
            spatial_measure%get_skewness(i)
    end do

    close(file_unit)

    write(*, fmt_general) 'File was written to /tmp/temporal_measure.txt'

end program example_measures
