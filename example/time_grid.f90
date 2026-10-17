!> Example showing Gillespie stochastic simulation using time_grid_t and btree sampler.
!> Simulates a simple 1D random walk with rates to left and right.
program example_time_grid
    use datastructs_mod
    use datastructs_kinds_mod
    use rndgen_mod
    implicit none

    type(time_grid_t) :: grid
    type(statistical_measure_t) :: spatial_measure
    class(sampler_base_t), allocatable :: my_sampler
    type(rndgen) :: gen

    real(kind=dp), parameter :: rate_left  = 10.0_dp
    real(kind=dp), parameter :: rate_right = 10.0_dp
    real(kind=dp), parameter :: total_rate = rate_left + rate_right

    real(kind=dp), parameter :: tmin = 1.0_dp
    real(kind=dp), parameter :: tmax = 50.0_dp
    real(kind=dp), parameter :: ratio = 1.05_dp

    real(kind=dp) :: t, x
    integer(kind=i4), parameter :: n_trajectories = 5000
    integer(kind=i4) :: traj, k

    ! 1. Choose and configure the sampler
    call choose_sampler(my_sampler, 'btree')
    call my_sampler%init(2)
    call my_sampler%set_weight(1, rate_left)
    call my_sampler%set_weight(2, rate_right)

    ! 2. Initialize the observation time grid (geometric/powerlaw spacing)
    call grid%init_geometric(tmin=tmin, tmax=tmax, ratio=ratio)
    write(*, fmt_general) "Initialized time_grid with ", grid%size(), " observation points."

    ! 3. Initialize statistical measure with the exact number of grid ticks
    call spatial_measure%init(grid%size())

    call gen%init(12345)

    ! 4. Run trajectories
    do traj = 1, n_trajectories
        t = 0.0_dp
        x = 0.0_dp
        call grid%reset()

        do while (t < tmax .and. grid%has_next())
            ! Advance Gillespie time
            t = t + (-log(1.0_dp - gen%rnd()) / total_rate)

            ! Record state at any ticks that occurred before or at current t
            call grid%record(spatial_measure, t, x)

            ! Perform stochastic jump
            select case (my_sampler%sample(gen))
            case (1)
                x = x - 1.0_dp
            case (2)
                x = x + 1.0_dp
            end select
        end do

        ! If the trajectory ended before all ticks up to tmax, record the final state
        call grid%record(spatial_measure, tmax, x)
    end do

    ! 5. Print a summary of the results at selected ticks
    write(*, fmt_general) "Trajectory sampling complete. First 5 observation points:"
    write(*, '(A10, A15, A15, A12)') "Tick", "Time", "Mean Position", "Variance"
    do k = 1, min(5, grid%size())
        write(*, '(I10, F15.4, F15.4, F12.4)') k, grid%time_at(k), &
            spatial_measure%get_mean(k), spatial_measure%get_variance(k)
    end do

end program example_time_grid
