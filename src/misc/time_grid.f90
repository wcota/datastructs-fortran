!> This module implements a fast time grid for stochastic dynamics (e.g. Gillespie/CTMC).
!> It precalculates observation time ticks and records piecewise-constant states
!> in O(1) amortized time per event, with zero heap allocations in the event loop.
module datastructs_time_grid_mod
    use datastructs_kinds_mod
    use datastructs_measures_mod, only: statistical_measure_t
    implicit none
    private

    real(kind=dp), parameter :: EPS_TIME = 1.0e-13_dp

    !> Derived type representing a precomputed observation time grid
    type :: time_grid_t
        real(kind=dp), allocatable :: times(:)    ! Array of precalculated time ticks
        integer(kind=i4)           :: n_points = 0 ! Total number of observation points
        integer(kind=i4)           :: current_tick = 1 ! Next tick to be evaluated (1..n_points+1)
    contains
        procedure :: init_uniform    => time_grid_init_uniform
        procedure :: init_geometric  => time_grid_init_geometric
        procedure :: init_log        => time_grid_init_log
        procedure :: init_custom     => time_grid_init_custom

        procedure :: reset           => time_grid_reset
        procedure :: size            => time_grid_size
        procedure :: time_at         => time_grid_time_at
        procedure :: get_times       => time_grid_get_times
        procedure :: next_time       => time_grid_next_time
        procedure :: has_next        => time_grid_has_next
        procedure :: needs_update    => time_grid_needs_update

        procedure :: collect_ticks   => time_grid_collect_ticks

        procedure, private :: time_grid_record_single
        procedure, private :: time_grid_record_two
        generic :: record => time_grid_record_single, time_grid_record_two

        final :: time_grid_finalize
    end type time_grid_t

    public :: time_grid_t

contains

    !> Initializes an evenly spaced time grid: t = tmin + (i-1)*dt for t <= tmax
    subroutine time_grid_init_uniform(this, tmin, tmax, dt)
        class(time_grid_t), intent(inout) :: this
        real(kind=dp), intent(in) :: tmin, tmax, dt
        integer(kind=i4) :: n, i

        if (dt <= 0.0_dp) error stop "time_grid_init_uniform: dt must be positive"
        if (tmax < tmin) error stop "time_grid_init_uniform: tmax must be >= tmin"

        call time_grid_cleanup(this)

        n = int(floor((tmax - tmin + EPS_TIME * dt) / dt)) + 1
        this%n_points = max(1, n)
        allocate(this%times(this%n_points))

        do i = 1, this%n_points
            this%times(i) = tmin + real(i - 1, kind=dp) * dt
        end do
        ! Guarantee upper bound consistency
        if (this%times(this%n_points) > tmax) this%times(this%n_points) = tmax

        this%current_tick = 1
    end subroutine time_grid_init_uniform

    !> Initializes a geometrically spaced time grid: t_i = tmin * ratio^(i-1)
    !> Suitable for power-law / logarithmic relaxations in stochastic models
    subroutine time_grid_init_geometric(this, tmin, tmax, ratio)
        class(time_grid_t), intent(inout) :: this
        real(kind=dp), intent(in) :: tmin, tmax, ratio
        integer(kind=i4) :: n, i
        real(kind=dp) :: curr_t

        if (tmin <= 0.0_dp) error stop "time_grid_init_geometric: tmin must be > 0"
        if (ratio <= 1.0_dp) error stop "time_grid_init_geometric: ratio must be > 1.0"
        if (tmax < tmin) error stop "time_grid_init_geometric: tmax must be >= tmin"

        call time_grid_cleanup(this)

        n = int(floor(log((tmax + EPS_TIME * tmin) / tmin) / log(ratio))) + 1
        this%n_points = max(1, n)
        allocate(this%times(this%n_points))

        curr_t = tmin
        do i = 1, this%n_points
            this%times(i) = curr_t
            curr_t = curr_t * ratio
        end do
        if (this%times(this%n_points) > tmax) this%times(this%n_points) = tmax

        this%current_tick = 1
    end subroutine time_grid_init_geometric

    !> Initializes a logarithmically spaced grid with a specified number of points
    subroutine time_grid_init_log(this, tmin, tmax, n_points)
        class(time_grid_t), intent(inout) :: this
        real(kind=dp), intent(in) :: tmin, tmax
        integer(kind=i4), intent(in) :: n_points
        integer(kind=i4) :: i
        real(kind=dp) :: log_min, log_max, dlog

        if (tmin <= 0.0_dp) error stop "time_grid_init_log: tmin must be > 0"
        if (tmax <= tmin) error stop "time_grid_init_log: tmax must be > tmin"
        if (n_points < 2) error stop "time_grid_init_log: n_points must be >= 2"

        call time_grid_cleanup(this)

        this%n_points = n_points
        allocate(this%times(this%n_points))

        log_min = log(tmin)
        log_max = log(tmax)
        dlog = (log_max - log_min) / real(n_points - 1, kind=dp)

        do i = 1, n_points - 1
            this%times(i) = exp(log_min + real(i - 1, kind=dp) * dlog)
        end do
        this%times(n_points) = tmax

        this%current_tick = 1
    end subroutine time_grid_init_log

    !> Initializes a custom user-supplied time grid
    subroutine time_grid_init_custom(this, times)
        class(time_grid_t), intent(inout) :: this
        real(kind=dp), intent(in) :: times(:)

        if (size(times) == 0) error stop "time_grid_init_custom: times array cannot be empty"

        call time_grid_cleanup(this)

        this%n_points = size(times)
        allocate(this%times(this%n_points))
        this%times = times
        this%current_tick = 1
    end subroutine time_grid_init_custom

    !> Resets the tick index to 1 for a new trajectory/realization
    subroutine time_grid_reset(this)
        class(time_grid_t), intent(inout) :: this
        this%current_tick = 1
    end subroutine time_grid_reset

    !> Returns the number of grid points
    pure function time_grid_size(this) result(res)
        class(time_grid_t), intent(in) :: this
        integer(kind=i4) :: res
        res = this%n_points
    end function time_grid_size

    !> Returns the time at a given tick index
    pure function time_grid_time_at(this, index) result(res)
        class(time_grid_t), intent(in) :: this
        integer(kind=i4), intent(in) :: index
        real(kind=dp) :: res

        if (index < 1 .or. index > this%n_points) then
            res = -1.0_dp
        else
            res = this%times(index)
        end if
    end function time_grid_time_at

    !> Returns a copy of the entire times array
    function time_grid_get_times(this) result(res)
        class(time_grid_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)

        if (allocated(this%times)) then
            res = this%times
        else
            allocate(res(0))
        end if
    end function time_grid_get_times

    !> Returns the timestamp of the next tick to record (or huge(dp) if done)
    pure function time_grid_next_time(this) result(res)
        class(time_grid_t), intent(in) :: this
        real(kind=dp) :: res

        if (this%current_tick <= this%n_points) then
            res = this%times(this%current_tick)
        else
            res = huge(1.0_dp)
        end if
    end function time_grid_next_time

    !> Returns true if there are remaining ticks to record
    pure function time_grid_has_next(this) result(res)
        class(time_grid_t), intent(in) :: this
        logical :: res
        res = (this%current_tick <= this%n_points)
    end function time_grid_has_next

    !> Fast O(1) check: true if current_time has reached or surpassed the next tick
    pure function time_grid_needs_update(this, current_time) result(res)
        class(time_grid_t), intent(in) :: this
        real(kind=dp), intent(in) :: current_time
        logical :: res

        res = (this%current_tick <= this%n_points .and. current_time >= this%times(this%current_tick))
    end function time_grid_needs_update

    !> Advances ticks up to current_time, returning the range [start_tick, end_tick]
    !> If no tick is due, returns start_tick > end_tick (e.g. start_tick, start_tick - 1).
    !> Zero heap allocations, O(1) amortized.
    subroutine time_grid_collect_ticks(this, current_time, start_tick, end_tick)
        class(time_grid_t), intent(inout) :: this
        real(kind=dp), intent(in) :: current_time
        integer(kind=i4), intent(out) :: start_tick, end_tick

        start_tick = this%current_tick
        do while (this%current_tick <= this%n_points)
            if (current_time < this%times(this%current_tick)) exit
            this%current_tick = this%current_tick + 1
        end do
        end_tick = this%current_tick - 1
    end subroutine time_grid_collect_ticks

    !> Records a single scalar state variable into a statistical_measure_t
    !> for all ticks <= current_time that haven't been recorded yet.
    subroutine time_grid_record_single(this, measure, current_time, state_value)
        class(time_grid_t), intent(inout) :: this
        type(statistical_measure_t), intent(inout) :: measure
        real(kind=dp), intent(in) :: current_time, state_value

        do while (this%current_tick <= this%n_points)
            if (current_time < this%times(this%current_tick)) exit
            call measure%add_point(this%current_tick, state_value)
            this%current_tick = this%current_tick + 1
        end do
    end subroutine time_grid_record_single

    !> Records two scalar state variables simultaneously into their respective measures
    subroutine time_grid_record_two(this, measure1, val1, measure2, val2, current_time)
        class(time_grid_t), intent(inout) :: this
        type(statistical_measure_t), intent(inout) :: measure1, measure2
        real(kind=dp), intent(in) :: val1, val2, current_time

        do while (this%current_tick <= this%n_points)
            if (current_time < this%times(this%current_tick)) exit
            call measure1%add_point(this%current_tick, val1)
            call measure2%add_point(this%current_tick, val2)
            this%current_tick = this%current_tick + 1
        end do
    end subroutine time_grid_record_two

    !> Internal cleanup
    subroutine time_grid_cleanup(this)
        type(time_grid_t), intent(inout) :: this
        if (allocated(this%times)) deallocate(this%times)
        this%n_points = 0
        this%current_tick = 1
    end subroutine time_grid_cleanup

    !> Finalizer
    subroutine time_grid_finalize(this)
        type(time_grid_t), intent(inout) :: this
        call time_grid_cleanup(this)
    end subroutine time_grid_finalize

end module datastructs_time_grid_mod
