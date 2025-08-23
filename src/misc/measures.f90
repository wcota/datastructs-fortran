!> This module implements a time step controller and an object to manage statistical measures
module datastructs_measures_mod
    use datastructs_kinds_mod
    implicit none
    private

    real(kind=dp), parameter :: EPSILON_VALUE = 1.0e-12_dp ! EPSILON_VALUE for floating point comparisons

    !> Measure controller for managing time steps
    !> * The idea is that this controller will keep track of the last value added and the last position added
    !> * The procedures will automatically add or not a given position
    !> * `get_pos_array` will return the array of positions in which the value can be added
    !> * `get_max_array_size` will return the maximum size of the array of positions that can be added
    type :: measure_controller_t
        ! automatically add or not a given position
        real(kind=dp) :: last_value_added = 0.0_dp
        integer(kind=i4) :: last_position_added = 0
        real(kind=dp) :: position_step = 1.0_dp
        real(kind=dp) :: min_value = 0.0_dp ! minimum value for the measure

        procedure(measure_controller_get_pos_array_i), pointer :: get_pos_array => null()
        procedure(measure_controller_get_max_array_size_i), pointer :: get_max_array_size => null()
        procedure(measure_controller_update), pointer :: update => null()
    contains
        procedure :: init => measure_controller_init
        procedure :: reset => measure_controller_reset
    end type measure_controller_t

    !> Object to handle statistical measures
    !> * This object will keep track of the statistical measures for a given set of points
    !> * Procedures are available to access the measures
    type :: statistical_measure_t
        integer(kind=i4), allocatable :: n_samples(:) ! number of samples for each point
        real(kind=dp), allocatable :: sum_values(:) ! used to store the values of the measure
        real(kind=dp), allocatable :: sum_values_squares(:) ! used to store the squares of the values of the measure
        real(kind=dp), allocatable :: sum_values_thirds(:) ! used to store the cubes of the values of the measure

        integer(kind=i4) :: n_size ! Number of points in the measure
        integer(kind=i4) :: max_n_samples = 0 ! Maximum number of samples for any point

    contains

        procedure :: init => statistical_measure_init
        procedure :: add_point => statistical_measure_add_point

        procedure, private :: statistical_measure_get_mean_pos
        procedure, private :: statistical_measure_get_variance_pos
        procedure, private :: statistical_measure_get_stddev_pos
        procedure, private :: statistical_measure_get_skewness_pos

        procedure, private :: statistical_measure_get_mean_array
        procedure, private :: statistical_measure_get_variance_array
        procedure, private :: statistical_measure_get_stddev_array
        procedure, private :: statistical_measure_get_skewness_array

        generic :: get_mean => statistical_measure_get_mean_pos, statistical_measure_get_mean_array
        generic :: get_variance => statistical_measure_get_variance_pos, statistical_measure_get_variance_array
        generic :: get_stddev => statistical_measure_get_stddev_pos, statistical_measure_get_stddev_array
        generic :: get_skewness => statistical_measure_get_skewness_pos, statistical_measure_get_skewness_array

        !procedure :: check_

    end type statistical_measure_t

    interface
        function measure_controller_get_pos_array_i(controller, value) result(res)
            import :: measure_controller_t, dp, i4
            class(measure_controller_t), intent(inout) :: controller
            real(kind=dp), intent(in) :: value
            integer(kind=i4), allocatable :: res(:)
        end function
        function measure_controller_get_max_array_size_i(controller, max_value) result(res)
            import :: measure_controller_t, dp, i4
            class(measure_controller_t), intent(inout) :: controller
            real(kind=dp), intent(in) :: max_value
            integer(kind=i4) :: res
        end function
        subroutine measure_controller_action(time_pos)
            import :: i4
            integer(kind=i4), intent(in) :: time_pos

        end subroutine measure_controller_action
        subroutine measure_controller_update(controller, value, action)
            import :: measure_controller_t, dp, measure_controller_action
            class(measure_controller_t), intent(inout) :: controller
            real(kind=dp), intent(in) :: value
            procedure(measure_controller_action) :: action
        end subroutine measure_controller_update
    end interface

    public :: statistical_measure_t, measure_controller_t

contains

    function measure_controller_get_pos_array_uniform(controller, value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        integer(kind=i4), allocatable :: res(:)
        integer(kind=i4) :: i, n

        n = int( floor( (value - controller%last_value_added + EPSILON_VALUE * controller%position_step) / controller%position_step ) )

        if (n > 0) then
            res = [(controller%last_position_added + i, i=1,n)]
            controller%last_value_added = value
            controller%last_position_added = controller%last_position_added + n
        else
            allocate(res(0))
        end if
    end function measure_controller_get_pos_array_uniform

    subroutine measure_controller_update_uniform(controller, value, action)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        procedure(measure_controller_action) :: action
        integer(kind=i4) :: n, time_pos

        n = int( floor( (value - controller%last_value_added + EPSILON_VALUE * controller%position_step) / controller%position_step ) )

        if (n > 0) then
            do time_pos = 1, n
                call action(controller%last_position_added + time_pos)
            end do
            controller%last_value_added = value
            controller%last_position_added = controller%last_position_added + n
        else
            ! do nothing
        end if
    end subroutine measure_controller_update_uniform

    function measure_controller_get_max_array_size_uniform(controller, max_value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: max_value
        integer(kind=i4) :: res

        res = int( floor( (max_value - controller%min_value + EPSILON_VALUE * controller%position_step) / controller%position_step ) )

    end function measure_controller_get_max_array_size_uniform

    function measure_controller_calc_n_powerlaw(controller, value) result(n)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        integer(kind=i4) :: n
        real(kind=dp) :: tick_last, ratio, eps

        tick_last = controller%position_step**controller%last_position_added
        ratio = controller%position_step
        eps = EPSILON_VALUE * tick_last

        n = int( floor( log( (value + eps) / tick_last ) / log(ratio) ) )
    end function measure_controller_calc_n_powerlaw

    function measure_controller_get_pos_array_powerlaw(controller, value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        integer(kind=i4), allocatable :: res(:)
        integer(kind=i4) :: n, i

        n = measure_controller_calc_n_powerlaw(controller, value)

        if (n > 0) then
            res = [(controller%last_position_added + i, i=1,n)]
            controller%last_value_added = value
            controller%last_position_added = controller%last_position_added + n
        else
            allocate(res(0))
        end if
    end function measure_controller_get_pos_array_powerlaw

    subroutine measure_controller_update_powerlaw(controller, value, action)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        procedure(measure_controller_action) :: action
        integer(kind=i4) :: n, time_pos

        n = measure_controller_calc_n_powerlaw(controller, value)

        if (n > 0) then
            do time_pos = 1, n
                call action(controller%last_position_added + time_pos)
            end do
            controller%last_value_added = value
            controller%last_position_added = controller%last_position_added + n
        else
            ! do nothing
        end if
    end subroutine measure_controller_update_powerlaw

    function measure_controller_get_max_array_size_powerlaw(controller, max_value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: max_value
        integer(kind=i4) :: res
        real(kind=dp) :: tick_last, ratio, eps

        tick_last = max(1.0_dp, controller%min_value)
        ratio = controller%position_step
        eps = EPSILON_VALUE * tick_last

        res = int( floor( log( (max_value + eps) / tick_last ) / log(ratio) ) )
    end function measure_controller_get_max_array_size_powerlaw

    subroutine measure_controller_init(controller, interval_type, step, min_value)
        class(measure_controller_t), intent(inout) :: controller
        character(len=*), intent(in) :: interval_type
        real(kind=dp), intent(in), optional :: step
        real(kind=dp), intent(in), optional :: min_value

        call controller%reset()

        select case (trim(adjustl(interval_type)))
          case ("uniform")
            controller%get_pos_array => measure_controller_get_pos_array_uniform
            controller%get_max_array_size => measure_controller_get_max_array_size_uniform
            controller%update => measure_controller_update_uniform
            ! default values
            controller%position_step = 1.0_dp
            controller%min_value = 0.0_dp
            if (present(step)) controller%position_step = step
            if (present(min_value)) controller%min_value = min_value
          case ("powerlaw")
            controller%get_pos_array => measure_controller_get_pos_array_powerlaw
            controller%get_max_array_size => measure_controller_get_max_array_size_powerlaw
            controller%update => measure_controller_update_powerlaw
            ! default values
            controller%position_step = 1.05_dp
            controller%min_value = 0.0_dp
            if (present(step)) controller%position_step = step
            if (present(min_value)) write(*, fmt_general) 'Warning: min_value not implemented for powerlaw'
          case default
            error stop "Error: Invalid interval type"
        end select

    end subroutine measure_controller_init

    subroutine measure_controller_reset(controller)
        class(measure_controller_t), intent(inout) :: controller

        controller%last_position_added = 0
        controller%last_value_added = controller%min_value
    end subroutine measure_controller_reset

    subroutine statistical_measure_init(this, n_size)
        class(statistical_measure_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n_size

        this%n_size = n_size
        if (allocated(this%n_samples)) deallocate(this%n_samples)
        if (allocated(this%sum_values)) deallocate(this%sum_values)
        if (allocated(this%sum_values_squares)) deallocate(this%sum_values_squares)
        if (allocated(this%sum_values_thirds)) deallocate(this%sum_values_thirds)
        allocate(this%n_samples(n_size))
        allocate(this%sum_values(n_size))
        allocate(this%sum_values_squares(n_size))
        allocate(this%sum_values_thirds(n_size))
        this%n_samples = 0
        this%sum_values = 0.0_dp
        this%sum_values_squares = 0.0_dp
        this%sum_values_thirds = 0.0_dp
        this%max_n_samples = 0

    end subroutine statistical_measure_init

    subroutine statistical_measure_add_point(this, pos, value)
        class(statistical_measure_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: pos
        real(kind=dp), intent(in) :: value

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        this%sum_values(pos) = this%sum_values(pos) + value
        this%sum_values_squares(pos) = this%sum_values_squares(pos) + value**2
        this%sum_values_thirds(pos) = this%sum_values_thirds(pos) + value**3
        this%n_samples(pos) = this%n_samples(pos) + 1

        if (this%n_samples(pos) > this%max_n_samples) then
            this%max_n_samples = this%n_samples(pos)
        end if
    end subroutine statistical_measure_add_point

    function statistical_measure_get_mean_pos(this, pos, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        logical, intent(in), optional :: use_max
        real(kind=dp) :: res
        integer(kind=i4) :: n_samples

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        n_samples = this%n_samples(pos)
        if (present(use_max)) then
            if (use_max) n_samples = this%max_n_samples
        end if

        if (n_samples == 0) then
            res = 0.0_dp
        else
            res = this%sum_values(pos) / n_samples
        end if
    end function statistical_measure_get_mean_pos

    function statistical_measure_get_variance_pos(this, pos, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        logical, intent(in), optional :: use_max
        real(kind=dp) :: res
        integer(kind=i4) :: n_samples

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        n_samples = this%n_samples(pos)
        if (present(use_max)) then
            if (use_max) n_samples = this%max_n_samples
        end if

        if (n_samples == 0) then
            res = 0.0_dp
        else
            res = (this%sum_values_squares(pos) / n_samples) - &
                (statistical_measure_get_mean_pos(this, pos, use_max) ** 2)
        end if
    end function statistical_measure_get_variance_pos

    function statistical_measure_get_stddev_pos(this, pos, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        logical, intent(in), optional :: use_max
        real(kind=dp) :: res

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        res = sqrt(statistical_measure_get_variance_pos(this, pos, use_max))
    end function statistical_measure_get_stddev_pos

    function statistical_measure_get_skewness_pos(this, pos, use_max) result(res)
        ! TODO: check if it is correct
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        logical, intent(in), optional :: use_max
        real(kind=dp) :: res
        integer(kind=i4) :: n_samples

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        n_samples = this%n_samples(pos)
        if (present(use_max)) then
            if (use_max) n_samples = this%max_n_samples
        end if

        if (n_samples < 2) then
            res = 0.0_dp
        else
            res = (this%sum_values_thirds(pos) / n_samples) - &
                3.0_dp * statistical_measure_get_mean_pos(this, pos, use_max) * &
                statistical_measure_get_variance_pos(this, pos, use_max) - &
                (statistical_measure_get_mean_pos(this, pos, use_max) ** 3)
            res = res / (statistical_measure_get_stddev_pos(this, pos, use_max) ** 3)
        end if
    end function statistical_measure_get_skewness_pos

    ! arrays
    function statistical_measure_get_mean_array(this, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        logical, intent(in), optional :: use_max
        integer(kind=i4) :: i

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_mean_pos(this, i, use_max)
        end do
    end function statistical_measure_get_mean_array

    function statistical_measure_get_variance_array(this, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i
        logical, intent(in), optional :: use_max

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_variance_pos(this, i, use_max)
        end do
    end function statistical_measure_get_variance_array

    function statistical_measure_get_stddev_array(this, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i
        logical, intent(in), optional :: use_max

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_stddev_pos(this, i, use_max)
        end do
    end function statistical_measure_get_stddev_array

    function statistical_measure_get_skewness_array(this, use_max) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i
        logical, intent(in), optional :: use_max

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_skewness_pos(this, i, use_max)
        end do
    end function statistical_measure_get_skewness_array

end module datastructs_measures_mod
