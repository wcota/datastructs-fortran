module datastructs_measures_mod
    use datastructs_kinds_mod
    implicit none
    private

    real(kind=dp), parameter :: EPSILON = 1.0e-12_dp ! epsilon for floating point comparisons

    type :: measure_controller_t
        ! automatically add or not a given position
        real(kind=dp) :: last_value_added = 0.0_dp
        integer(kind=i4) :: last_position_added = 0
        real(kind=dp) :: position_step = 1.0_dp
        real(kind=dp) :: min_value = 0.0_dp ! minimum value for the measure

        procedure(measure_controller_get_pos_array_i), pointer :: get_pos_array => null()
        procedure(measure_controller_get_max_array_size_i), pointer :: get_max_array_size => null()
    contains
        procedure :: init => measure_controller_init
        procedure :: reset => measure_controller_reset
    end type measure_controller_t

    type :: statistical_measure_t
        integer(kind=i4), allocatable :: n_samples(:) ! number of samples for each point
        real(kind=dp), allocatable :: sum_values(:) ! used to store the values of the measure
        real(kind=dp), allocatable :: sum_values_squares(:) ! used to store the squares of the values of the measure
        real(kind=dp), allocatable :: sum_values_thirds(:) ! used to store the cubes of the values of the measure

        integer(kind=i4) :: n_size ! Number of points in the measure

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
    end interface

    public :: statistical_measure_t, measure_controller_t

contains

    function measure_controller_get_pos_array_uniform(controller, value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        integer(kind=i4), allocatable :: res(:)
        integer(kind=i4) :: i, n

        n = int( floor( (value - controller%last_value_added + EPSILON * controller%position_step) / controller%position_step ) )

        if (n > 0) then
            res = [(controller%last_position_added + i, i=1,n)]
            controller%last_value_added = value
            controller%last_position_added = controller%last_position_added + n
        else
            allocate(res(0))
        end if
    end function measure_controller_get_pos_array_uniform

    function measure_controller_get_max_array_size_uniform(controller, max_value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: max_value
        integer(kind=i4) :: res

        res = int( floor( (max_value - controller%min_value + EPSILON * controller%position_step) / controller%position_step ) )

    end function measure_controller_get_max_array_size_uniform

    function measure_controller_get_pos_array_powerlaw(controller, value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: value
        integer(kind=i4), allocatable :: res(:)
        integer(kind=i4) :: i, aux_pos

        aux_pos = 0
        do while (value >= controller%position_step**(controller%last_position_added + aux_pos + 1))
            aux_pos = aux_pos + 1
        end do

        if (aux_pos > 0) then
            res = [(controller%last_position_added + i, i=1,aux_pos)]
            controller%last_value_added = value
            controller%last_position_added = controller%last_position_added + aux_pos
        else
            allocate(res(0))
        end if
    end function measure_controller_get_pos_array_powerlaw

    function measure_controller_get_max_array_size_powerlaw(controller, max_value) result(res)
        class(measure_controller_t), intent(inout) :: controller
        real(kind=dp), intent(in) :: max_value
        integer(kind=i4) :: res, aux_pos

        aux_pos = 0
        do while (max_value >= controller%position_step**(controller%last_position_added + aux_pos + 1))
            aux_pos = aux_pos + 1
        end do

        res = aux_pos

    end function measure_controller_get_max_array_size_powerlaw

    subroutine measure_controller_init(controller, interval_type, step, min_value)
        class(measure_controller_t), intent(inout) :: controller
        character(len=*), intent(in) :: interval_type
        real(kind=dp), intent(in), optional :: step
        real(kind=dp), intent(in), optional :: min_value

        if (present(step)) controller%position_step = step
        if (present(min_value)) controller%min_value = min_value
        call controller%reset()

        select case (trim(adjustl(interval_type)))
          case ("uniform")
            controller%get_pos_array => measure_controller_get_pos_array_uniform
            controller%get_max_array_size => measure_controller_get_max_array_size_uniform
          case ("powerlaw")
            controller%get_pos_array => measure_controller_get_pos_array_powerlaw
            controller%get_max_array_size => measure_controller_get_max_array_size_powerlaw
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
        allocate(this%n_samples(n_size))
        allocate(this%sum_values(n_size))
        allocate(this%sum_values_squares(n_size))
        allocate(this%sum_values_thirds(n_size))
        this%n_samples = 0
        this%sum_values = 0.0_dp
        this%sum_values_squares = 0.0_dp
        this%sum_values_thirds = 0.0_dp

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
    end subroutine statistical_measure_add_point

    function statistical_measure_get_mean_pos(this, pos) result(res)
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        real(kind=dp) :: res

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        if (this%n_samples(pos) == 0) then
            res = 0.0_dp
        else
            res = this%sum_values(pos) / this%n_samples(pos)
        end if
    end function statistical_measure_get_mean_pos

    function statistical_measure_get_variance_pos(this, pos) result(res)
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        real(kind=dp) :: res

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        if (this%n_samples(pos) == 0) then
            res = 0.0_dp
        else
            res = (this%sum_values_squares(pos) / this%n_samples(pos)) - &
                (statistical_measure_get_mean_pos(this, pos) ** 2)
        end if
    end function statistical_measure_get_variance_pos

    function statistical_measure_get_stddev_pos(this, pos) result(res)
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        real(kind=dp) :: res

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        res = sqrt(statistical_measure_get_variance_pos(this, pos))
    end function statistical_measure_get_stddev_pos

    function statistical_measure_get_skewness_pos(this, pos) result(res)
        ! TODO: check if it is correct
        class(statistical_measure_t), intent(in) :: this
        integer(kind=i4), intent(in) :: pos
        real(kind=dp) :: res

        if (pos < 1 .or. pos > this%n_size) then
            error stop "Error: Invalid position"
            return
        end if

        if (this%n_samples(pos) < 2) then
            res = 0.0_dp
        else
            res = (this%sum_values_thirds(pos) / this%n_samples(pos)) - &
                3.0_dp * statistical_measure_get_mean_pos(this, pos) * &
                statistical_measure_get_variance_pos(this, pos) - &
                (statistical_measure_get_mean_pos(this, pos) ** 3)
            res = res / (statistical_measure_get_stddev_pos(this, pos) ** 3)
        end if
    end function statistical_measure_get_skewness_pos

    ! arrays
    function statistical_measure_get_mean_array(this) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_mean_pos(this, i)
        end do
    end function statistical_measure_get_mean_array

    function statistical_measure_get_variance_array(this) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_variance_pos(this, i)
        end do
    end function statistical_measure_get_variance_array

    function statistical_measure_get_stddev_array(this) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_stddev_pos(this, i)
        end do
    end function statistical_measure_get_stddev_array

    function statistical_measure_get_skewness_array(this) result(res)
        class(statistical_measure_t), intent(in) :: this
        real(kind=dp), allocatable :: res(:)
        integer(kind=i4) :: i

        allocate(res(this%n_size))
        res = 0.0_dp

        do i = 1, this%n_size
            res(i) = statistical_measure_get_skewness_pos(this, i)
        end do
    end function statistical_measure_get_skewness_array

end module datastructs_measures_mod
