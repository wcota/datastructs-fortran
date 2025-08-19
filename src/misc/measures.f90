module datastructs_measures_mod
    use datastructs_kinds_mod
    implicit none
    private

    type :: statistical_measure_t
        character(len=:), allocatable :: name ! Name of the measure (optional)
        
        integer(kind=i4), allocatable :: n_samples(:) ! number of samples for each point
        real(kind=dp), allocatable :: sum_values(:) ! used to store the values of the measure
        real(kind=dp), allocatable :: sum_values_squares(:) ! used to store the squares of the values of the measure
        real(kind=dp), allocatable :: sum_values_thirds(:) ! used to store the cubes of the values of the measure

        integer(kind=i4) :: n_size ! Number of points in the measure

        !integer(kind=i4) :: last_position_added = 0

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

    public :: statistical_measure_t

contains

    subroutine statistical_measure_init(this, n_size, interval_type)
        class(statistical_measure_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n_size
        character(len=*), intent(in), optional :: interval_type

        this%n_size = n_size
        allocate(this%n_samples(n_size))
        allocate(this%sum_values(n_size))
        allocate(this%sum_values_squares(n_size))
        allocate(this%sum_values_thirds(n_size))
        this%n_samples = 0
        this%sum_values = 0.0_dp
        this%sum_values_squares = 0.0_dp
        this%sum_values_thirds = 0.0_dp

        if (present(interval_type)) then
            ! TODO
            return
        end if

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
