module datastructs_logger_mod
    use datastructs_kinds_mod
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    private

    integer, parameter :: LOG_ERROR   = 0
    integer, parameter :: LOG_WARNING = 1
    integer, parameter :: LOG_INFO    = 2
    integer, parameter :: LOG_DEBUG   = 3

    logical :: LOGGER_VERBOSE = .false.
    integer(kind=i4) :: LOGGER_LEVEL = LOG_INFO
    integer(kind=i4) :: LOGGER_OUTPUT_UNIT = output_unit
    integer(kind=i4) :: LOGGER_ERROR_UNIT = error_unit
    logical :: LOGGER_OK = .true.

    public :: log_unit, set_verbose, set_level, set_output_unit, set_error_unit, set_output_defaults, log_write
    public :: LOG_ERROR, LOG_WARNING, LOG_INFO, LOG_DEBUG, LOGGER_OK

contains
    subroutine set_verbose(flag)
        logical, intent(in) :: flag
        LOGGER_VERBOSE = flag
    end subroutine set_verbose

    subroutine set_level(level)
        integer(kind=i4), intent(in) :: level
        LOGGER_LEVEL = level
    end subroutine set_level

    subroutine set_output_unit(unit)
        integer(kind=i4), intent(in) :: unit
        LOGGER_OUTPUT_UNIT = unit
    end subroutine set_output_unit

    subroutine set_error_unit(unit)
        integer(kind=i4), intent(in) :: unit
        LOGGER_ERROR_UNIT = unit
    end subroutine set_error_unit

    subroutine set_output_defaults()
        LOGGER_OUTPUT_UNIT = output_unit
        LOGGER_ERROR_UNIT = error_unit
    end subroutine set_output_defaults

    function log_unit(level) result(unit)
        integer(kind=i4), intent(in) :: level
        integer(kind=i4) :: unit

        unit = -1
        LOGGER_OK = .false.

        if (.not. LOGGER_VERBOSE) return

        select case(level)
          case(LOG_ERROR)
            unit = LOGGER_ERROR_UNIT
            LOGGER_OK = .true.
          case(LOG_WARNING)
            if (LOGGER_LEVEL >= LOG_WARNING) then
                unit = LOGGER_OUTPUT_UNIT
                LOGGER_OK = .true.
            end if
          case(LOG_INFO)
            if (LOGGER_LEVEL >= LOG_INFO) then
                unit = LOGGER_OUTPUT_UNIT
                LOGGER_OK = .true.
            end if
          case(LOG_DEBUG)
            if (LOGGER_LEVEL >= LOG_DEBUG) then
                unit = LOGGER_OUTPUT_UNIT
                LOGGER_OK = .true.
            end if
        end select
    end function log_unit

    subroutine log_write(level, message)
        integer(kind=i4), intent(in) :: level
        character(len=*), intent(in) :: message
        integer(kind=i4) :: unit

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general) message
    end subroutine log_write

end module datastructs_logger_mod
