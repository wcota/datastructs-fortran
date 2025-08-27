!> Logging module for managing verbosity levels and output destinations.
!> This module provides a simple logging system with support for different log
!> levels (ERROR, WARNING, INFO, DEBUG), configurable verbosity, and output units.
!>
!> Features:
!> - Enable or disable global verbosity
!> - Control the log level threshold (e.g., show only ERROR, or include DEBUG)
!> - Redirect logs to custom output units or reset to defaults (stdout/stderr)
!> - Helper function to determine whether to log and which unit to use
!> - Convenience subroutine to write logs directly
!>
!> @note
!> This module is designed for modular use and can be applied to any program.
!> @endnote
module datastructs_logger_mod
    use datastructs_kinds_mod
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    private

    ! Log level constants
    integer, parameter :: LOG_ERROR   = 0   !! Error messages
    integer, parameter :: LOG_WARNING = 1   !! Warning messages
    integer, parameter :: LOG_INFO    = 2   !! Informational messages
    integer, parameter :: LOG_DEBUG   = 3   !! Debug messages

    character(len=*), parameter :: names(0:3) = ['[error]  ', '[warning]', '[info]   ', '[debug]  ']

    !> Global verbosity flag (if .false., no log messages are printed)
    logical :: LOGGER_VERBOSE = .false.
    !> Current log level threshold (default: LOG_INFO)
    integer(kind=i4) :: LOGGER_LEVEL = LOG_INFO
    !> Output unit for normal logs (default: standard output)
    integer(kind=i4) :: LOGGER_OUTPUT_UNIT = output_unit
    !> Output unit for error logs (default: standard error)
    integer(kind=i4) :: LOGGER_ERROR_UNIT = error_unit
    !> Indicates if the last call to log_unit allowed logging
    logical :: LOGGER_OK = .true.

    interface log_write
        module procedure log_write_message
        module procedure log_write_message_no_advance
        module procedure log_write_message_i4
        module procedure log_write_message_i4_no_advance
        module procedure log_write_message_dp
        module procedure log_write_message_dp_no_advance
    end interface log_write

    public :: log_unit, set_verbose, set_level, set_output_unit, set_error_unit, set_unit_defaults, log_write
    public :: LOG_ERROR, LOG_WARNING, LOG_INFO, LOG_DEBUG, LOGGER_OK

contains
    !> Enable or disable global verbosity.
    !> If set to `.false.`, no log messages will be printed regardless of level.
    subroutine set_verbose(flag)
        logical, intent(in) :: flag !! Logical flag to enable (`.true.`) or disable (`.false.`) verbosity.
        LOGGER_VERBOSE = flag
    end subroutine set_verbose

    !> Set the current log level threshold.
    !! Only messages with level <= LOGGER_LEVEL will be printed.
    subroutine set_level(level)
        integer(kind=i4), intent(in) :: level !! Log level (use LOG_ERROR, LOG_WARNING, LOG_INFO, LOG_DEBUG)
        LOGGER_LEVEL = level
    end subroutine set_level

    !> Set the output unit for normal log messages.
    subroutine set_output_unit(unit)
        integer(kind=i4), intent(in) :: unit !! Fortran unit number for INFO/WARNING/DEBUG logs.
        LOGGER_OUTPUT_UNIT = unit
    end subroutine set_output_unit

    !> Set the output unit for error log messages.
    subroutine set_error_unit(unit)
        integer(kind=i4), intent(in) :: unit !! Fortran unit number for ERROR logs.
        LOGGER_ERROR_UNIT = unit
    end subroutine set_error_unit

    !> Reset output units to default (stdout for normal logs, stderr for errors).
    subroutine set_unit_defaults()
        LOGGER_OUTPUT_UNIT = output_unit
        LOGGER_ERROR_UNIT = error_unit
    end subroutine set_unit_defaults

    !> Determine the output unit for a given log level and update LOGGER_OK.
    !>
    !> This function returns the Fortran unit where the message should be written,
    !> and sets LOGGER_OK to indicate if the log should be written.
    function log_unit(level) result(unit)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        integer(kind=i4) :: unit !! The Fortran output unit (or -1 if logging is disabled)

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

    !> Write a log message for the given level using the current logger settings.
    !>
    !> This is a convenience routine that calls log_unit() internally and writes
    !> the message if LOGGER_OK is `.true.`.
    subroutine log_write_message(level, message)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        character(len=*), intent(in) :: message !! The text message to log
        integer(kind=i4) :: unit

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general) names(level), message
    end subroutine log_write_message

    !> Write a log message for a given level without advancing the line
    subroutine log_write_message_no_advance(level, message, newline)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        character(len=*), intent(in) :: message !! The text message to log
        logical, intent(in) :: newline !! If `.false.`, do not append a newline
        character(len=:), allocatable :: advance
        integer(kind=i4) :: unit

        advance = "no"
        if (newline) advance = "yes"

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general, advance=advance) names(level), message
    end subroutine log_write_message_no_advance

    !> Write a log message for a given level, appending an integer at the end
    subroutine log_write_message_i4(level, message, value)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        character(len=*), intent(in) :: message !! The text message to log
        integer(kind=i4), intent(in) :: value !! Integer value to append
        integer(kind=i4) :: unit

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general) names(level), message, value
    end subroutine log_write_message_i4

    !> Write a log message for a given level without advancing the line, appending an integer at the end
    subroutine log_write_message_i4_no_advance(level, message, value, newline)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        character(len=*), intent(in) :: message !! The text message to log
        integer(kind=i4), intent(in) :: value !! Integer value to append
        logical, intent(in) :: newline !! If `.false.`, do not append a newline
        integer(kind=i4) :: unit
        character(len=:), allocatable :: advance

        advance = "no"
        if (newline) advance = "yes"

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general, advance=advance) names(level), message, value
    end subroutine log_write_message_i4_no_advance

    !> Write a log message for a given level, appending a double at the end
    subroutine log_write_message_dp(level, message, value)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        character(len=*), intent(in) :: message !! The text message to log
        real(kind=dp), intent(in) :: value !! Double precision value to append
        integer(kind=i4) :: unit

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general) names(level), message, value
    end subroutine log_write_message_dp

    !> Write a log message for a given level without advancing the line, appending a double at the end
    subroutine log_write_message_dp_no_advance(level, message, value, newline)
        integer(kind=i4), intent(in) :: level !! Log level of the message
        character(len=*), intent(in) :: message !! The text message to log
        real(kind=dp), intent(in) :: value !! Double precision value to append
        logical, intent(in) :: newline !! If `.false.`, do not append a newline
        integer(kind=i4) :: unit
        character(len=:), allocatable :: advance

        advance = "no"
        if (newline) advance = "yes"

        unit = log_unit(level)
        if (LOGGER_OK) write(unit, fmt_general, advance=advance) names(level), message, value
    end subroutine log_write_message_dp_no_advance

end module datastructs_logger_mod
