program example_logger
    use datastructs_mod
    use datastructs_kinds_mod
    implicit none

    integer(kind=i4) :: u
    integer(kind=i4) :: funit_out, funit_err

    ! configurações do logger
    write(*, fmt_general) "# Testing with verbose and LOG_ERROR"
    call set_verbose(.true.)
    call set_level(LOG_ERROR)
    call test()

    write(*, fmt_general) "# Testing with verbose and LOG_WARNING"
    call set_level(LOG_WARNING)
    call test()

    write(*, fmt_general) "# Testing with verbose and LOG_INFO"
    call set_level(LOG_INFO)
    call test()

    write(*, fmt_general) "# Testing with verbose and LOG_DEBUG"
    call set_level(LOG_DEBUG)
    call test()

    write(*, fmt_general) "# Testing without verbose"
    call set_verbose(.false.)
    call test()

    write(*, fmt_general) "# Logging to files /tmp/output.log and /tmp/error.log"
    call set_verbose(.true.)
    open(newunit=funit_out, file="/tmp/output.log", status="replace", action="write")
    open(newunit=funit_err, file="/tmp/error.log", status="replace", action="write")
    call set_output_unit(funit_out)
    call set_error_unit(funit_err)
    call test()
    close(funit_out)
    close(funit_err)

    write(*, fmt_general) "# Testing with output defaults"
    call set_unit_defaults()
    call test()

contains

    subroutine test()

        call log_write(LOG_INFO, "Starting program...")

        ! When using write with multiple arguments, this is way to go
        u = log_unit(LOG_DEBUG)
        if (LOGGER_OK) write(u, fmt_general) "Initial value of N =", 42

        call log_write(LOG_WARNING, "Warning: parameter near the limit!")

        call log_write(LOG_ERROR, "Error: failed to allocate resource.")

        write(*, fmt_general) "------------------------------------------------------------"

    end subroutine test

end program example_logger
