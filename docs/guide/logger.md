# Logging & Diagnostics

`datastructs-fortran` includes an integrated, thread-safe (serial execution) logging module (`datastructs_logger_mod`) designed for scientific applications, parameter sweeps, and command-line interfaces.

---

## 1. Log Levels & Constants

The logging subsystem defines four hierarchical severity levels:

| Level Constant | Integer Value | Label Prefix | Purpose |
| :--- | :---: | :---: | :--- |
| `LOG_ERROR` | `0` | `[error]  ` | Critical failures, invalid simulation states, or file I/O errors |
| `LOG_WARNING` | `1` | `[warning]` | Non-fatal anomalies, parameter fallbacks, or convergence warnings |
| `LOG_INFO` | `2` | `[info]   ` | High-level progress, step counts, simulation milestones (Default) |
| `LOG_DEBUG` | `3` | `[debug]  ` | Low-level debugging details, array contents, inner-loop timings |

Messages are only emitted if their level is $\le$ the active threshold (`LOGGER_LEVEL`).

---

## 2. Idiomatic Formatting via `log_unit`

In Fortran, direct string interpolation with multiple data types (integers, floating point numbers, and strings) is most naturally done using standard Fortran `write(unit, fmt) ...`.

The function `log_unit(level)` returns the destination unit and sets `LOGGER_OK` to `.true.` only if the message should be printed:

```fortran
use datastructs_mod, only: log_unit, LOGGER_OK, LOG_INFO, LOG_DEBUG, dp, i4
implicit none

integer(kind=i4) :: u, step
real(kind=dp) :: elapsed_time

step = 1000
elapsed_time = 4.25_dp

! Idiomatic log block:
u = log_unit(LOG_INFO)
if (LOGGER_OK) write(u, '(A, I0, A, F6.2, A)') "[info] Step ", step, " reached in ", elapsed_time, " seconds"
```

If verbosity is disabled or if the current log level is `LOG_WARNING`, `LOGGER_OK` is `.false.` and the formatted `write` statement is completely bypassed with near-zero overhead.

---

## 3. Direct Logging via `log_write`

For simple diagnostic messages, `log_write` provides convenient overloaded interfaces for text, 32-bit integers, and double-precision floats:

```fortran
use datastructs_mod, only: log_write, LOG_INFO, LOG_WARNING, LOG_ERROR, dp
implicit none

call log_write(LOG_INFO, "Initializing simulation grid...")
call log_write(LOG_WARNING, "Tolerance relaxed to: ", 1.0e-5_dp)
call log_write(LOG_ERROR, "Invalid boundary condition encountered!")
```

---

## 4. Configuring Verbosity & Redirection

### Controlling Severity Level
You can raise or lower the verbosity threshold programmatically:

```fortran
use datastructs_mod, only: set_level, LOG_DEBUG, LOG_WARNING

! Include debug messages
call set_level(LOG_DEBUG)

! Quiet mode: only warnings and errors
call set_level(LOG_WARNING)
```

### Suppressing All Logs
Useful during automated test suites, parameter sweeps, or batch execution:

```fortran
use datastructs_mod, only: set_verbose

call set_verbose(.false.) ! Completely disables all log output
```

### Redirecting Output to Custom Units / Files

By default, normal logs (`INFO`, `WARNING`, `DEBUG`) write to standard output (`output_unit`), while errors (`ERROR`) write to standard error (`error_unit`). You can redirect them to custom disk files or MPI rank-specific logs:

```fortran
use datastructs_mod, only: set_output_unit, set_error_unit, set_unit_defaults
implicit none

integer :: log_file_unit

open(newunit=log_file_unit, file="simulation.log", status="replace", action="write")

! Redirect all normal logs to simulation.log
call set_output_unit(log_file_unit)

! ... run simulation ...

! Reset back to standard stdout/stderr
call set_unit_defaults()
close(log_file_unit)
```
