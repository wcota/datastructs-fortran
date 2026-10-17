# Fortran Module: datastructs-fortran

## Description

The Fortran library `datastructs-fortran` provides a comprehensive collection of common data structures, weighted random samplers, and statistical observation tools optimized for high-performance stochastic dynamics (e.g. Gillespie algorithm / Continuous-Time Markov Chains):

- **Data Structures**:
  - **Dynamic Lists** (`dynamical_list_t`): Integer lists with $O(1)$ fast swap-with-last removal, trimming, and dynamic expansion.
  - **Fixed Lists** (`fixed_list_t`): Linked fixed-size blocks.
  - **Max Heaps** (`maxheap_t`): Priority queue maintaining maximum value and its associated index with $O(\log N)$ updates and removals.
- **Observation & Statistical Measures**:
  - **Observation Time Grid** (`time_grid_t`): Precalculated uniform, geometric, or logarithmic observation time ticks for continuous-time processes. Records piecewise-constant trajectory states in amortized $O(1)$ time with **zero memory allocations** in the event loop.
  - **Statistical Measures** (`statistical_measure_t`): Online accumulator for 1st, 2nd, and 3rd statistical moments (mean, variance, standard deviation, skewness).
  - **Weighted Histograms** (`weighted_histogram_t`): Frequency bins with arbitrary weights.
- **Weighted Samplers** (`sampler_base_t`, `choose_sampler`):
  - Fast selection of indices proportional to dynamic rates/weights.
  - Available algorithms: `btree` (binary sum tree, $O(\log N)$), `rejection`, `rejection_maxheap`, `rejection_two_classes`, `rejection_maxheap_two_classes`, and `rejection_maxheap_composition` (St-Onge et al. 2019 power-of-2 geometric partition).
- **Sorting**:
  - Native in-place and indexed sorting (`sort`, `sort_index`) for `i4`, `i8`, and `dp` types without external dependencies.
- **Hash Functions**:
  - `djb2`, `fnv1a`, and bijective integer pair packing (`pack_pair`).
- **Logger**:
  - Configurable log levels (`LOG_ERROR`, `LOG_WARNING`, `LOG_INFO`, `LOG_DEBUG`).

The package is lightweight and completely standalone, with only [`rndgen-fortran`](https://github.com/wcota/rndgen-fortran/) as an external dependency (no heavy `stdlib` required).

## Usage

Add `datastructs-fortran` as a dependency in your `fpm.toml`:

```toml
[dependencies]
datastructs-fortran.git = "https://github.com/wcota/datastructs-fortran"
```

Import the main module in your Fortran source:

```fortran
use datastructs_mod
```

### High-Performance Stochastic Gillespie Example

Using `time_grid_t` with `statistical_measure_t` and `sampler_base_t`:

```fortran
program gillespie_example
    use datastructs_mod
    use datastructs_kinds_mod
    use rndgen_mod
    implicit none

    type(time_grid_t) :: grid
    type(statistical_measure_t) :: state_measure
    class(sampler_base_t), allocatable :: sampler
    type(rndgen) :: gen

    real(kind=dp) :: t, state, total_rate
    integer(kind=i4) :: traj

    ! 1. Precalculate observation grid: e.g. power-law/geometric spacing
    call grid%init_geometric(tmin=1.0_dp, tmax=100.0_dp, ratio=1.05_dp)
    call state_measure%init(grid%size())

    ! 2. Configure weighted sampler
    call choose_sampler(sampler, 'btree')
    call sampler%init(2)
    call sampler%set_weight(1, 10.0_dp)
    call sampler%set_weight(2, 5.0_dp)
    total_rate = sampler%sum()

    call gen%init(12345)

    ! 3. Simulation trajectories
    do traj = 1, 1000
        t = 0.0_dp
        state = 0.0_dp
        call grid%reset()

        do while (t < 100.0_dp .and. grid%has_next())
            ! Advance Gillespie jump time
            t = t + (-log(1.0_dp - gen%rnd()) / total_rate)

            ! Zero-allocation O(1) observation record
            call grid%record(state_measure, t, state)

            ! Update physical state
            if (sampler%sample(gen) == 1) then
                state = state + 1.0_dp
            else
                state = max(0.0_dp, state - 1.0_dp)
            end if
        end do
        call grid%record(state_measure, 100.0_dp, state)
    end do
end program gillespie_example
```

## Running Examples and Tests

Run all tests:
```bash
fpm test
```

Run specific examples:
```bash
fpm run --example time_grid
fpm run --example sampler
fpm run --example maxheap
fpm run --example dynamical_list
```

## Versioning and Release Automation

A release helper script is available in `scripts/release.py`:

```bash
# Preview upcoming release without making changes
python3 scripts/release.py patch --dry-run
python3 scripts/release.py minor --dry-run

# Perform version bump (updates fpm.toml, creates git commit and annotated tag)
python3 scripts/release.py minor

# Or specify a target version directly
python3 scripts/release.py 0.5.0
```
