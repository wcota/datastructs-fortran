# Getting Started with datastructs-fortran

`datastructs-fortran` is a modular, high-performance Fortran 2008+ library engineered for scientific computing, stochastic simulations, and data management.

## Key Features

- **Continuous-Time Stochastic Samplers**: 6 algorithms ranging from $O(1)$ rejection to $O(\log N)$ binary trees and multi-tier geometric composition for broad rate distributions.
- **Temporal Observation Grid (`time_grid_t`)**: Precalculated time ticks for Gillespie and Markov chain simulations with zero allocations in event loops.
- **Native $O(N \log N)$ Sorting**: Guaranteed worst-case Introsort with zero external dependencies.
- **Dynamic Containers**: Unordered dynamic lists with $O(1)$ swap-with-last removal, binary max-heaps with reverse index lookup, and doubly-linked fixed lists.
- **Statistical Accumulators**: Online computation of moments, mean, variance, and histograms with weighted observations.

---

## Installation via FPM

Add `datastructs-fortran` to your `fpm.toml`:

```toml
[dependencies]
datastructs-fortran = { git = "https://github.com/wcota/datastructs-fortran", tag = "v0.6.1" }
```

To use development branch directly:

```toml
[dependencies]
datastructs-fortran = { git = "https://github.com/wcota/datastructs-fortran", branch = "main" }
```

---

## Module Import Strategy

You can import everything via the umbrella module:

```fortran
use datastructs_mod
```

Or selectively import specific modules for faster compilation:

```fortran
use datastructs_kinds_mod       ! sp, dp, i4, i8
use datastructs_sorting_mod     ! sort, sort_index
use datastructs_time_grid_mod   ! time_grid_t
use datastructs_measures_mod    ! statistical_measure_t
use datastructs_samplers_mod    ! choose_sampler, sampler_base_t
use datastructs_maxheap_mod     ! maxheap_t
```

---

## Guides in this Documentation

Explore the detailed guides for each subsystem:

1. [**Weighted Samplers**](./samplers.md): Theoretical foundations and decision guide for continuous-time event sampling.
2. [**Temporal Time Grid**](./time-grid.md): Event recording for jump processes and CTMC models with zero overhead.
3. [**High-Performance Sorting**](./sorting.md): Guaranteed $O(N \log N)$ Introsort benchmarks and sorting with indices.
4. [**Containers & Priority Queues**](./containers.md): Dynamic lists, Max-Heaps, and linked structures.
