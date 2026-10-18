---
layout: home

hero:
  name: "datastructs-fortran"
  text: "Data Structures & Samplers for Fortran"
  tagline: "High-performance data structures, continuous-time random samplers, and sorting for modern scientific Fortran."
  actions:
    - theme: brand
      text: API Reference
      link: /api/
    - theme: alt
      text: GitHub Repository
      link: https://github.com/wcota/datastructs-fortran

features:
  - title: Weighted Samplers
    details: Binary tree (B-Tree), rejection, and composition samplers optimized for stochastic Monte Carlo and Gillespie CTMC dynamics.
  - title: Temporal Time Grid
    details: Piecewise-constant observation grids (uniform, geometric, logarithmic) with zero allocations in event loops.
  - title: Fast Native Sorting
    details: Guaranteed O(N log N) in-place Introsort and sort_index with zero external dependencies.
  - title: Containers & Lists
    details: Dynamic expandable lists, linked fixed lists, and Max-Heap priority queues.
---

## Overview

`datastructs-fortran` is a modular, high-performance Fortran library providing common data structures, stochastic sampling algorithms, and utilities for scientific computing.

It is designed with **zero runtime overhead**, memory efficiency, and minimal dependencies in mind.

## Installation via FPM

Add `datastructs-fortran` to your `fpm.toml`:

```toml
[dependencies]
datastructs-fortran = { git = "https://github.com/wcota/datastructs-fortran", tag = "v0.6.1" }
```

Then simply import the umbrella module or individual components:

```fortran
use datastructs_mod
```

---

## Key Modules

### 1. Weighted Samplers (`choose_sampler`)
Six sampling algorithms sharing an abstract interface (`sampler_base_t`):
- **`btree`**: Complete binary tree with prefix-sum indexing ($O(\log N)$ updates, $O(\log N)$ sampling). Exact and deterministic.
- **`rejection`**: Direct rejection with fixed maximum weight ($O(1)$ updates, acceptance proportional to weight distribution).
- **`rejection_maxheap`**: Rejection sampling maintaining exact max-weight via Max-Heap.
- **`rejection_two_classes`** / **`rejection_maxheap_two_classes`**: Two-class partitioning (fast vs. slow rates).
- **`rejection_maxheap_composition`**: Multi-class geometric binning ($[2^k, 2^{k+1})$) for broad weight distributions.

### 2. Temporal Time Grid (`time_grid_t`)
Optimized for Gillespie simulations and continuous-time Markov chains:
```fortran
type(time_grid_t) :: grid
type(statistical_measure_t) :: measure

! Initialize a geometric or uniform grid
call grid%init_geometric(tmin=0.1_dp, tmax=100.0_dp, ratio=1.2_dp)
call measure%init(grid%size())

! During stochastic trajectory:
call grid%record(measure, t, current_state)
```

### 3. Native Sorting (`datastructs_sorting_mod`)
High-performance in-place Introsort (median-of-three Quicksort + Insertion sort + Heapsort fallback) and non-destructive `sort_index`:
```fortran
use datastructs_mod, only: sort, sort_index

call sort(my_array)                    ! In-place O(N log N)
call sort(my_array, reverse=.true.)    ! Descending order
call sort_index(my_array, sorted_idx)  ! Permutation indices without mutating array
```

### 4. Dynamic Lists & Priority Queues
- **`dynamical_list_t`**: Expandable array list with element removal, search, insertion, and bulk operations.
- **`fixed_list_t`**: Fixed-size list with next/prev pointer linking.
- **`maxheap_t`**: Binary max-heap for priority queues and rate tracking.

### 5. Hashing & Serialization
- **`pack_pair`**: Cantor-pairing-based perfect hash mapping two `integer(i4)` into a unique `integer(i8)` with zero false collisions.
- **`djb2`** & **`fnv1a`**: Fast non-cryptographic hashes for arrays of integers.
