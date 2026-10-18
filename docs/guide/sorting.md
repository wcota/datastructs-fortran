# Native Introsort (`datastructs_sorting_mod`)

Modern scientific codes frequently sort arrays of coordinates, energies, or rates. While standard libraries like `stdlib` provide sorting, importing external libraries can introduce dependency conflicts in FPM packages.

`datastructs_sorting_mod` provides an optimized **Introsort** (Musser, 1997) implemented natively in pure Fortran 2008 with:
- **Worst-case $O(N \log N)$ complexity**: Hybrid of QuickSort, switching to HeapSort when recursion depth exceeds $2 \lfloor \log_2 N \rfloor$, and InsertionSort for small sub-arrays ($N \le 16$).
- **Zero dynamic memory allocation** for direct in-place sort (`sort`).
- **Support for both direct in-place sort and permutation index sort (`sort_index`)** across `integer(i4)` and `real(dp)`.

---

## API Overview

```fortran
use datastructs_sorting_mod, only: sort, sort_index
implicit none

integer(kind=i4) :: arr(5)
integer(kind=i4), allocatable :: idx(:)

arr = [50, 20, 40, 10, 30]

! 1. In-place sorting (ascending by default)
call sort(arr)
! arr is now: [10, 20, 30, 40, 50]

! 2. In-place sorting descending
call sort(arr, reverse=.true.)
! arr is now: [50, 40, 30, 20, 10]

! 3. Permutation indices without mutating original array
arr = [50, 20, 40, 10, 30]
call sort_index(arr, idx)
! idx is: [4, 2, 5, 3, 1] (arr(idx) is sorted)
```

---

## Benchmark: Native Introsort vs. stdlib_sorting

Tested on Linux x86_64, `gfortran -O3`:

| Array Size $N$ | Routine | Native (ms) | `stdlib` (ms) | Speedup Ratio |
| :--- | :--- | :--- | :--- | :--- |
| **$N = 100$** | `sort(integer)` | 0.0061 ms | 0.0062 ms | **1.02x** |
| | `sort_index(integer)` | 0.0082 ms | 0.0112 ms | **1.37x faster** |
| **$N = 1,000$** | `sort(integer)` | 0.086 ms | 0.093 ms | **1.08x faster** |
| | `sort_index(integer)` | 0.116 ms | 0.168 ms | **1.45x faster** |
| **$N = 50,000$** | `sort(integer)` | 6.66 ms | 7.37 ms | **1.11x faster** |
| | `sort_index(integer)` | 9.15 ms | 11.94 ms | **1.30x faster** |
| **$N = 500,000$** | `sort(integer)` | 75.4 ms | 92.9 ms | **1.23x faster** |
| | `sort_index(integer)` | 119.4 ms | 149.9 ms | **1.25x faster** |

The native Introsort matches or outperforms `stdlib` across all array sizes while eliminating the external library dependency.
