---
title: datastructs_sorting_mod
---

# datastructs_sorting_mod

> Internal high-performance sorting module for datastructs-fortran.
 Provides in-place introsort (hybrid of median-of-3 quicksort, heapsort fallback,
 and insertion sort) with guaranteed O(N log N) worst-case complexity and
 O(1) auxiliary space, without requiring any external dependencies (such as stdlib).

**Source**: `src/misc/sorting.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_sorting_mod["datastructs_sorting_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
```

## Contents

- [sort](#sort)
- [sort_index](#sort-index)

## Interfaces

### sort

Sorts an array in-place in ascending (default) or descending order (`reverse=.true.`).
 Uses Introsort (median-of-three Quicksort with cutoff to Insertion sort and Heapsort fallback).
 Guaranteed worst-case \(O(N \log N)\) complexity and \(O(1)\) auxiliary memory.

**Module procedures**: `sort_i4`, `sort_i8`, `sort_dp`

### sort_index

Computes the permutation indices that sort `arr` without modifying `arr`.
 If `idx` is not allocated or has the wrong size, it is automatically allocated.
 Guaranteed worst-case \(O(N \log N)\) complexity.

**Module procedures**: `sort_index_i4`, `sort_index_i8`, `sort_index_dp`
