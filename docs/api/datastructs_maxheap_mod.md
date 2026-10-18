---
title: datastructs_maxheap_mod
---

# datastructs_maxheap_mod

> This module implements a maxheap structure, keeping track of the maximum value and its index.
 Example:
 ```fortran
 program example_maxheap
     use datastructs_mod
     use datastructs_kinds_mod
     implicit none
     type(maxheap_t) :: my_heap
     real(kind=dp), parameter :: list(*) = [40.0_dp,25.5_dp,30.1_dp,100.4_dp,2.0_dp]
     integer(kind=i4) :: i
 
     call my_heap%init(10)
 
     do i = 1, size(list)
         call my_heap%add(list(i), i)
     end do
 
     call my_heap%print()
 
     call my_heap%remove(4)
 
     call my_heap%print()
 end program example_maxheap
 ```

**Source**: `src/lists/maxheap.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_maxheap_mod["datastructs_maxheap_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
```

## Contents

- [maxheap_t](#maxheap-t)
- [maxheap](#maxheap)

## Derived Types

### maxheap_t

Type for a maxheap

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `values` | real(kind=dp) | allocatable |  |
| `indices` | integer(kind=i4) | allocatable |  |
| `pos_of` | integer(kind=i4) | allocatable |  |
| `n` | integer(kind=i4) |  |  |
| `max_n` | integer(kind=i4) |  |  |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `init` |  | Initialize maxheap with maximum capacity |
| `add` |  | Insert value with external index in O(log N) |
| `remove` |  | Remove element by external index in O(log N) |
| `add_weight` |  | Update weight of existing index in O(log N) |
| `print` |  | Print heap arrays to stdout |
| `max_value` |  | Return maximum weight value in O(1) |
| `max_index` |  | Return external index with maximum weight in O(1) |

## Interfaces

### maxheap

Constructor for a maxheap

**Module procedures**: `maxheap_new`
