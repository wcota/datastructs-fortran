---
title: datastructs_lists_mod
---

# datastructs_lists_mod

> General module for lists and related operations
 It imports the necessary modules for list and heap implementations

**Source**: `src/lists/lists.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_lists_mod["datastructs_lists_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
  datastructs_lists_mod["datastructs_lists_mod"] --> datastructs_lists_dynamical_list_mod["datastructs_lists_dynamical_list_mod"]
  datastructs_lists_mod["datastructs_lists_mod"] --> datastructs_lists_fixed_list_mod["datastructs_lists_fixed_list_mod"]
  datastructs_lists_mod["datastructs_lists_mod"] --> datastructs_maxheap_mod["datastructs_maxheap_mod"]
```

## Contents

- [unique_values](#unique-values)

## Functions

### unique_values

Find unique values in a list of integers.
 Uses internal high-performance Introsort to sort values in \(O(N \log N)\)
 before sequential deduplication.
 Returns a newly allocated array of unique integers in ascending order.

**Returns**: `integer(kind=i4)`

```fortran
function unique_values(list) result(unique)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `list` | integer(kind=i4) | in |  | Input list of integers |

**Call graph**

```mermaid
flowchart TD
  unique_values["unique_values"] --> sort["sort"]
  style unique_values fill:#3e63dd,stroke:#99b,stroke-width:2px
```
