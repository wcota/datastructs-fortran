---
title: datastructs_lists_fixed_list_mod
---

# datastructs_lists_fixed_list_mod

> This module implements a fixed list, that has a fixed size and cannot be resized.
 It also has pointers, allowing to build a linked list structure.
 Example:
 ```fortran
 program example_fixed_list
     use datastructs_fortran
     use kinds_mod
     implicit none
     type(fixed_list_t) :: my_list
     type(fixed_list_t), target :: my_list2
     my_list = fixed_list([1,2,3,4,5])
     my_list2 = fixed_list([6,7,8])

     call my_list%print()

     my_list%next => my_list2

     call my_list%next%print()
 end program example_fixed_list
 ```

**Source**: `src/lists/fixed_list.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_lists_fixed_list_mod["datastructs_lists_fixed_list_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
```

## Contents

- [fixed_list_t](#fixed-list-t)
- [fixed_list](#fixed-list)
- [new_fixed_list_pointer](#new-fixed-list-pointer)

## Derived Types

### fixed_list_t

Type for a fixed list, that can be used as a list of lists

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `initial_index` | integer(kind=i4) |  | Metadata |
| `final_index` | integer(kind=i4) |  | Metadata |
| `n` | integer(kind=i4) |  |  |
| `list` | integer(kind=i4) | allocatable |  |
| `next` | type([fixed_list_t](/api/datastructs_lists_fixed_list_mod#fixed-list-t)) | pointer | Pointer to next list in chain |
| `prev` | type([fixed_list_t](/api/datastructs_lists_fixed_list_mod#fixed-list-t)) | pointer | Pointer to previous list in chain |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `init` |  | Initialize the list |
| `sum` |  | Sum the elements |
| `print` |  | Print the list elements |

## Interfaces

### fixed_list

Constructor for a fixed list

**Module procedures**: `fixed_list_new`

## Functions

### new_fixed_list_pointer

Create a new fixed list pointer
 Input: list - an array of integers

**Returns**: type([fixed_list_t](/api/datastructs_lists_fixed_list_mod#fixed-list-t))

```fortran
function new_fixed_list_pointer(list) result(this)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `list` | integer(kind=i4) | in |  |  |
