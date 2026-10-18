---
title: datastructs_kinds_mod
---

# datastructs_kinds_mod

> Module providing kind parameters and utility functions for numerical types.

 Includes:
 - Predefined kind parameters for real and integer types
 - Utility subroutines like swap for integers and reals
 - Functions for formatting and parsing data
 Single precision real (≈32 bits), range: -3.40282347E+38 to 3.40282347E+38, smallest positive: 1.17549435E-38
 Double precision real (≈64 bits), range: -1.7976931348623157E+308 to 1.7976931348623157E+308, smallest positive: 2.2250738585072014E-308
 Quadruple precision real (≈128 bits)
 1-byte integer, range: -128 to 127
 2-byte integer, range: -32768 to 32767
 4-byte integer, range: -2147483648 to 2147483647
 8-byte integer, range: -9223372036854775808 to 9223372036854775807

**Source**: `src/misc/kinds.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_kinds_mod["datastructs_kinds_mod"] --> iso_fortran_env["iso_fortran_env"]
```

## Contents

- [swap](#swap)
- [list_ranges](#list-ranges)
- [choose_fmt_based_on](#choose-fmt-based-on)
- [count_integers_from_string](#count-integers-from-string)

## Variables

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `fmt_general` | character(len=*) | parameter | Default output formats for write operations |
| `fmt_comma` | character(len=*) | parameter | Comma-separated |

## Interfaces

### swap

Generic interface for swapping values of different types.

**Module procedures**: `swap_int`, `swap_real`

## Subroutines

### list_ranges

Print ranges for supported numeric types.

```fortran
subroutine list_ranges()
```

## Functions

### choose_fmt_based_on

Select a format string based on the file extension.

**Returns**: `character(len=:)`

```fortran
function choose_fmt_based_on(filename) result(fmt)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `filename` | character(len=*) | in |  | Input filename |

### count_integers_from_string

Count the number of integers in a string separated by spaces or commas.

**Returns**: `integer(kind=i4)`

```fortran
function count_integers_from_string(str) result(count)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `str` | character(len=*) | in |  | Input string |
