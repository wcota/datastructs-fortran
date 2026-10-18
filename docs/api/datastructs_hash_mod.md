---
title: datastructs_hash_mod
---

# datastructs_hash_mod

> Non-cryptographic hashing and integer packing algorithms.

 Provides:
 - `djb2`: Classic Dan Bernstein string/array hash adapted for 32-bit integer arrays.
 - `fnv1a`: 64-bit Fowler–Noll–Vo hash with high dispersion across permutations.
 - `pack_pair`: Perfect bijection packing two 32-bit positive integers into a single 64-bit integer.

**Source**: `src/misc/hash.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_hash_mod["datastructs_hash_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
```

## Contents

- [djb2](#djb2)
- [pack_pair](#pack-pair)
- [fnv1a](#fnv1a)

## Functions

### djb2

Computes the 64-bit djb2 hash of an array of 32-bit integers.

 Formula: \( \text{hash} = ((\text{hash} \ll 5) + \text{hash}) + x_i \)
 Starting with initial offset 5381.

**Returns**: `integer(kind=i8)`

```fortran
function djb2(list) result(r)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `list` | integer(kind=i4) | in |  | Array of 32-bit integers to hash |

### pack_pair

Packs a pair of positive 32-bit integers into a unique 64-bit integer.

 Uses bitwise shift: \( r = (a \ll 32) + b \).
 Guaranteed zero false collisions for all \( a, b \in [0, 2^{32}-1] \).

**Returns**: `integer(kind=i8)`

```fortran
function pack_pair(a, b) result(r)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `a` | integer(kind=i4) | in |  | First 32-bit integer (upper 32 bits) |
| `b` | integer(kind=i4) | in |  | Second 32-bit integer (lower 32 bits) |

### fnv1a

Computes the 64-bit FNV-1a (Fowler–Noll–Vo) hash of an array of 32-bit integers.

 Provides excellent avalanche dispersion and collision resistance for hyperedges
 and sequence tuples.

**Returns**: `integer(kind=i8)`

```fortran
function fnv1a(list) result(r)
```

**Arguments**

| Name | Type | Intent | Attributes | Description |
|------|------|--------|------------|-------------|
| `list` | integer(kind=i4) | in |  | Array of 32-bit integers to hash |
