---
title: datastructs_samplers_rejection_maxheap_mod
---

# datastructs_samplers_rejection_maxheap_mod

> Rejection weighted sampler with dynamic Max-Heap tracking.

 Maintains the exact maximum active weight \( w_{\text{max}} \) dynamically using
 an internal `maxheap_t`. Avoids acceptance rate degradation when large weights
 are removed or decreased.

 Complexity:
 - **Sampling**: \( O(1) \) expected trials (\(\approx w_{\text{max}} / \bar{w}\)).
 - **Weight update**: \( O(\log N) \) (heapsort sift-up / sift-down).
 - **Memory**: \( O(N) \) (stores dynamic list + binary heap).

**Source**: `src/samplers/rejection_maxheap.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_samplers_rejection_maxheap_mod["datastructs_samplers_rejection_maxheap_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
  datastructs_samplers_rejection_maxheap_mod["datastructs_samplers_rejection_maxheap_mod"] --> datastructs_lists_mod["datastructs_lists_mod"]
  datastructs_samplers_rejection_maxheap_mod["datastructs_samplers_rejection_maxheap_mod"] --> datastructs_samplers_base_mod["datastructs_samplers_base_mod"]
```

## Contents

- [weighted_sampler_t](#weighted-sampler-t)
- [weighted_sampler](#weighted-sampler)

## Derived Types

### weighted_sampler_t

Derived type implementing the maxheap-tracked rejection sampler

**Inheritance**

```mermaid
classDiagram
  sampler_base_t <|-- weighted_sampler_t
```

**Extends**: [`sampler_base_t`](/api/datastructs_samplers_base_mod#sampler-base-t)

#### Components

| Name | Type | Attributes | Description |
|------|------|------------|-------------|
| `weights` | real(kind=dp) | allocatable | Individual weights assigned to each element |
| `n` | integer(kind=i4) |  | Total number of weights |
| `indices` | type([dynamical_list_t](/api/datastructs_lists_dynamical_list_mod#dynamical-list-t)) | allocatable | Dynamic list of active element indices |
| `heap` | type([maxheap_t](/api/datastructs_maxheap_mod#maxheap-t)) | allocatable | Max-Heap tracking exact maximum active weight |
| `position_of` | integer(kind=i4) | allocatable | Map from element index to position in `indices` |
| `current_sum` | real(kind=dp) |  | Current sum of all active weights |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `init` |  | Generic initialization interface |
| `init_n` |  | Initializes sampler with capacity N |
| `init_w` |  | Initializes with capacity N |
| `init_w2` |  | Initializes with capacity N |
| `reset` |  | Clears active elements and resets heap |
| `set_weight` |  | Sets weight for an index in O(log N) |
| `set_weight_array` |  | Sets all weights in O(N log N) |
| `add_weight` |  | Adds delta weight in O(log N) |
| `sample` |  | Samples an index via rejection with exact max-weight |
| `remove` |  | Removes index from active list and heap in O(log N) |
| `sum` |  | Returns total sum of weights in O(1) |

## Interfaces

### weighted_sampler

Constructor

**Module procedures**: `weighted_sampler_new`
