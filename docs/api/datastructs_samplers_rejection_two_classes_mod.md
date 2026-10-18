---
title: datastructs_samplers_rejection_two_classes_mod
---

# datastructs_samplers_rejection_two_classes_mod

> Two-class rejection weighted sampler.

 Partitions elements into two classes (e.g. slow and fast reactions) according to
 a weight threshold. First selects an active class proportionally to its total weight,
 then samples within that class using rejection sampling.

 Complexity:
 - **Sampling**: \( O(1) \) expected trials, significantly higher acceptance efficiency
   than single-class rejection when rates have bimodal or disparate magnitudes.
 - **Weight update**: \( O(1) \) (may move element between the two classes).
 - **Memory**: \( O(N) \).

**Source**: `src/samplers/rejection_two_classes.f90`

**Dependencies**

```mermaid
graph LR
  datastructs_samplers_rejection_two_classes_mod["datastructs_samplers_rejection_two_classes_mod"] --> datastructs_kinds_mod["datastructs_kinds_mod"]
  datastructs_samplers_rejection_two_classes_mod["datastructs_samplers_rejection_two_classes_mod"] --> datastructs_lists_mod["datastructs_lists_mod"]
  datastructs_samplers_rejection_two_classes_mod["datastructs_samplers_rejection_two_classes_mod"] --> datastructs_samplers_base_mod["datastructs_samplers_base_mod"]
  datastructs_samplers_rejection_two_classes_mod["datastructs_samplers_rejection_two_classes_mod"] --> datastructs_samplers_rejection_mod["datastructs_samplers_rejection_mod"]
```

## Contents

- [weighted_sampler_t](#weighted-sampler-t)
- [weighted_sampler](#weighted-sampler)

## Derived Types

### weighted_sampler_t

Derived type implementing the two-class rejection sampler

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
| `samplers` | type([weighted_sampler_t](/api/datastructs_samplers_rejection_mod#weighted-sampler-t)) |  | Sub-samplers for class 1 (slow) and class 2 (fast) |
| `threshold` | real(kind=dp) |  | Weight threshold separating the two classes |
| `sampler_of_index` | integer(kind=i4) | allocatable | Map from element index to class index (1 or 2) |

#### Type-Bound Procedures

| Name | Attributes | Description |
|------|------------|-------------|
| `init` |  | Generic initialization interface |
| `init_n` |  | Initializes sampler with capacity N |
| `init_w` |  | Initializes with capacity N and custom threshold |
| `init_w2` |  | Initializes with capacity N, threshold, and max_weight |
| `reset` |  | Clears both sub-samplers and index maps |
| `set_weight` |  | Sets weight for an index, placing it into class 1 or 2 in O(1) |
| `set_weight_array` |  | Sets all weights in O(N) |
| `add_weight` |  | Adds delta weight in O(1) |
| `sample` |  | Selects class proportionally then samples via rejection |
| `remove` |  | Removes element in O(1) |
| `sum` |  | Returns sum of weights across both classes in O(1) |

## Interfaces

### weighted_sampler

Constructor

**Module procedures**: `weighted_sampler_new`
