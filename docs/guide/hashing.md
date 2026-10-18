# Hashing & Integer Bit Packing

Scientific simulations frequently work with pairs of objects (such as graph edges $(u, v)$, interacting particles, coordinate indices $(x, y)$, or hash map keys) and variable-length integer sequences (such as paths, hyperedges, or state tuples).

`datastructs-fortran` provides three non-cryptographic, high-performance hashing and packing functions:

1. **`pack_pair`**: Zero-collision bijection packing two 32-bit integers into a single 64-bit integer.
2. **`djb2`**: Fast 64-bit hash for variable-length integer arrays.
3. **`fnv1a`**: 64-bit Fowler–Noll–Vo hash with high dispersion and avalanche properties.

---

## 1. Zero-Collision Pair Packing (`pack_pair`)

In graph algorithms and spatial simulations, one often needs to map an edge between nodes $a$ and $b$ to a single 64-bit key for fast sorting, binary search, or storage in a hash table.

Standard techniques like Cantor pairing $\pi(a, b) = \frac{(a+b)(a+b+1)}{2} + b$ easily overflow 64-bit integers even for moderately sized node IDs ($a, b > 10^7$).

`pack_pair` uses 64-bit bitwise shifting:

$$
\text{packed\_key} = (a \ll 32) + b
$$

```fortran
function pack_pair(a, b) result(r)
    integer(kind=i4), intent(in) :: a  ! upper 32 bits
    integer(kind=i4), intent(in) :: b  ! lower 32 bits
    integer(kind=i8) :: r
    r = ishft(int(a, i8), 32) + int(b, i8)
end function pack_pair
```

### Key Mathematical Properties
- **Exact Bijection**: The mapping is strictly $1:1$ for all non-negative 32-bit integers $a, b \in [0, 2^{32}-1]$.
- **Zero False Collisions**: It is mathematically impossible for two distinct pairs $(a_1, b_1) \neq (a_2, b_2)$ to produce the same 64-bit packed integer.
- **Microsecond Speed**: Executes in a single CPU register shift and addition operation (`shl` + `add`).

### Undirected Edge Canonical Representation

For an undirected edge where $(u, v) \equiv (v, u)$, always pass the canonical order $\min(u, v)$ and $\max(u, v)$:

```fortran
use datastructs_mod, only: pack_pair, i4, i8
implicit none

integer(kind=i4) :: u, v
integer(kind=i8) :: edge_key1, edge_key2

u = 450000
v = 120

! Pack canonical undirected edge
edge_key1 = pack_pair(min(u, v), max(u, v))
edge_key2 = pack_pair(min(v, u), max(v, u))

if (edge_key1 == edge_key2) then
    print *, "Undirected edges match canonically! Packed key:", edge_key1
end if
```

---

## 2. Sequence Hashing (`djb2` & `fnv1a`)

When hashing variable-length arrays of 32-bit integers (e.g. hyperedges $[v_1, v_2, \dots, v_k]$, paths, or molecular subgraphs), `datastructs_hash_mod` provides two robust algorithms:

### Dan Bernstein's Hash (`djb2`)
Iterates over the elements multiplying the running hash by 33 via bitwise shift:

$$
\text{hash}_{i} = ((\text{hash}_{i-1} \ll 5) + \text{hash}_{i-1}) + x_i
$$

Starting with initial offset $5381$.

```fortran
use datastructs_mod, only: djb2, i4, i8
implicit none

integer(kind=i4) :: hyperedge(4)
integer(kind=i8) :: h

hyperedge = [10_i4, 45_i4, 999_i4, 12034_i4]
h = djb2(hyperedge)

print '(A, Z16)', "DJB2 Hash: 0x", h
```

### Fowler–Noll–Vo Hash (`fnv1a`)
Iterates over the sequence XOR-ing with the current value and multiplying by the 64-bit FNV prime:

$$
\text{hash}_i = (\text{hash}_{i-1} \oplus x_i) \times \text{FNV\_PRIME}
$$

Starting with offset basis `0xCBF29CE484222325`.

```fortran
use datastructs_mod, only: fnv1a, i4, i8
implicit none

integer(kind=i4) :: sequence(3)
integer(kind=i8) :: h

sequence = [101_i4, 202_i4, 303_i4]
h = fnv1a(sequence)

print '(A, Z16)', "FNV-1a Hash: 0x", h
```

### Which One Should You Use?

| Algorithm | Strengths | Best Used For |
| :--- | :--- | :--- |
| **`pack_pair`** | Provably $0$ collisions, $O(1)$ CPU cycle | Pairs of nodes, grid cells $(x, y)$, undirected/directed graph edges |
| **`djb2`** | Extremely lightweight, fast on short arrays | Short tuples, quick duplicate detection in memory |
| **`fnv1a`** | Strong avalanche effect, uniform distribution across all 64 bits | Hyperedges, paths, general hash tables and bloom filters |
