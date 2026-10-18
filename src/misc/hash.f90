!> Non-cryptographic hashing and integer packing algorithms.
!>
!> Provides:
!> - `djb2`: Classic Dan Bernstein string/array hash adapted for 32-bit integer arrays.
!> - `fnv1a`: 64-bit Fowler–Noll–Vo hash with high dispersion across permutations.
!> - `pack_pair`: Perfect bijection packing two 32-bit positive integers into a single 64-bit integer.
module datastructs_hash_mod
    use datastructs_kinds_mod
    implicit none
    private

    public :: djb2, pack_pair, fnv1a

contains

    !> Computes the 64-bit djb2 hash of an array of 32-bit integers.
    !>
    !> Formula: \( \text{hash} = ((\text{hash} \ll 5) + \text{hash}) + x_i \)
    !> Starting with initial offset 5381.
    function djb2(list) result(r)
        integer(kind=i4), intent(in) :: list(:) !! Array of 32-bit integers to hash
        integer(kind=i8) :: r !! Resulting 64-bit hash value

        integer(kind=i4) :: i
        integer(kind=i4) :: l

        l = size(list)
        r = 5381_i8

        do i = 1, l
            r = (ishft(r, 5) + r) + int(list(i), i8)
        end do

    end function djb2

    !> Packs a pair of positive 32-bit integers into a unique 64-bit integer.
    !>
    !> Uses bitwise shift: \( r = (a \ll 32) + b \).
    !> Guaranteed zero false collisions for all \( a, b \in [0, 2^{32}-1] \).
    function pack_pair(a, b) result(r)
        integer(kind=i4), intent(in) :: a !! First 32-bit integer (upper 32 bits)
        integer(kind=i4), intent(in) :: b !! Second 32-bit integer (lower 32 bits)
        integer(kind=i8) :: r !! Packed 64-bit integer

        r = ishft(int(a, i8), 32) + int(b, i8)

    end function pack_pair

    !> Computes the 64-bit FNV-1a (Fowler–Noll–Vo) hash of an array of 32-bit integers.
    !>
    !> Provides excellent avalanche dispersion and collision resistance for hyperedges
    !> and sequence tuples.
    function fnv1a(list) result(r)
        integer(kind=i4), intent(in) :: list(:) !! Array of 32-bit integers to hash
        integer(kind=i8) :: r !! Resulting 64-bit hash value

        integer(kind=i4) :: i, l
        integer(kind=i8), parameter :: FNV_OFFSET_BASIS = int(z'CBF29CE484222325', i8)
        integer(kind=i8), parameter :: FNV_PRIME = int(z'00000100000001B3', i8)

        l = size(list)
        r = FNV_OFFSET_BASIS

        do i = 1, l
            r = ieor(r, int(list(i), i8))
            r = r * FNV_PRIME
        end do
    end function fnv1a

end module datastructs_hash_mod
