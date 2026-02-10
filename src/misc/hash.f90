module datastructs_hash_mod
    use datastructs_kinds_mod
    implicit none
    private

    public :: djb2, pack_pair, fnv1a

contains

    !> DJB2 hash function
    !> Inspired by:
    !> - http://www.cse.yorku.ca/~oz/hash.html
    !> - https://github.com/pdebuyl/fortran_hash_table/blob/master/src/dictionary_m.f90
    function djb2(list) result(r)
        integer(kind=i4), intent(in) :: list(:) !! List of integers to hash
        integer(kind=i8) :: r !! Computed hash value

        integer(kind=i4) :: i !! Loop index
        integer(kind=i4) :: l !! Length of the list

        l = size(list)

        r = 5381_i8

        do i = 1, l
            r = (ishft(r, 5) + r) + int(list(i), i8)
        end do

    end function djb2

    !> Pack a pair of 32 bit integers into a single integer of 64 bits
    !> Only works for positive integers!
    function pack_pair(a, b) result(r)
        integer(kind=i4), intent(in) :: a, b
        integer(kind=i8) :: r

        r = ishft(int(a, i8), 32) + int(b, i8)

    end function pack_pair

    !> FNV-1a hash function (64-bit)
    !> Better dispersion for longer lists/hyperedges
    !> Check https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
    function fnv1a(list) result(r)
        integer(kind=i4), intent(in) :: list(:)
        integer(kind=i8) :: r

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
