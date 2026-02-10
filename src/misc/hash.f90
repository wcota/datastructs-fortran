module datastructs_hash_mod
    use datastructs_kinds_mod
    implicit none
    private

    public :: djb2, pack_pair

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
    function pack_pair(a, b) result(r)
        integer(kind=i4), intent(in) :: a, b
        integer(kind=i8) :: r

        r = int(a, i8) * 4294967296_i8 + int(b, i8)

    end function pack_pair

end module datastructs_hash_mod
