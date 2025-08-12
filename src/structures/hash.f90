module hOGA_hash_mod
    use hOGA_kinds_mod
    implicit none
    private

    public :: djb2

    contains

    !> DJB2 hash function
    !> Inspired by http://www.cse.yorku.ca/~oz/hash.html
    !> and https://github.com/pdebuyl/fortran_hash_table/blob/master/src/dictionary_m.f90
    function djb2(list) result(r)
        integer(kind=i4), intent(in) :: list(:)
        integer(kind=i8) :: r
    
        integer(kind=i4) :: i, l
    
        l = size(list)
    
        r = 5381_i8
    
        do i = 1, l
           r = (ishft(r, 5) + r) + int(list(i), i8)
        end do
    
      end function djb2

end module
