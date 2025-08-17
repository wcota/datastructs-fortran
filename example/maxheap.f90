program example_maxheap
    use datastructs_fortran
    use kinds_mod
    implicit none
    type(maxheap_t) :: my_heap
    real(kind=dp), parameter :: list(*) = [40.0_dp,25.5_dp,30.1_dp,100.4_dp,2.0_dp]
    integer(kind=i4) :: i

    call my_heap%init(10)

    do i = 1, size(list)
        call my_heap%add(list(i), i)
    end do

    call my_heap%print()

    call my_heap%remove(4)

    call my_heap%print()
end program example_maxheap
