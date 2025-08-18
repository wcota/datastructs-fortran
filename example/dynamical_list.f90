program example_dynamical_list
    use datastructs_mod
    use datastructs_kinds_mod
    implicit none
    type(dynamical_list_t) :: my_list
    integer(kind=i4) :: i
    call my_list%init(10)
    do i = 1, 10
        call my_list%add(i)
    end do
    call my_list%print()
    call my_list%remove(5)
    call my_list%print()
end program example_dynamical_list
