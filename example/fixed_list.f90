program example_fixed_list
    use datastructs_mod
    use datastructs_kinds_mod
    implicit none
    type(fixed_list_t) :: my_list
    type(fixed_list_t), target :: my_list2
    my_list = fixed_list([1,2,3,4,5])
    my_list2 = fixed_list([6,7,8])

    call my_list%print()

    my_list%next => my_list2

    call my_list%next%print()
end program example_fixed_list
