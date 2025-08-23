program test_dynamical_list2
    use datastructs_kinds_mod
    use datastructs_mod, only: dynamical_list_t, dynamical_list
    use rndgen_mod
    implicit none

    type(dynamical_list_t) :: list
    type(rndgen) :: rnd

    call rnd%init(12345)

    ! Initilize the list of size 50 with random integers
    list = dynamical_list(rnd%rnd_array(50, 1, 100))
    call list%print()

    call list%remove(1)
    call list%print()

    call list%add(10)
    call list%print()

    call list%remove(2)
    call list%print()

    call list%remove(3)
    call list%print()

    call list%add([1,2])
    call list%print()

    call list%remove([20,21])
    call list%print()

    call list%remove(20,30)
    call list%print()

    call list%reset()
    call list%print()

    call list%add(rnd%rnd_array(10, 1, 100))
    call list%print()

    call list%trim()
    call list%print()
    write(*,fmt_general) 'Sum = ', list%sum()

    list = dynamical_list([1,2,3]) ! will trigger error

end program test_dynamical_list2