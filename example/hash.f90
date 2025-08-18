program test_hash
    use datastructs_kinds_mod
    use datastructs_mod, only : djb2
    use rndgen_mod
    use stdlib_sorting, only : sort, sort_index
    implicit none

    type(rndgen) :: gen

    integer(kind=i4), allocatable :: list(:)
    integer(kind=i4), allocatable :: list_index(:)

    call gen%init(1213213)

    write(*,*) 'Generated list of random integers:'
    list = gen%rnd_array(10, 1, 1000000000)
    
    write(*, fmt_general) list
    write(*, fmt_general) 'Calculating hash:'
    write(*,fmt_general) 'Hash value of the list:', djb2(list)

    allocate(list_index(size(list)))
    write(*,fmt_general) 'Sorting the list using sort_index():'
    call sort_index(list, list_index)
    write(*, fmt_general) list
    write(*, fmt_general) list_index
    write(*, fmt_general) 'Calculating hash:'
    write(*,fmt_general) 'Hash value of the list:', djb2(list)

    write(*,fmt_general) 'Sorting the list using sort():'
    call sort(list)
    write(*, fmt_general) list
    write(*, fmt_general) 'Calculating hash:'
    write(*,fmt_general) 'Hash value of the list:', djb2(list)

end program test_hash