program test_hash
    use datastructs_kinds_mod
    use datastructs_mod, only : djb2, pack_pair
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

    write(*,fmt_general) 'Packing the first two elements of the list:'
    write(*,fmt_general) 'Elements: ', list(1), list(2)
    write(*,fmt_general) 'Packed value:', pack_pair(list(1), list(2))
    write(*,fmt_general) 'Packed value (swapped):', pack_pair(list(2), list(1))

end program test_hash