program test_hash_collisions
    use datastructs_kinds_mod
    use datastructs_mod, only : djb2, fnv1a
    use rndgen_mod
    use stdlib_sorting, only : sort, sort_index
    implicit none

    integer(kind=i4), parameter :: N_SAMPLES = 1000
    integer(kind=i4), parameter :: MAX_NODES = 5
    integer(kind=i4), parameter :: NODE_RANGE = 10

    type(rndgen) :: gen
    integer(kind=i4), allocatable :: hyperedge(:)
    integer(kind=i8), allocatable :: hashes_djb2(:), hashes_fnv1a(:)
    integer(kind=i4), allocatable :: sorted_idx(:)

    ! structure to store the actual edges for later checking
    integer(kind=i4), allocatable :: all_data(:)
    integer(kind=i4), allocatable :: offsets(:) ! Where each edge starts in all_data
    integer(kind=i4), allocatable :: sizes(:)   ! Size of each edge

    integer(kind=i4) :: i, j, edge_size, current_pos
    integer(kind=i4) :: false_djb2, false_fnv1a

    call gen%init(42)

    allocate(hashes_djb2(N_SAMPLES), hashes_fnv1a(N_SAMPLES))
    allocate(offsets(N_SAMPLES), sizes(N_SAMPLES))
    allocate(all_data(N_SAMPLES * MAX_NODES)) ! Maximum estimated space
    allocate(sorted_idx(N_SAMPLES))

    write(*,fmt_general) ">>> Generating samples and storing data..."

    current_pos = 1
    do i = 1, N_SAMPLES
        edge_size = gen%int(2, MAX_NODES)
        allocate(hyperedge(edge_size))
        hyperedge = gen%rnd_array(edge_size, 1, NODE_RANGE)
        call sort(hyperedge)

        ! Stores metadata for later checking
        offsets(i) = current_pos
        sizes(i)   = edge_size
        all_data(current_pos : current_pos + edge_size - 1) = hyperedge
        current_pos = current_pos + edge_size

        hashes_djb2(i)  = djb2(hyperedge)
        hashes_fnv1a(i) = fnv1a(hyperedge)
        deallocate(hyperedge)
    end do

    ! --- False Collisions ---
    write(*,fmt_general) ">>> Analyzing FALSE collisions (Hash match, Data mismatch)..."

    ! DJB2
    call sort_index(hashes_djb2, sorted_idx)
    false_djb2 = 0
    do i = 1, N_SAMPLES - 1
        if (hashes_djb2(sorted_idx(i)) == hashes_djb2(sorted_idx(i+1))) then
            ! If hashes are equal, check if the content is different
            if (.not. are_edges_equal(sorted_idx(i), sorted_idx(i+1))) then
                false_djb2 = false_djb2 + 1
            end if
        end if
    end do

    ! FNV-1a
    call sort_index(hashes_fnv1a, sorted_idx)
    false_fnv1a = 0
    do i = 1, N_SAMPLES - 1
        if (hashes_fnv1a(sorted_idx(i)) == hashes_fnv1a(sorted_idx(i+1))) then
            if (.not. are_edges_equal(sorted_idx(i), sorted_idx(i+1))) then
                false_fnv1a = false_fnv1a + 1
            end if
        end if
    end do

    write(*,fmt_general) "------------------------------------------"
    write(*,fmt_general) "Results for ", N_SAMPLES, " samples (NODE_RANGE=", NODE_RANGE, "):"
    write(*,fmt_general) "DJB2 FALSE Collisions:  ", false_djb2
    write(*,fmt_general) "FNV-1a FALSE Collisions: ", false_fnv1a
    write(*,fmt_general) "------------------------------------------"

contains

    logical function are_edges_equal(idx1, idx2)
        integer(kind=i4), intent(in) :: idx1, idx2
        integer(kind=i4) :: p1, p2, s1, s2

        s1 = sizes(idx1); s2 = sizes(idx2)
        if (s1 /= s2) then
            are_edges_equal = .false.
            return
        end if

        p1 = offsets(idx1); p2 = offsets(idx2)
        are_edges_equal = all(all_data(p1:p1+s1-1) == all_data(p2:p2+s2-1))
    end function are_edges_equal

end program test_hash_collisions