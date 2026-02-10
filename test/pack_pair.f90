program test_pack_perfection
    use datastructs_kinds_mod
    use datastructs_mod, only : pack_pair
    use rndgen_mod
    use stdlib_sorting, only : sort_index
    implicit none

    integer(kind=i4), parameter :: N_SAMPLES = int(1e6_dp, i4) ! tested with 1e8_dp (passed)
    integer(kind=i4), parameter :: NODE_RANGE = huge(i4)

    type(rndgen) :: gen
    integer(kind=i4), allocatable :: original_src(:), original_tgt(:)
    integer(kind=i8), allocatable :: packed_values(:)
    integer(kind=i4), allocatable :: sorted_idx(:)

    integer(kind=i4) :: i, false_collisions, natural_duplicates

    call gen%init(12345)

    allocate(original_src(N_SAMPLES), original_tgt(N_SAMPLES))
    allocate(packed_values(N_SAMPLES), sorted_idx(N_SAMPLES))

    write(*,*) ">>> Generating ", N_SAMPLES, " pairs and packing them..."

    do i = 1, N_SAMPLES
        ! generating random pairs (src, tgt)
        original_src(i) = gen%int(1, NODE_RANGE)
        original_tgt(i) = gen%int(1, NODE_RANGE)

        ! Computing the "perfect hash"
        packed_values(i) = pack_pair(original_src(i), original_tgt(i))
    end do

    write(*,*) ">>> Analyzing for false collisions..."

    ! Sorting to find adjacent equal values
    call sort_index(packed_values, sorted_idx)

    false_collisions = 0
    natural_duplicates = 0

    do i = 1, N_SAMPLES - 1
        if (packed_values(sorted_idx(i)) == packed_values(sorted_idx(i+1))) then

            ! Real proof: if the pack is equal, the originals MUST be equal
            if (original_src(sorted_idx(i)) == original_src(sorted_idx(i+1)) .and. &
                original_tgt(sorted_idx(i)) == original_tgt(sorted_idx(i+1))) then

                natural_duplicates = natural_duplicates + 1
            else
                ! If we reach here, the math has broken (or the code has a bug)
                false_collisions = false_collisions + 1
            end if
        end if
    end do

    write(*,*) "------------------------------------------"
    write(*,*) "Test Results for pack_pair:"
    write(*,*) "Total Samples:      ", N_SAMPLES
    write(*,*) "Natural Duplicates: ", natural_duplicates
    write(*,*) "FALSE Collisions:   ", false_collisions
    write(*,*) "------------------------------------------"

    if (false_collisions == 0) then
        write(*,*) "Conclusion: pack_pair is a Perfect Hash (Zero Collisions)."
    else
        error stop "Conclusion: Logic Error Detected!"
    end if

end program test_pack_perfection