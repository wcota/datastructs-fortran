program benchmark_sorting
    use datastructs_kinds_mod
    use datastructs_sorting_mod, only : native_sort => sort, native_sort_index => sort_index
    use stdlib_sorting, only : stdlib_sort => sort, stdlib_sort_index => sort_index
    use rndgen_mod
    implicit none

    type(rndgen) :: rng
    integer(kind=i4) :: seed = 42

    call rng%init(seed)

    write(*, '(/, A)') '========================================================================'
    write(*, '(A)')    ' BENCHMARK: datastructs-fortran (Native) vs stdlib_sorting (stdlib)'
    write(*, '(A)')    '========================================================================'

    call run_bench_all_sizes(rng)

contains

    subroutine run_bench_all_sizes(gen)
        type(rndgen), intent(inout) :: gen
        integer(kind=i4) :: sizes(5)
        integer(kind=i4) :: s_idx, n, reps

        sizes = [16, 100, 1000, 50000, 500000]

        do s_idx = 1, size(sizes)
            n = sizes(s_idx)
            if (n <= 100) then
                reps = 10000
            else if (n <= 1000) then
                reps = 1000
            else if (n <= 50000) then
                reps = 20
            else
                reps = 5
            end if

            write(*, '(/, A, I7, A, I6, A)') '--- ARRAY SIZE N = ', n, ' (Reps = ', reps, ') ---'
            call compare_direct_sort_i4(gen, n, reps)
            call compare_direct_sort_dp(gen, n, reps)
            call compare_index_sort_i4(gen, n, reps)
        end do
    end subroutine run_bench_all_sizes

    subroutine compare_direct_sort_i4(gen, n, reps)
        type(rndgen), intent(inout) :: gen
        integer(kind=i4), intent(in) :: n, reps
        integer(kind=i4), allocatable :: raw(:), a1(:), a2(:)
        real(kind=dp) :: t0, t1, t_native, t_stdlib
        integer(kind=i4) :: r, i

        allocate(raw(n), a1(n), a2(n))

        t_native = 0.0_dp
        t_stdlib = 0.0_dp

        do r = 1, reps
            do i = 1, n
                raw(i) = gen%int(-1000000, 1000000)
            end do

            a1 = raw
            call cpu_time(t0)
            call native_sort(a1)
            call cpu_time(t1)
            t_native = t_native + (t1 - t0)

            a2 = raw
            call cpu_time(t0)
            call stdlib_sort(a2)
            call cpu_time(t1)
            t_stdlib = t_stdlib + (t1 - t0)

            ! Verification
            if (any(a1 /= a2)) then
                error stop "Direct sort i4 verification mismatch!"
            end if
        end do

        write(*, '(A25, A12, F10.4, A6, A12, F10.4, A6, A10, F6.2, A1)') &
            'Direct Sort integer(i4):', &
            ' Native = ', t_native * 1000.0_dp, ' ms | ', &
            ' stdlib = ', t_stdlib * 1000.0_dp, ' ms | ', &
            ' Ratio = ', (t_native / max(t_stdlib, 1.0e-12_dp)), 'x'

        deallocate(raw, a1, a2)
    end subroutine compare_direct_sort_i4

    subroutine compare_direct_sort_dp(gen, n, reps)
        type(rndgen), intent(inout) :: gen
        integer(kind=i4), intent(in) :: n, reps
        real(kind=dp), allocatable :: raw(:), a1(:), a2(:)
        real(kind=dp) :: t0, t1, t_native, t_stdlib
        integer(kind=i4) :: r, i

        allocate(raw(n), a1(n), a2(n))

        t_native = 0.0_dp
        t_stdlib = 0.0_dp

        do r = 1, reps
            do i = 1, n
                raw(i) = gen%rnd()
            end do

            a1 = raw
            call cpu_time(t0)
            call native_sort(a1)
            call cpu_time(t1)
            t_native = t_native + (t1 - t0)

            a2 = raw
            call cpu_time(t0)
            call stdlib_sort(a2)
            call cpu_time(t1)
            t_stdlib = t_stdlib + (t1 - t0)

            ! Verification
            do i = 1, n - 1
                if (a1(i) > a1(i+1) .or. a2(i) > a2(i+1)) then
                    error stop "Direct sort dp not sorted!"
                end if
            end do
        end do

        write(*, '(A25, A12, F10.4, A6, A12, F10.4, A6, A10, F6.2, A1)') &
            'Direct Sort real(dp):', &
            ' Native = ', t_native * 1000.0_dp, ' ms | ', &
            ' stdlib = ', t_stdlib * 1000.0_dp, ' ms | ', &
            ' Ratio = ', (t_native / max(t_stdlib, 1.0e-12_dp)), 'x'

        deallocate(raw, a1, a2)
    end subroutine compare_direct_sort_dp

    subroutine compare_index_sort_i4(gen, n, reps)
        type(rndgen), intent(inout) :: gen
        integer(kind=i4), intent(in) :: n, reps
        integer(kind=i4), allocatable :: raw(:), a2(:)
        integer(kind=i4), allocatable :: idx1(:), idx2(:)
        real(kind=dp) :: t0, t1, t_native, t_stdlib
        integer(kind=i4) :: r, i

        allocate(raw(n), a2(n), idx2(n))

        t_native = 0.0_dp
        t_stdlib = 0.0_dp

        do r = 1, reps
            do i = 1, n
                raw(i) = gen%int(-1000000, 1000000)
            end do

            call cpu_time(t0)
            call native_sort_index(raw, idx1)
            call cpu_time(t1)
            t_native = t_native + (t1 - t0)

            a2 = raw
            call cpu_time(t0)
            call stdlib_sort_index(a2, idx2)
            call cpu_time(t1)
            t_stdlib = t_stdlib + (t1 - t0)

            ! Verification
            do i = 1, n - 1
                if (raw(idx1(i)) > raw(idx1(i+1)) .or. raw(idx2(i)) > raw(idx2(i+1))) then
                    error stop "Indexed sort not sorted!"
                end if
            end do
        end do

        write(*, '(A25, A12, F10.4, A6, A12, F10.4, A6, A10, F6.2, A1)') &
            'Index Sort integer(i4):', &
            ' Native = ', t_native * 1000.0_dp, ' ms | ', &
            ' stdlib = ', t_stdlib * 1000.0_dp, ' ms | ', &
            ' Ratio = ', (t_native / max(t_stdlib, 1.0e-12_dp)), 'x'

        deallocate(raw, a2, idx2)
        if (allocated(idx1)) deallocate(idx1)
    end subroutine compare_index_sort_i4

end program benchmark_sorting
