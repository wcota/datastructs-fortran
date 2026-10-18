program test_range_check
    use datastructs_mod, only: dynamical_list_t
    use datastructs_kinds_mod
    implicit none

    type(dynamical_list_t) :: dl
    integer(kind=i4) :: n, i, j, k, idx
    integer(kind=i4), allocatable :: orig(:), expected(:), remaining(:)
    integer(kind=i4), allocatable :: pos_arr(:)
    integer(kind=i4) :: count_tests, failed_tests

    count_tests = 0
    failed_tests = 0

    ! -------------------------------------------------------------
    ! 1. Exhaustive test of all ranges (i, j) for N from 1 to 30
    ! -------------------------------------------------------------
    do n = 1, 30
        allocate(orig(n))
        do i = 1, n
            orig(i) = i * 10
        end do

        do i = 1, n
            do j = i, n
                count_tests = count_tests + 1
                call dl%init(n)
                do k = 1, n
                    call dl%add(orig(k))
                end do

                ! Expected: elements outside [i, j]
                allocate(expected(n - (j - i + 1)))
                idx = 0
                do k = 1, i - 1
                    idx = idx + 1
                    expected(idx) = orig(k)
                end do
                do k = j + 1, n
                    idx = idx + 1
                    expected(idx) = orig(k)
                end do

                ! Remove range [i, j]
                call dl%remove(i, j)

                if (dl%n_used /= size(expected)) then
                    print *, "FAIL size: n=", n, " range=[", i, ",", j, "]"
                    failed_tests = failed_tests + 1
                else
                    allocate(remaining(dl%n_used))
                    remaining = dl%list(1:dl%n_used)
                    call sort_int(remaining)
                    call sort_int(expected)
                    if (any(remaining /= expected)) then
                        print *, "FAIL content: n=", n, " range=[", i, ",", j, "]"
                        failed_tests = failed_tests + 1
                    end if
                    deallocate(remaining)
                end if
                deallocate(expected)
                call dl%reset()
            end do
        end do
        deallocate(orig)
    end do

    print *, "Range tests (all i<=j up to N=30):", count_tests, "Passed:", count_tests - failed_tests

    ! -------------------------------------------------------------
    ! 2. Test remove array of positions: [1, 3, 5], [2, 4], etc.
    ! -------------------------------------------------------------
    call dl%init(10)
    do k = 1, 10
        call dl%add(k * 100)
    end do
    ! Remove positions 2, 5, 8 (values 200, 500, 800)
    call dl%remove([2, 5, 8])
    if (dl%n_used /= 7) then
        print *, "FAIL array size"
        failed_tests = failed_tests + 1
    end if
    allocate(remaining(dl%n_used))
    remaining = dl%list(1:dl%n_used)
    call sort_int(remaining)
    expected = [100, 300, 400, 600, 700, 900, 1000]
    if (any(remaining /= expected)) then
        print *, "FAIL array content: ", remaining
        failed_tests = failed_tests + 1
    else
        print *, "Array removal test [2, 5, 8] passed!"
    end if
    deallocate(remaining)
    deallocate(expected)

    if (failed_tests == 0) then
        print *, "=== ALL DYNAMICAL_LIST REMOVAL TESTS PASSED SUCCESSFULLY! ==="
    else
        error stop "Some dynamical_list tests failed!"
    end if

contains
    subroutine sort_int(arr)
        integer(kind=i4), intent(inout) :: arr(:)
        integer(kind=i4) :: p, q, temp
        do p = 1, size(arr) - 1
            do q = p + 1, size(arr)
                if (arr(p) > arr(q)) then
                    temp = arr(p)
                    arr(p) = arr(q)
                    arr(q) = temp
                end if
            end do
        end do
    end subroutine sort_int
end program test_range_check
