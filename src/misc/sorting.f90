!> Internal high-performance sorting module for datastructs-fortran.
!> Provides in-place introsort (hybrid of median-of-3 quicksort, heapsort fallback,
!> and insertion sort) with guaranteed O(N log N) worst-case complexity and
!> O(1) auxiliary space, without requiring any external dependencies (such as stdlib).
module datastructs_sorting_mod
    use datastructs_kinds_mod
    implicit none
    private

    interface sort
        module procedure sort_i4
        module procedure sort_i8
        module procedure sort_dp
    end interface sort

    interface sort_index
        module procedure sort_index_i4
        module procedure sort_index_i8
        module procedure sort_index_dp
    end interface sort_index

    public :: sort, sort_index

    integer(kind=i4), parameter :: INSERTION_CUTOFF = 16

contains

    pure function calc_max_depth(n) result(res)
        integer(kind=i4), intent(in) :: n
        integer(kind=i4) :: res, temp
        res = 0
        temp = n
        do while (temp > 1)
            temp = temp / 2
            res = res + 1
        end do
        res = 2 * res
    end function calc_max_depth

    ! =========================================================================
    ! Direct In-Place Sort: integer(i4)
    ! =========================================================================
    subroutine sort_i4(arr, reverse)
        integer(kind=i4), intent(inout) :: arr(:)
        logical, intent(in), optional   :: reverse
        logical :: rev
        integer(kind=i4) :: n

        n = size(arr)
        if (n <= 1) return

        rev = .false.
        if (present(reverse)) rev = reverse

        call introsort_i4(arr, 1, n, calc_max_depth(n), rev)
    end subroutine sort_i4

    recursive subroutine introsort_i4(arr, left, right, depth_limit, rev)
        integer(kind=i4), intent(inout) :: arr(:)
        integer(kind=i4), intent(in)    :: left, right, depth_limit
        logical, intent(in)             :: rev
        integer(kind=i4) :: l, r, d, mid, pivot, i, j, temp

        l = left
        r = right
        d = depth_limit

        do while (r - l > INSERTION_CUTOFF)
            if (d == 0) then
                call heapsort_i4(arr(l:r), rev)
                return
            end if
            d = d - 1

            mid = l + (r - l) / 2

            ! Median-of-three
            if (.not. rev) then
                if (arr(l) > arr(mid)) then; temp = arr(l); arr(l) = arr(mid); arr(mid) = temp; end if
                if (arr(l) > arr(r)) then; temp = arr(l); arr(l) = arr(r); arr(r) = temp; end if
                if (arr(mid) > arr(r)) then; temp = arr(mid); arr(mid) = arr(r); arr(r) = temp; end if
            else
                if (arr(l) < arr(mid)) then; temp = arr(l); arr(l) = arr(mid); arr(mid) = temp; end if
                if (arr(l) < arr(r)) then; temp = arr(l); arr(l) = arr(r); arr(r) = temp; end if
                if (arr(mid) < arr(r)) then; temp = arr(mid); arr(mid) = arr(r); arr(r) = temp; end if
            end if

            pivot = arr(mid)
            i = l
            j = r

            do
                if (.not. rev) then
                    do while (arr(i) < pivot); i = i + 1; end do
                    do while (arr(j) > pivot); j = j - 1; end do
                else
                    do while (arr(i) > pivot); i = i + 1; end do
                    do while (arr(j) < pivot); j = j - 1; end do
                end if

                if (i >= j) exit
                temp = arr(i); arr(i) = arr(j); arr(j) = temp
                i = i + 1
                j = j - 1
            end do

            if (j - l < r - i) then
                if (l < j) call introsort_i4(arr, l, j, d, rev)
                l = i
            else
                if (i < r) call introsort_i4(arr, i, r, d, rev)
                r = j
            end if
        end do

        if (r > l) call insertion_sort_i4(arr, l, r, rev)
    end subroutine introsort_i4

    subroutine insertion_sort_i4(arr, left, right, rev)
        integer(kind=i4), intent(inout) :: arr(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: i, j, key

        do i = left + 1, right
            key = arr(i)
            j = i - 1
            do while (j >= left)
                if (.not. rev) then
                    if (arr(j) <= key) exit
                else
                    if (arr(j) >= key) exit
                end if
                arr(j + 1) = arr(j)
                j = j - 1
            end do
            arr(j + 1) = key
        end do
    end subroutine insertion_sort_i4

    subroutine heapsort_i4(arr, rev)
        integer(kind=i4), intent(inout) :: arr(:)
        logical, intent(in)             :: rev
        integer(kind=i4) :: n, start, end

        n = size(arr)
        if (n <= 1) return

        do start = n / 2, 1, -1
            call sift_down_i4(arr, start, n, rev)
        end do

        do end = n, 2, -1
            call swap_i4(arr(1), arr(end))
            call sift_down_i4(arr, 1, end - 1, rev)
        end do
    end subroutine heapsort_i4

    subroutine sift_down_i4(arr, start, end, rev)
        integer(kind=i4), intent(inout) :: arr(:)
        integer(kind=i4), intent(in)    :: start, end
        logical, intent(in)             :: rev
        integer(kind=i4) :: root, child, swap_pos

        root = start
        do while (2 * root <= end)
            child = 2 * root
            swap_pos = root

            if (.not. rev) then
                if (arr(swap_pos) < arr(child)) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(swap_pos) < arr(child + 1)) swap_pos = child + 1
                end if
            else
                if (arr(swap_pos) > arr(child)) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(swap_pos) > arr(child + 1)) swap_pos = child + 1
                end if
            end if

            if (swap_pos == root) return
            call swap_i4(arr(root), arr(swap_pos))
            root = swap_pos
        end do
    end subroutine sift_down_i4

    elemental subroutine swap_i4(a, b)
        integer(kind=i4), intent(inout) :: a, b
        integer(kind=i4) :: temp
        temp = a; a = b; b = temp
    end subroutine swap_i4

    ! =========================================================================
    ! Direct In-Place Sort: integer(i8)
    ! =========================================================================
    subroutine sort_i8(arr, reverse)
        integer(kind=i8), intent(inout) :: arr(:)
        logical, intent(in), optional   :: reverse
        logical :: rev
        integer(kind=i4) :: n

        n = size(arr)
        if (n <= 1) return

        rev = .false.
        if (present(reverse)) rev = reverse

        call introsort_i8(arr, 1, n, calc_max_depth(n), rev)
    end subroutine sort_i8

    recursive subroutine introsort_i8(arr, left, right, depth_limit, rev)
        integer(kind=i8), intent(inout) :: arr(:)
        integer(kind=i4), intent(in)    :: left, right, depth_limit
        logical, intent(in)             :: rev
        integer(kind=i4) :: l, r, d, mid, i, j
        integer(kind=i8) :: pivot, temp

        l = left
        r = right
        d = depth_limit

        do while (r - l > INSERTION_CUTOFF)
            if (d == 0) then
                call heapsort_i8(arr(l:r), rev)
                return
            end if
            d = d - 1

            mid = l + (r - l) / 2

            if (.not. rev) then
                if (arr(l) > arr(mid)) then; temp = arr(l); arr(l) = arr(mid); arr(mid) = temp; end if
                if (arr(l) > arr(r)) then; temp = arr(l); arr(l) = arr(r); arr(r) = temp; end if
                if (arr(mid) > arr(r)) then; temp = arr(mid); arr(mid) = arr(r); arr(r) = temp; end if
            else
                if (arr(l) < arr(mid)) then; temp = arr(l); arr(l) = arr(mid); arr(mid) = temp; end if
                if (arr(l) < arr(r)) then; temp = arr(l); arr(l) = arr(r); arr(r) = temp; end if
                if (arr(mid) < arr(r)) then; temp = arr(mid); arr(mid) = arr(r); arr(r) = temp; end if
            end if

            pivot = arr(mid)
            i = l
            j = r

            do
                if (.not. rev) then
                    do while (arr(i) < pivot); i = i + 1; end do
                    do while (arr(j) > pivot); j = j - 1; end do
                else
                    do while (arr(i) > pivot); i = i + 1; end do
                    do while (arr(j) < pivot); j = j - 1; end do
                end if

                if (i >= j) exit
                temp = arr(i); arr(i) = arr(j); arr(j) = temp
                i = i + 1
                j = j - 1
            end do

            if (j - l < r - i) then
                if (l < j) call introsort_i8(arr, l, j, d, rev)
                l = i
            else
                if (i < r) call introsort_i8(arr, i, r, d, rev)
                r = j
            end if
        end do

        if (r > l) call insertion_sort_i8(arr, l, r, rev)
    end subroutine introsort_i8

    subroutine insertion_sort_i8(arr, left, right, rev)
        integer(kind=i8), intent(inout) :: arr(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: i, j
        integer(kind=i8) :: key

        do i = left + 1, right
            key = arr(i)
            j = i - 1
            do while (j >= left)
                if (.not. rev) then
                    if (arr(j) <= key) exit
                else
                    if (arr(j) >= key) exit
                end if
                arr(j + 1) = arr(j)
                j = j - 1
            end do
            arr(j + 1) = key
        end do
    end subroutine insertion_sort_i8

    subroutine heapsort_i8(arr, rev)
        integer(kind=i8), intent(inout) :: arr(:)
        logical, intent(in)             :: rev
        integer(kind=i4) :: n, start, end

        n = size(arr)
        if (n <= 1) return

        do start = n / 2, 1, -1
            call sift_down_i8(arr, start, n, rev)
        end do

        do end = n, 2, -1
            call swap_i8(arr(1), arr(end))
            call sift_down_i8(arr, 1, end - 1, rev)
        end do
    end subroutine heapsort_i8

    subroutine sift_down_i8(arr, start, end, rev)
        integer(kind=i8), intent(inout) :: arr(:)
        integer(kind=i4), intent(in)    :: start, end
        logical, intent(in)             :: rev
        integer(kind=i4) :: root, child, swap_pos

        root = start
        do while (2 * root <= end)
            child = 2 * root
            swap_pos = root

            if (.not. rev) then
                if (arr(swap_pos) < arr(child)) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(swap_pos) < arr(child + 1)) swap_pos = child + 1
                end if
            else
                if (arr(swap_pos) > arr(child)) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(swap_pos) > arr(child + 1)) swap_pos = child + 1
                end if
            end if

            if (swap_pos == root) return
            call swap_i8(arr(root), arr(swap_pos))
            root = swap_pos
        end do
    end subroutine sift_down_i8

    elemental subroutine swap_i8(a, b)
        integer(kind=i8), intent(inout) :: a, b
        integer(kind=i8) :: temp
        temp = a; a = b; b = temp
    end subroutine swap_i8

    ! =========================================================================
    ! Direct In-Place Sort: real(dp)
    ! =========================================================================
    subroutine sort_dp(arr, reverse)
        real(kind=dp), intent(inout)  :: arr(:)
        logical, intent(in), optional :: reverse
        logical :: rev
        integer(kind=i4) :: n

        n = size(arr)
        if (n <= 1) return

        rev = .false.
        if (present(reverse)) rev = reverse

        call introsort_dp(arr, 1, n, calc_max_depth(n), rev)
    end subroutine sort_dp

    recursive subroutine introsort_dp(arr, left, right, depth_limit, rev)
        real(kind=dp), intent(inout) :: arr(:)
        integer(kind=i4), intent(in) :: left, right, depth_limit
        logical, intent(in)          :: rev
        integer(kind=i4) :: l, r, d, mid, i, j
        real(kind=dp)    :: pivot, temp

        l = left
        r = right
        d = depth_limit

        do while (r - l > INSERTION_CUTOFF)
            if (d == 0) then
                call heapsort_dp(arr(l:r), rev)
                return
            end if
            d = d - 1

            mid = l + (r - l) / 2

            if (.not. rev) then
                if (arr(l) > arr(mid)) then; temp = arr(l); arr(l) = arr(mid); arr(mid) = temp; end if
                if (arr(l) > arr(r)) then; temp = arr(l); arr(l) = arr(r); arr(r) = temp; end if
                if (arr(mid) > arr(r)) then; temp = arr(mid); arr(mid) = arr(r); arr(r) = temp; end if
            else
                if (arr(l) < arr(mid)) then; temp = arr(l); arr(l) = arr(mid); arr(mid) = temp; end if
                if (arr(l) < arr(r)) then; temp = arr(l); arr(l) = arr(r); arr(r) = temp; end if
                if (arr(mid) < arr(r)) then; temp = arr(mid); arr(mid) = arr(r); arr(r) = temp; end if
            end if

            pivot = arr(mid)
            i = l
            j = r

            do
                if (.not. rev) then
                    do while (arr(i) < pivot); i = i + 1; end do
                    do while (arr(j) > pivot); j = j - 1; end do
                else
                    do while (arr(i) > pivot); i = i + 1; end do
                    do while (arr(j) < pivot); j = j - 1; end do
                end if

                if (i >= j) exit
                temp = arr(i); arr(i) = arr(j); arr(j) = temp
                i = i + 1
                j = j - 1
            end do

            if (j - l < r - i) then
                if (l < j) call introsort_dp(arr, l, j, d, rev)
                l = i
            else
                if (i < r) call introsort_dp(arr, i, r, d, rev)
                r = j
            end if
        end do

        if (r > l) call insertion_sort_dp(arr, l, r, rev)
    end subroutine introsort_dp

    subroutine insertion_sort_dp(arr, left, right, rev)
        real(kind=dp), intent(inout) :: arr(:)
        integer(kind=i4), intent(in) :: left, right
        logical, intent(in)          :: rev
        integer(kind=i4) :: i, j
        real(kind=dp)    :: key

        do i = left + 1, right
            key = arr(i)
            j = i - 1
            do while (j >= left)
                if (.not. rev) then
                    if (arr(j) <= key) exit
                else
                    if (arr(j) >= key) exit
                end if
                arr(j + 1) = arr(j)
                j = j - 1
            end do
            arr(j + 1) = key
        end do
    end subroutine insertion_sort_dp

    subroutine heapsort_dp(arr, rev)
        real(kind=dp), intent(inout) :: arr(:)
        logical, intent(in)          :: rev
        integer(kind=i4) :: n, start, end

        n = size(arr)
        if (n <= 1) return

        do start = n / 2, 1, -1
            call sift_down_dp(arr, start, n, rev)
        end do

        do end = n, 2, -1
            call swap_dp(arr(1), arr(end))
            call sift_down_dp(arr, 1, end - 1, rev)
        end do
    end subroutine heapsort_dp

    subroutine sift_down_dp(arr, start, end, rev)
        real(kind=dp), intent(inout) :: arr(:)
        integer(kind=i4), intent(in) :: start, end
        logical, intent(in)          :: rev
        integer(kind=i4) :: root, child, swap_pos

        root = start
        do while (2 * root <= end)
            child = 2 * root
            swap_pos = root

            if (.not. rev) then
                if (arr(swap_pos) < arr(child)) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(swap_pos) < arr(child + 1)) swap_pos = child + 1
                end if
            else
                if (arr(swap_pos) > arr(child)) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(swap_pos) > arr(child + 1)) swap_pos = child + 1
                end if
            end if

            if (swap_pos == root) return
            call swap_dp(arr(root), arr(swap_pos))
            root = swap_pos
        end do
    end subroutine sift_down_dp

    elemental subroutine swap_dp(a, b)
        real(kind=dp), intent(inout) :: a, b
        real(dp) :: temp
        temp = a; a = b; b = temp
    end subroutine swap_dp

    ! =========================================================================
    ! Indexed Sort: integer(i4)
    ! =========================================================================
    subroutine sort_index_i4(arr, idx, reverse)
        integer(kind=i4), intent(in)            :: arr(:)
        integer(kind=i4), allocatable, intent(inout) :: idx(:)
        logical, intent(in), optional           :: reverse
        logical :: rev
        integer(kind=i4) :: n, i

        n = size(arr)
        if (allocated(idx)) then
            if (size(idx) /= n) then
                deallocate(idx)
                allocate(idx(n))
            end if
        else
            allocate(idx(n))
        end if

        if (n == 0) return
        do i = 1, n
            idx(i) = i
        end do
        if (n == 1) return

        rev = .false.
        if (present(reverse)) rev = reverse

        call introsort_idx_i4(arr, idx, 1, n, calc_max_depth(n), rev)
    end subroutine sort_index_i4

    recursive subroutine introsort_idx_i4(arr, idx, left, right, depth_limit, rev)
        integer(kind=i4), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right, depth_limit
        logical, intent(in)             :: rev
        integer(kind=i4) :: l, r, d, mid, i, j, pivot_val, temp

        l = left
        r = right
        d = depth_limit

        do while (r - l > INSERTION_CUTOFF)
            if (d == 0) then
                call heapsort_idx_range_i4(arr, idx, l, r, rev)
                return
            end if
            d = d - 1

            mid = l + (r - l) / 2

            if (.not. rev) then
                if (arr(idx(l)) > arr(idx(mid))) then; temp = idx(l); idx(l) = idx(mid); idx(mid) = temp; end if
                if (arr(idx(l)) > arr(idx(r))) then; temp = idx(l); idx(l) = idx(r); idx(r) = temp; end if
                if (arr(idx(mid)) > arr(idx(r))) then; temp = idx(mid); idx(mid) = idx(r); idx(r) = temp; end if
            else
                if (arr(idx(l)) < arr(idx(mid))) then; temp = idx(l); idx(l) = idx(mid); idx(mid) = temp; end if
                if (arr(idx(l)) < arr(idx(r))) then; temp = idx(l); idx(l) = idx(r); idx(r) = temp; end if
                if (arr(idx(mid)) < arr(idx(r))) then; temp = idx(mid); idx(mid) = idx(r); idx(r) = temp; end if
            end if

            pivot_val = arr(idx(mid))
            i = l
            j = r

            do
                if (.not. rev) then
                    do while (arr(idx(i)) < pivot_val); i = i + 1; end do
                    do while (arr(idx(j)) > pivot_val); j = j - 1; end do
                else
                    do while (arr(idx(i)) > pivot_val); i = i + 1; end do
                    do while (arr(idx(j)) < pivot_val); j = j - 1; end do
                end if

                if (i >= j) exit
                temp = idx(i); idx(i) = idx(j); idx(j) = temp
                i = i + 1
                j = j - 1
            end do

            if (j - l < r - i) then
                if (l < j) call introsort_idx_i4(arr, idx, l, j, d, rev)
                l = i
            else
                if (i < r) call introsort_idx_i4(arr, idx, i, r, d, rev)
                r = j
            end if
        end do

        if (r > l) call insertion_sort_idx_i4(arr, idx, l, r, rev)
    end subroutine introsort_idx_i4

    subroutine insertion_sort_idx_i4(arr, idx, left, right, rev)
        integer(kind=i4), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: i, j, key_idx, key_val

        do i = left + 1, right
            key_idx = idx(i)
            key_val = arr(key_idx)
            j = i - 1
            do while (j >= left)
                if (.not. rev) then
                    if (arr(idx(j)) <= key_val) exit
                else
                    if (arr(idx(j)) >= key_val) exit
                end if
                idx(j + 1) = idx(j)
                j = j - 1
            end do
            idx(j + 1) = key_idx
        end do
    end subroutine insertion_sort_idx_i4

    subroutine heapsort_idx_range_i4(arr, idx, left, right, rev)
        integer(kind=i4), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: n, start, end, offset

        n = right - left + 1
        offset = left - 1

        do start = n / 2, 1, -1
            call sift_down_idx_range_i4(arr, idx, start, n, offset, rev)
        end do

        do end = n, 2, -1
            call swap_i4(idx(offset + 1), idx(offset + end))
            call sift_down_idx_range_i4(arr, idx, 1, end - 1, offset, rev)
        end do
    end subroutine heapsort_idx_range_i4

    subroutine sift_down_idx_range_i4(arr, idx, start, end, offset, rev)
        integer(kind=i4), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: start, end, offset
        logical, intent(in)             :: rev
        integer(kind=i4) :: root, child, swap_pos

        root = start
        do while (2 * root <= end)
            child = 2 * root
            swap_pos = root

            if (.not. rev) then
                if (arr(idx(offset + swap_pos)) < arr(idx(offset + child))) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(idx(offset + swap_pos)) < arr(idx(offset + child + 1))) swap_pos = child + 1
                end if
            else
                if (arr(idx(offset + swap_pos)) > arr(idx(offset + child))) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(idx(offset + swap_pos)) > arr(idx(offset + child + 1))) swap_pos = child + 1
                end if
            end if

            if (swap_pos == root) return
            call swap_i4(idx(offset + root), idx(offset + swap_pos))
            root = swap_pos
        end do
    end subroutine sift_down_idx_range_i4

    ! =========================================================================
    ! Indexed Sort: integer(i8)
    ! =========================================================================
    subroutine sort_index_i8(arr, idx, reverse)
        integer(kind=i8), intent(in)            :: arr(:)
        integer(kind=i4), allocatable, intent(inout) :: idx(:)
        logical, intent(in), optional           :: reverse
        logical :: rev
        integer(kind=i4) :: n, i

        n = size(arr)
        if (allocated(idx)) then
            if (size(idx) /= n) then
                deallocate(idx)
                allocate(idx(n))
            end if
        else
            allocate(idx(n))
        end if

        if (n == 0) return
        do i = 1, n
            idx(i) = i
        end do
        if (n == 1) return

        rev = .false.
        if (present(reverse)) rev = reverse

        call introsort_idx_i8(arr, idx, 1, n, calc_max_depth(n), rev)
    end subroutine sort_index_i8

    recursive subroutine introsort_idx_i8(arr, idx, left, right, depth_limit, rev)
        integer(kind=i8), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right, depth_limit
        logical, intent(in)             :: rev
        integer(kind=i4) :: l, r, d, mid, i, j, temp
        integer(kind=i8) :: pivot_val

        l = left
        r = right
        d = depth_limit

        do while (r - l > INSERTION_CUTOFF)
            if (d == 0) then
                call heapsort_idx_range_i8(arr, idx, l, r, rev)
                return
            end if
            d = d - 1

            mid = l + (r - l) / 2

            if (.not. rev) then
                if (arr(idx(l)) > arr(idx(mid))) then; temp = idx(l); idx(l) = idx(mid); idx(mid) = temp; end if
                if (arr(idx(l)) > arr(idx(r))) then; temp = idx(l); idx(l) = idx(r); idx(r) = temp; end if
                if (arr(idx(mid)) > arr(idx(r))) then; temp = idx(mid); idx(mid) = idx(r); idx(r) = temp; end if
            else
                if (arr(idx(l)) < arr(idx(mid))) then; temp = idx(l); idx(l) = idx(mid); idx(mid) = temp; end if
                if (arr(idx(l)) < arr(idx(r))) then; temp = idx(l); idx(l) = idx(r); idx(r) = temp; end if
                if (arr(idx(mid)) < arr(idx(r))) then; temp = idx(mid); idx(mid) = idx(r); idx(r) = temp; end if
            end if

            pivot_val = arr(idx(mid))
            i = l
            j = r

            do
                if (.not. rev) then
                    do while (arr(idx(i)) < pivot_val); i = i + 1; end do
                    do while (arr(idx(j)) > pivot_val); j = j - 1; end do
                else
                    do while (arr(idx(i)) > pivot_val); i = i + 1; end do
                    do while (arr(idx(j)) < pivot_val); j = j - 1; end do
                end if

                if (i >= j) exit
                temp = idx(i); idx(i) = idx(j); idx(j) = temp
                i = i + 1
                j = j - 1
            end do

            if (j - l < r - i) then
                if (l < j) call introsort_idx_i8(arr, idx, l, j, d, rev)
                l = i
            else
                if (i < r) call introsort_idx_i8(arr, idx, i, r, d, rev)
                r = j
            end if
        end do

        if (r > l) call insertion_sort_idx_i8(arr, idx, l, r, rev)
    end subroutine introsort_idx_i8

    subroutine insertion_sort_idx_i8(arr, idx, left, right, rev)
        integer(kind=i8), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: i, j, key_idx
        integer(kind=i8) :: key_val

        do i = left + 1, right
            key_idx = idx(i)
            key_val = arr(key_idx)
            j = i - 1
            do while (j >= left)
                if (.not. rev) then
                    if (arr(idx(j)) <= key_val) exit
                else
                    if (arr(idx(j)) >= key_val) exit
                end if
                idx(j + 1) = idx(j)
                j = j - 1
            end do
            idx(j + 1) = key_idx
        end do
    end subroutine insertion_sort_idx_i8

    subroutine heapsort_idx_range_i8(arr, idx, left, right, rev)
        integer(kind=i8), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: n, start, end, offset

        n = right - left + 1
        offset = left - 1

        do start = n / 2, 1, -1
            call sift_down_idx_range_i8(arr, idx, start, n, offset, rev)
        end do

        do end = n, 2, -1
            call swap_i4(idx(offset + 1), idx(offset + end))
            call sift_down_idx_range_i8(arr, idx, 1, end - 1, offset, rev)
        end do
    end subroutine heapsort_idx_range_i8

    subroutine sift_down_idx_range_i8(arr, idx, start, end, offset, rev)
        integer(kind=i8), intent(in)    :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: start, end, offset
        logical, intent(in)             :: rev
        integer(kind=i4) :: root, child, swap_pos

        root = start
        do while (2 * root <= end)
            child = 2 * root
            swap_pos = root

            if (.not. rev) then
                if (arr(idx(offset + swap_pos)) < arr(idx(offset + child))) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(idx(offset + swap_pos)) < arr(idx(offset + child + 1))) swap_pos = child + 1
                end if
            else
                if (arr(idx(offset + swap_pos)) > arr(idx(offset + child))) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(idx(offset + swap_pos)) > arr(idx(offset + child + 1))) swap_pos = child + 1
                end if
            end if

            if (swap_pos == root) return
            call swap_i4(idx(offset + root), idx(offset + swap_pos))
            root = swap_pos
        end do
    end subroutine sift_down_idx_range_i8

    ! =========================================================================
    ! Indexed Sort: real(dp)
    ! =========================================================================
    subroutine sort_index_dp(arr, idx, reverse)
        real(kind=dp), intent(in)               :: arr(:)
        integer(kind=i4), allocatable, intent(inout) :: idx(:)
        logical, intent(in), optional           :: reverse
        logical :: rev
        integer(kind=i4) :: n, i

        n = size(arr)
        if (allocated(idx)) then
            if (size(idx) /= n) then
                deallocate(idx)
                allocate(idx(n))
            end if
        else
            allocate(idx(n))
        end if

        if (n == 0) return
        do i = 1, n
            idx(i) = i
        end do
        if (n == 1) return

        rev = .false.
        if (present(reverse)) rev = reverse

        call introsort_idx_dp(arr, idx, 1, n, calc_max_depth(n), rev)
    end subroutine sort_index_dp

    recursive subroutine introsort_idx_dp(arr, idx, left, right, depth_limit, rev)
        real(kind=dp), intent(in)       :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right, depth_limit
        logical, intent(in)             :: rev
        integer(kind=i4) :: l, r, d, mid, i, j, temp
        real(kind=dp)    :: pivot_val

        l = left
        r = right
        d = depth_limit

        do while (r - l > INSERTION_CUTOFF)
            if (d == 0) then
                call heapsort_idx_range_dp(arr, idx, l, r, rev)
                return
            end if
            d = d - 1

            mid = l + (r - l) / 2

            if (.not. rev) then
                if (arr(idx(l)) > arr(idx(mid))) then; temp = idx(l); idx(l) = idx(mid); idx(mid) = temp; end if
                if (arr(idx(l)) > arr(idx(r))) then; temp = idx(l); idx(l) = idx(r); idx(r) = temp; end if
                if (arr(idx(mid)) > arr(idx(r))) then; temp = idx(mid); idx(mid) = idx(r); idx(r) = temp; end if
            else
                if (arr(idx(l)) < arr(idx(mid))) then; temp = idx(l); idx(l) = idx(mid); idx(mid) = temp; end if
                if (arr(idx(l)) < arr(idx(r))) then; temp = idx(l); idx(l) = idx(r); idx(r) = temp; end if
                if (arr(idx(mid)) < arr(idx(r))) then; temp = idx(mid); idx(mid) = idx(r); idx(r) = temp; end if
            end if

            pivot_val = arr(idx(mid))
            i = l
            j = r

            do
                if (.not. rev) then
                    do while (arr(idx(i)) < pivot_val); i = i + 1; end do
                    do while (arr(idx(j)) > pivot_val); j = j - 1; end do
                else
                    do while (arr(idx(i)) > pivot_val); i = i + 1; end do
                    do while (arr(idx(j)) < pivot_val); j = j - 1; end do
                end if

                if (i >= j) exit
                temp = idx(i); idx(i) = idx(j); idx(j) = temp
                i = i + 1
                j = j - 1
            end do

            if (j - l < r - i) then
                if (l < j) call introsort_idx_dp(arr, idx, l, j, d, rev)
                l = i
            else
                if (i < r) call introsort_idx_dp(arr, idx, i, r, d, rev)
                r = j
            end if
        end do

        if (r > l) call insertion_sort_idx_dp(arr, idx, l, r, rev)
    end subroutine introsort_idx_dp

    subroutine insertion_sort_idx_dp(arr, idx, left, right, rev)
        real(kind=dp), intent(in)       :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: i, j, key_idx
        real(kind=dp)    :: key_val

        do i = left + 1, right
            key_idx = idx(i)
            key_val = arr(key_idx)
            j = i - 1
            do while (j >= left)
                if (.not. rev) then
                    if (arr(idx(j)) <= key_val) exit
                else
                    if (arr(idx(j)) >= key_val) exit
                end if
                idx(j + 1) = idx(j)
                j = j - 1
            end do
            idx(j + 1) = key_idx
        end do
    end subroutine insertion_sort_idx_dp

    subroutine heapsort_idx_range_dp(arr, idx, left, right, rev)
        real(kind=dp), intent(in)       :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: left, right
        logical, intent(in)             :: rev
        integer(kind=i4) :: n, start, end, offset

        n = right - left + 1
        offset = left - 1

        do start = n / 2, 1, -1
            call sift_down_idx_range_dp(arr, idx, start, n, offset, rev)
        end do

        do end = n, 2, -1
            call swap_i4(idx(offset + 1), idx(offset + end))
            call sift_down_idx_range_dp(arr, idx, 1, end - 1, offset, rev)
        end do
    end subroutine heapsort_idx_range_dp

    subroutine sift_down_idx_range_dp(arr, idx, start, end, offset, rev)
        real(kind=dp), intent(in)       :: arr(:)
        integer(kind=i4), intent(inout) :: idx(:)
        integer(kind=i4), intent(in)    :: start, end, offset
        logical, intent(in)             :: rev
        integer(kind=i4) :: root, child, swap_pos

        root = start
        do while (2 * root <= end)
            child = 2 * root
            swap_pos = root

            if (.not. rev) then
                if (arr(idx(offset + swap_pos)) < arr(idx(offset + child))) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(idx(offset + swap_pos)) < arr(idx(offset + child + 1))) swap_pos = child + 1
                end if
            else
                if (arr(idx(offset + swap_pos)) > arr(idx(offset + child))) swap_pos = child
                if (child + 1 <= end) then
                    if (arr(idx(offset + swap_pos)) > arr(idx(offset + child + 1))) swap_pos = child + 1
                end if
            end if

            if (swap_pos == root) return
            call swap_i4(idx(offset + root), idx(offset + swap_pos))
            root = swap_pos
        end do
    end subroutine sift_down_idx_range_dp

end module datastructs_sorting_mod
