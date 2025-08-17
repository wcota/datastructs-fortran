!> This module implements a maxheap structure, keeping track of the maximum value and its index.
!> Example:
!> ```fortran
!> program example_maxheap
!>     use datastructs_fortran
!>     use kinds_mod
!>     implicit none
!>     type(maxheap_t) :: my_heap
!>     real(kind=dp), parameter :: list(*) = [40.0_dp,25.5_dp,30.1_dp,100.4_dp,2.0_dp]
!>     integer(kind=i4) :: i
!> 
!>     call my_heap%init(10)
!> 
!>     do i = 1, size(list)
!>         call my_heap%add(list(i), i)
!>     end do
!> 
!>     call my_heap%print()
!> 
!>     call my_heap%remove(4)
!> 
!>     call my_heap%print()
!> end program example_maxheap
!> ```
module maxheap_mod
    use kinds_mod
    implicit none
    private

    !> Constructor for a maxheap
    interface maxheap
        module procedure maxheap_new
    end interface maxheap

    !> Type for a maxheap
    type :: maxheap_t
        real(dp), allocatable :: values(:)     ! heap of values
        integer(i4), allocatable :: indices(:) ! their corresponding external indices
        integer(i4), allocatable :: pos_of(:)  ! map: index -> position in heap (0 if not present)
        integer(i4) :: n = 0                   ! current heap size
        integer(i4) :: max_n                   ! max capacity
    contains
        procedure :: init => maxheap_init ! initialize the maxheap
        procedure :: add => maxheap_add ! adds a new value to the heap
        procedure :: remove => maxheap_remove ! removes a value from the heap
        procedure :: add_weight => maxheap_add_weight ! adds a delta weight to an existing index
        procedure :: print => maxheap_print ! print the maxheap
        procedure :: max_value => maxheap_max_value ! returns the max value
        procedure :: max_index => maxheap_max_index ! returns the index of the max value

        final :: maxheap_finalize ! finalize the maxheap
    end type

    public :: maxheap, maxheap_t

contains

    !> Create a new maxheap
    !> Input: max_size - maximum number of elements in the heap
    function maxheap_new(max_size) result(this)
        type(maxheap_t) :: this
        integer(kind=i4) :: max_size

        call this%init(max_size)

    end function maxheap_new

    !> Initialize an empty maxheap
    !> Input: max_size - maximum number of elements in the heap
    subroutine maxheap_init(this, max_size)
        class(maxheap_t), intent(inout) :: this
        integer(i4), intent(in) :: max_size

        this%max_n = max_size
        this%n = 0
        allocate(this%values(max_size))
        allocate(this%indices(max_size))
        allocate(this%pos_of(max_size))
        this%pos_of = 0
    end subroutine maxheap_init

    !> Adds a new value to the maxheap, with a corresponding index
    !> Input: value - the value to add
    !>        index - the corresponding index of the value
    subroutine maxheap_add(this, value, index)
        class(maxheap_t), intent(inout) :: this
        real(dp), intent(in) :: value
        integer(i4), intent(in) :: index
        integer(i4) :: pos, parent

        if (this%n >= this%max_n) error stop "Heap full"
        if (index < 1 .or. index > this%max_n) error stop "Index out of bounds"
        if (this%pos_of(index) /= 0) error stop "Index already in heap"

        this%n = this%n + 1
        pos = this%n
        this%values(pos) = value
        this%indices(pos) = index
        this%pos_of(index) = pos

        ! bubble up
        do while (pos > 1)
            parent = pos / 2
            if (this%values(parent) >= this%values(pos)) exit

            call swap(this%values(parent), this%values(pos))
            call swap(this%indices(parent), this%indices(pos))
            this%pos_of(this%indices(parent)) = parent
            this%pos_of(this%indices(pos)) = pos

            pos = parent
        end do
    end subroutine maxheap_add

    !> Adds a delta weight to an existing index
    !> Input: delta_weight - the weight to add
    !>        index - the corresponding index of the value
    subroutine maxheap_add_weight(this, delta_weight, index)
        class(maxheap_t), intent(inout) :: this
        real(dp), intent(in) :: delta_weight
        integer(i4), intent(in) :: index
        real(dp) :: weight

        weight = this%values(this%pos_of(index)) + delta_weight

        ! first we remove it
        call this%remove(index)

        ! then we add the new weight
        call this%add(weight, index)
    end subroutine maxheap_add_weight

    !> Removes a value from the maxheap
    !> Input: index - the corresponding index of the value
    subroutine maxheap_remove(this, index)
        class(maxheap_t), intent(inout) :: this
        integer(i4), intent(in) :: index
        integer(i4) :: pos, last_index, left, right, largest

        if (index < 1 .or. index > this%max_n) return
        pos = this%pos_of(index)
        if (pos == 0) return ! not in heap, no problem

        last_index = this%indices(this%n)
        this%values(pos) = this%values(this%n)
        this%indices(pos) = last_index
        this%pos_of(last_index) = pos

        this%pos_of(index) = 0
        this%n = this%n - 1

        ! heapify down
        do
            left = 2 * pos
            right = 2 * pos + 1
            largest = pos

            if (left <= this%n) then
                if (this%values(left) > this%values(largest)) largest = left
            end if

            if (right <= this%n) then
                if (this%values(right) > this%values(largest)) largest = right
            end if

            if (largest == pos) exit

            call swap(this%values(pos), this%values(largest))
            call swap(this%indices(pos), this%indices(largest))
            this%pos_of(this%indices(pos)) = pos
            this%pos_of(this%indices(largest)) = largest

            pos = largest
        end do
    end subroutine maxheap_remove

    !> Gets the maximum value from the maxheap
    function maxheap_max_value(this) result(value)
        class(maxheap_t), intent(in) :: this
        real(dp) :: value
        if (this%n > 0) then
            value = this%values(1)
        else
            write(*,fmt_general) "Heap is empty"
            value = -huge(value)
        end if
    end function maxheap_max_value

    !> Gets the original index of the maximum value
    function maxheap_max_index(this) result(index)
        class(maxheap_t), intent(in) :: this
        integer(i4) :: index
        if (this%n > 0) then
            index = this%indices(1)
        else
            write(*,fmt_general) "Heap is empty"
            index = -1 ! or some other invalid value
        end if
    end function maxheap_max_index

    !> Prints the contents of the maxheap
    subroutine maxheap_print(this)
        class(maxheap_t), intent(in) :: this
        if (this%n > 0) then
            write(*,fmt_general) "Max value and corresponding index:", this%values(1), this%indices(1)
        else
            write(*,fmt_general) "Heap is empty"
        end if
    end subroutine maxheap_print

    !> Finalizes the maxheap
    subroutine maxheap_finalize(this)
        type(maxheap_t), intent(inout) :: this
        if (allocated(this%values)) deallocate(this%values)
        if (allocated(this%indices)) deallocate(this%indices)
        if (allocated(this%pos_of)) deallocate(this%pos_of)
    end subroutine maxheap_finalize

end module maxheap_mod