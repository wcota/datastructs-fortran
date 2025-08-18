!> This module implements a dynamical list, in which elements can be added or removed dynamically.
!> It assumes that there is a maximum size, and elements can be added or removed up to that size.
!> Example:
!> ```fortran
!> program test_dynamical_list
!>     use datastructs_fortran
!>     use kinds_mod
!>     implicit none
!>     type(dynamical_list_t) :: my_list
!>     integer(kind=i4) :: i
!>     call my_list%init(10)
!>     do i = 1, 10
!>         call my_list%add(i)
!>     end do
!>     call my_list%print()
!>     call my_list%remove(5)
!>     call my_list%print()
!> end program test_dynamical_list
!> ```
module datastructs_lists_dynamical_list_mod
    use datastructs_kinds_mod
    implicit none
    private

    !> Constructor for a dynamical list
    interface dynamical_list
        module procedure dynamical_list_new
    end interface dynamical_list

    !> List of integers with a given maximum fixed size
    !> The first index is always 1, and filled up to n_used
    !> It contains routines for managing the list
    type :: dynamical_list_t
        integer(kind=i4) :: n = 0
        integer(kind=i4) :: n_used = 0
        integer(kind=i4), allocatable :: list(:)
    contains
        procedure :: init => dynamical_list_init ! initialize the list
        procedure :: reset => dynamical_list_reset ! reset the list
        procedure :: print => dynamical_list_print ! print the list
        procedure :: expand => dynamical_list_expand ! expand the list to a new size
        procedure :: trim => dynamical_list_trim ! trim the list to n_used
        procedure :: last => dynamical_list_last ! get the last element
        procedure :: count => dynamical_list_count ! count the number of times an element appears in the list

        procedure, private :: dynamical_list_create_from_array
        procedure, private :: dynamical_list_add_element, dynamical_list_add_array
        procedure, private :: dynamical_list_remove_position, dynamical_list_remove_position_array, &
            dynamical_list_remove_position_range

        generic :: add => dynamical_list_add_element, dynamical_list_add_array ! add elements
        generic :: remove => dynamical_list_remove_position, dynamical_list_remove_position_array, &
            dynamical_list_remove_position_range ! remove indexes
        generic :: assignment(=) => dynamical_list_create_from_array ! create from array

        procedure :: get => dynamical_list_get ! get element at position

        procedure :: sum => dynamical_list_sum ! sum the elements

        final :: finalize_dynamical_list ! finalize the list
    end type dynamical_list_t

    public :: dynamical_list, dynamical_list_t

contains

    !> Create a new dynamical list
    !> Input: list - an array of integers
    !> Output: a new dynamical list
    function dynamical_list_new(list) result(this)
        type(dynamical_list_t) :: this
        integer(kind=i4), intent(in) :: list(:)

        ! allocate the dynamical list
        this%n = size(list)

        ! since we start with a full list, we set n_used to n
        this%n_used = size(list)

        ! copy the elements to the list
        this%list = list
    end function dynamical_list_new

    !> Finalize the list
    !> For that, we need to deallocate the list
    subroutine finalize_dynamical_list(this)
        type(dynamical_list_t), intent(inout) :: this
        if (allocated(this%list)) deallocate(this%list)
    end subroutine finalize_dynamical_list

    !> Initialize an empty list with a given size
    !> Input: n - the size of the list
    subroutine dynamical_list_init(this, n)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        this%n = n
        this%n_used = 0
        allocate(this%list(n))
    end subroutine dynamical_list_init

    !> Reset the list
    !> For that, we only need to set n_used to 0
    subroutine dynamical_list_reset(this)
        class(dynamical_list_t), intent(inout) :: this
        this%n_used = 0
    end subroutine dynamical_list_reset

    !> Trim the list to the number of used elements
    subroutine dynamical_list_trim(this)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), allocatable :: new_list(:)

        new_list = this%list(1:this%n_used)
        call move_alloc(new_list, this%list)
        this%n = this%n_used
    end subroutine dynamical_list_trim

    !> Expand the list to a new size
    subroutine dynamical_list_expand(this, new_size)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: new_size
        integer(kind=i4), allocatable :: new_list(:)

        if (new_size <= this%n) then
            stop 'Error: new size must be greater than current size'
        end if

        allocate(new_list(new_size))
        new_list(1:this%n_used) = this%list(1:this%n_used)
        call move_alloc(new_list, this%list)
        this%n = new_size

    end subroutine dynamical_list_expand

    !> Return the sum of the elements in the list
    !> It sums only the valid elements
    function dynamical_list_sum(this) result(val)
        class(dynamical_list_t), intent(in) :: this
        integer(kind=i4) :: val

        val = sum(this%list(1:this%n_used))

    end function dynamical_list_sum

    !> Returns the element at the given position
    !> Input: position - the position of the element to return
    function dynamical_list_get(this, position) result(val)
        class(dynamical_list_t), intent(in) :: this
        integer(kind=i4), intent(in) :: position
        integer(kind=i4) :: val

        val = this%list(position)

    end function dynamical_list_get

    !> Returns the last element of the list
    function dynamical_list_last(this) result(val)
        class(dynamical_list_t), intent(in) :: this
        integer(kind=i4) :: val

        val = dynamical_list_get(this, this%n_used)

    end function dynamical_list_last

    !> Count the number of times an element appears in the list
    function dynamical_list_count(this, element) result(cnt)
        class(dynamical_list_t), intent(in) :: this
        integer(kind=i4), intent(in) :: element
        integer(kind=i4) :: cnt

        cnt = count(this%list(1:this%n_used) == element)

    end function dynamical_list_count

    !> Add an element to the list
    !> It assumes that the list is already allocated
    !> The element is added to the end of the list
    !> Input: element - the element to add
    subroutine dynamical_list_add_element(this, element)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: element

        this%n_used = this%n_used + 1
        this%list(this%n_used) = element

    end subroutine dynamical_list_add_element

    !> Add an array to the list
    !> It will initialize the list if it is not already allocated, with the size of the array
    !> Input: array - the array to add
    subroutine dynamical_list_add_array(this, array)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: array(:)

        ! add the new array to the end of the list
        this%list(this%n_used+1:this%n_used+size(array)) = array
        ! add the size of the array to the number of used elements
        this%n_used = this%n_used + size(array)

    end subroutine dynamical_list_add_array

    !> Create from array, if it is not already allocated
    !> Input: array - the array to create the list from
    subroutine dynamical_list_create_from_array(this, array)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: array(:)

        if (.not. allocated(this%list)) then
            call this%init(size(array))
            call dynamical_list_add_array(this, array)
        else
            stop 'Error: list already allocated'
        end if

    end subroutine dynamical_list_create_from_array

    !> Remove an element from the list
    !> It will replace the element at the given position with the last element
    !> WARNING: This can change the position of the elements in the list, DO NOT use sequentially
    !>  with the original indexes, that are not updated after each removal
    !> Input: position - the position of the element to remove
    subroutine dynamical_list_remove_position(this, position)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: position

        ! replace the element at position with the last element
        this%list(position) = this%list(this%n_used)
        ! decrease the number of used elements
        this%n_used = this%n_used - 1

    end subroutine dynamical_list_remove_position

    !> Remove a range of elements from the list
    !> It will replace the elements in the range with the last elements
    !> This is safe because the elements are removed in reverse order
    !> Input: ini_position - the initial position of the range to remove
    !>        fin_position - the final position of the range to remove
    subroutine dynamical_list_remove_position_range(this, ini_position, fin_position)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: ini_position, fin_position
        integer(kind=i4) :: position_idx

        do position_idx = fin_position, ini_position, -1
            call dynamical_list_remove_position(this, position_idx)
        end do

    end subroutine dynamical_list_remove_position_range

    !> Remove a list of elements from the list
    !> It will replace the elements in the list with the last elements
    !> For that, we need to sort the positions
    !> It is safe because the elements are removed in reverse order
    !> Input: position_arr - the array of positions to remove
    !> Dependency: stdlib_sorting
    subroutine dynamical_list_remove_position_array(this, position_arr)
        use stdlib_sorting, only: sort
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: position_arr(:)
        integer(kind=i4), allocatable :: sorted_position_arr(:)
        integer(kind=i4) :: position_idx

        ! First, we need to sort the positions in descending order
        ! This is necessary to avoid changing the positions of the elements that are not yet removed
        sorted_position_arr = position_arr
        call sort(sorted_position_arr, reverse=.true.)

        do position_idx = 1, size(sorted_position_arr)
            call dynamical_list_remove_position(this, sorted_position_arr(position_idx))
        end do

    end subroutine dynamical_list_remove_position_array

    !> Print the list
    subroutine dynamical_list_print(this)
        class(dynamical_list_t), intent(in) :: this

        write(*,fmt_general) '(n = ', this%n, ', n_used = ', this%n_used, ')', this%list(1:this%n_used)

    end subroutine dynamical_list_print

end module datastructs_lists_dynamical_list_mod