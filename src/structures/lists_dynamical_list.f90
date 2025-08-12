module hOGA_lists_dynamical_list_mod
    use hOGA_kinds_mod
    implicit none
    private
    
    interface dynamical_list
        module procedure dynamical_list_new
    end interface

    !> List of integers with a given fixed size
    !> The first index is always 1, and filled up to n_used
    type :: dynamical_list_t
        integer(kind=i4) :: n
        integer(kind=i4) :: n_used
        integer(kind=i4), allocatable :: list(:)
        ! TODO: add an aditional real array, maybe in an extended object structure
        ! use case: the list contains the indexes of a given array, and the corresponding values in the real list
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
    end type

    public :: dynamical_list, dynamical_list_t

contains

    !> Create a new dynamical list
    function dynamical_list_new(list) result(this)
        type(dynamical_list_t) :: this
        integer(kind=i4), intent(in) :: list(:)

        ! allocate the dynamical list
        this%n = size(list)
        this%n_used = size(list)

        ! copy the list
        this%list = list
    end function dynamical_list_new

    !> Finalize the list
    subroutine finalize_dynamical_list(this)
        type(dynamical_list_t), intent(inout) :: this
        if (allocated(this%list)) deallocate(this%list)
    end subroutine finalize_dynamical_list

    !> Initialize the list with a given size
    subroutine dynamical_list_init(this, n)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: n
        this%n = n
        this%n_used = 0
        allocate(this%list(n))
    end subroutine dynamical_list_init

    !> Reset the list
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

    !> Sum the valid elements of the list
    function dynamical_list_sum(this) result(val)
        class(dynamical_list_t), intent(in) :: this
        integer(kind=i4) :: val

        val = sum(this%list(1:this%n_used))

    end function dynamical_list_sum

    !> Get element at position (copy)
    function dynamical_list_get(this, position) result(val)
        class(dynamical_list_t), intent(in) :: this
        integer(kind=i4), intent(in) :: position
        integer(kind=i4) :: val

        val = this%list(position)

    end function dynamical_list_get

    !> Get the last element of the list
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
    subroutine dynamical_list_add_element(this, element)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: element

        this%n_used = this%n_used + 1
        this%list(this%n_used) = element

    end subroutine dynamical_list_add_element

    !> Add an array to the list
    !> It will initialize the list if it is not already allocated, with the size of the array
    subroutine dynamical_list_add_array(this, array)
        class(dynamical_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: array(:)

        ! add the new array to the end of the list
        this%list(this%n_used+1:this%n_used+size(array)) = array
        ! add the size of the array to the number of used elements
        this%n_used = this%n_used + size(array)

    end subroutine dynamical_list_add_array

    !> Create from array, if it is not already allocated
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
    !> WARNING: This can change the position of the elements in the list, do not use sequentially with indexes 
    !> that are not updated after each removal
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

end module
