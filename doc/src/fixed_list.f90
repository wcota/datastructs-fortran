!> This module implements a fixed list, that has a fixed size and cannot be resized.
!> It also has pointers, allowing to build a linked list structure.
!> Example:
!> ```fortran
!> program example_fixed_list
!>     use datastructs_fortran
!>     use kinds_mod
!>     implicit none
!>     type(fixed_list_t) :: my_list
!>     type(fixed_list_t), target :: my_list2
!>     my_list = fixed_list([1,2,3,4,5])
!>     my_list2 = fixed_list([6,7,8])
!> 
!>     call my_list%print()
!> 
!>     my_list%next => my_list2
!> 
!>     call my_list%next%print()
!> end program example_fixed_list
!> ```

module lists_fixed_list_mod
    use kinds_mod
    implicit none
    private

    !> Constructor for a fixed list
    interface fixed_list
        module procedure fixed_list_new
    end interface fixed_list

    !> Type for a fixed list, that can be used as a list of lists
    type :: fixed_list_t
        !> Metadata
        integer(kind=i4) :: initial_index, final_index
        integer(kind=i4) :: n = 0
        integer(kind=i4), allocatable :: list(:)

        ! Pointers for the linked list
        ! It can be used to create a list of lists with different sizes
        type(fixed_list_t), pointer :: next, prev
    contains
        procedure :: init => fixed_list_init ! initialize the list
        procedure :: sum => fixed_list_sum ! sum the elements
        procedure :: print => fixed_list_print ! print the list

        final :: finalize_fixed_list ! finalize the list
    end type

    public :: fixed_list, new_fixed_list_pointer, fixed_list_t

contains

    !> Create a new fixed list
    !> Input: list - an array of integers
    function fixed_list_new(list) result(this)
        type(fixed_list_t) :: this
        integer(kind=i4), intent(in) :: list(:)

        ! allocate the fixed list
        this%initial_index = 1
        this%final_index = size(list)
        this%list = list

        ! set the size of the list
        this%n = size(list)

    end function fixed_list_new

    !> Create a new fixed list pointer
    !> Input: list - an array of integers
    function new_fixed_list_pointer(list) result(this)
        type(fixed_list_t), pointer :: this
        integer(kind=i4), intent(in) :: list(:)

        this%initial_index = 1
        this%final_index = size(list)
        this%list = list

        ! set the size of the list
        this%n = size(list)

    end function new_fixed_list_pointer

    !> Initialize the fixed list
    !> Input: i1 - initial index, i2 - final index
    !>        If i2 is not present, i1 is used as the size (final index)
    subroutine fixed_list_init(this, i1, i2)
        class(fixed_list_t), intent(inout) :: this
        integer(kind=i4), intent(in) :: i1
        integer(kind=i4), intent(in), optional :: i2
        integer(kind=i4) :: ini_index, fin_index

        if (present(i2)) then
            ini_index = i1
            fin_index = i2
        else
            ini_index = 1
            fin_index = i1
        end if

        this%initial_index = ini_index
        this%final_index = fin_index
        allocate(this%list(ini_index:fin_index))

        this%n = fin_index - ini_index + 1
    end subroutine fixed_list_init

    !> Sum the elements of the fixed list
    function fixed_list_sum(this) result(val)
        class(fixed_list_t), intent(in) :: this
        integer(kind=i4) :: val

        val = sum(this%list(this%initial_index:this%final_index))

    end function fixed_list_sum

    !> Print the fixed list
    subroutine fixed_list_print(this)
        class(fixed_list_t), intent(in) :: this

        write(*,fmt_general) '(initial_index = ', this%initial_index, ', final_index = ', this%final_index, ')', this%list

    end subroutine fixed_list_print

    !> Finalize the list
    subroutine finalize_fixed_list(this)
        type(fixed_list_t), intent(inout) :: this
        if (allocated(this%list)) deallocate(this%list)
    end subroutine finalize_fixed_list

end module lists_fixed_list_mod