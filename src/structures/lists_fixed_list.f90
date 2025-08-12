module hOGA_lists_fixed_list_mod
    use hOGA_kinds_mod
    implicit none
    private

    interface fixed_list
        module procedure fixed_list_new
    end interface

    type :: fixed_list_t
        integer(kind=i4) :: initial_index, final_index
        integer(kind=i4) :: n
        integer(kind=i4), allocatable :: list(:)

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

    function new_fixed_list_pointer(list) result(this)
        type(fixed_list_t), pointer :: this
        integer(kind=i4), intent(in) :: list(:)

        this%initial_index = 1
        this%final_index = size(list)
        this%list = list

    end function new_fixed_list_pointer

    !> Initialize the fixed list
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

end module
