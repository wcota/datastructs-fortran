!> General module for lists and related operations
module lists_mod
    use kinds_mod
    use lists_dynamical_list_mod
    use lists_fixed_list_mod
    use maxheap_mod
    implicit none
    private

    !> Constructors
    public :: dynamical_list, fixed_list, maxheap

    !> Derived types
    public :: dynamical_list_t, fixed_list_t, maxheap_t

    !> Operations
    public :: unique_values
    public :: new_fixed_list_pointer

contains

    !> Find unique values in a list of integers
    !> It uses the standard library sorting module
    !> The result is a list of unique integers
    function unique_values(list) result(unique)
        use stdlib_sorting, only: sort
        integer(kind=i4), intent(in) :: list(:)
        integer(kind=i4), allocatable :: unique(:)
        integer(kind=i4), allocatable :: sorted_list(:)
        integer(kind=i4) :: i, count_unique

        sorted_list = list

        call sort(sorted_list)

        count_unique = 1
        do i = 2, size(sorted_list)
            if (sorted_list(i) /= sorted_list(i-1)) count_unique = count_unique + 1
        end do

        ! get the unique values
        allocate(unique(count_unique))
        unique(1) = sorted_list(1)
        count_unique = 1
        do i = 2, size(sorted_list)
            if (sorted_list(i) /= sorted_list(i-1)) then
                count_unique = count_unique + 1
                unique(count_unique) = sorted_list(i)
            end if
        end do

    end function unique_values

end module
