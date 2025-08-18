module datastructs_fortran
    use hash_mod, only: djb2
    use lists_mod, only: dynamical_list, fixed_list, maxheap, &
                         dynamical_list_t, fixed_list_t, maxheap_t, &
                         unique_values, new_fixed_list_pointer
    use samplers_base_mod, only : sampler_base_t
    use samplers_mod, only: choose_sampler, samplers_choices
    implicit none
    private

    !> Hash functions
    public :: djb2

    !> Lists and maxheap constructors
    public :: dynamical_list, fixed_list, maxheap

    !> Derived types for lists and maxheap
    public :: dynamical_list_t, fixed_list_t, maxheap_t

    !> Utility functions for lists and maxheap
    public :: unique_values, new_fixed_list_pointer

    !> Base type for all samplers
    public :: sampler_base_t

    !> Choose a sampler based on the selected algorithm
    public :: choose_sampler, samplers_choices
end module datastructs_fortran