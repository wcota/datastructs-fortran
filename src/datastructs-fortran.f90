module datastructs_fortran
    use hash_mod, only: djb2
    use lists_mod, only: dynamical_list, fixed_list, maxheap, &
                         dynamical_list_t, fixed_list_t, maxheap_t, &
                         unique_values, new_fixed_list_pointer
    use samplers_base_mod, only : sampler_base_t
    use samplers_mod, only: choose_sampler
    implicit none
    private

    public :: djb2

    public :: dynamical_list, fixed_list, maxheap
    public :: dynamical_list_t, fixed_list_t, maxheap_t
    public :: unique_values, new_fixed_list_pointer

    public :: sampler_base_t
    public :: choose_sampler
end module datastructs_fortran