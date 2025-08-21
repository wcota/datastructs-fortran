module datastructs_mod
    use datastructs_hash_mod, only: djb2
    use datastructs_lists_mod, only: dynamical_list, fixed_list, maxheap, &
                         dynamical_list_t, fixed_list_t, maxheap_t, &
                         unique_values, new_fixed_list_pointer
    use datastructs_samplers_base_mod, only : sampler_base_t
    use datastructs_samplers_mod, only: choose_sampler, sampler_choices
    use datastructs_measures_mod, only : statistical_measure_t, measure_controller_t
    use datastructs_histograms_mod, only : weighted_histogram_t
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
    public :: choose_sampler, sampler_choices

    !> Statistical measures
    public :: statistical_measure_t, measure_controller_t, weighted_histogram_t
end module datastructs_mod