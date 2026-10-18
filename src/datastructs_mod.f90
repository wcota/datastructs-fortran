!> `datastructs_mod` is the main umbrella module for the `datastructs-fortran` library.
!>
!> It re-exports all primary derived types, constructors, and utility procedures:
!> - **Weighted Samplers**: `choose_sampler`, `sampler_choices`, `sampler_base_t`
!> - **Temporal Observation Grid**: `time_grid_t` (uniform, geometric, logarithmic)
!> - **Sorting**: `sort`, `sort_index` (high-performance in-place Introsort)
!> - **Lists & Priority Queues**: `dynamical_list_t`, `fixed_list_t`, `maxheap_t`
!> - **Hashing & Packing**: `djb2`, `fnv1a`, `pack_pair`
!> - **Statistical Measures**: `statistical_measure_t`, `measure_controller_t`, `weighted_histogram_t`
!> - **Logging**: `log_unit`, `log_write`, `set_verbose`, `set_level`, etc.
module datastructs_mod
    use datastructs_hash_mod, only: djb2, pack_pair, fnv1a
    use datastructs_lists_mod, only: dynamical_list, fixed_list, maxheap, &
                         dynamical_list_t, fixed_list_t, maxheap_t, &
                         unique_values, new_fixed_list_pointer
    use datastructs_samplers_base_mod, only : sampler_base_t
    use datastructs_samplers_mod, only: choose_sampler, sampler_choices
    use datastructs_measures_mod, only : statistical_measure_t, measure_controller_t
    use datastructs_time_grid_mod, only : time_grid_t
    use datastructs_histograms_mod, only : weighted_histogram_t
    use datastructs_sorting_mod, only : sort, sort_index
    use datastructs_logger_mod, only : log_unit, set_verbose, set_level, set_output_unit, set_error_unit, set_unit_defaults, log_write, LOG_ERROR, LOG_WARNING, LOG_INFO, LOG_DEBUG, LOGGER_OK
    implicit none
    private

    !> Hash and packing functions
    public :: djb2, pack_pair, fnv1a

    !> High-performance sorting procedures
    public :: sort, sort_index

    !> Lists and maxheap constructors
    public :: dynamical_list, fixed_list, maxheap

    !> Derived types for lists and maxheap
    public :: dynamical_list_t, fixed_list_t, maxheap_t

    !> Utility functions for lists and maxheap
    public :: unique_values, new_fixed_list_pointer

    !> Base abstract type for all weighted samplers
    public :: sampler_base_t

    !> Factory function to instantiate weighted samplers
    public :: choose_sampler, sampler_choices

    !> Statistical measures and observation time grid for CTMC dynamics
    public :: statistical_measure_t, measure_controller_t, weighted_histogram_t, time_grid_t

    !> Logging utilities and constants
    public :: log_unit, set_verbose, set_level, set_output_unit, set_error_unit, set_unit_defaults, log_write, LOG_ERROR, LOG_WARNING, LOG_INFO, LOG_DEBUG, LOGGER_OK
end module datastructs_mod