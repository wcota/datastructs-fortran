!> Module to select an weighted sampler
!> The idea is that it contains the list of indexes up to a size n,
!> and each index has an weight
!> The sampling is done proportionally to the weights.
!> It depends on the `rndgen-fortran` module, found at <https://github.com/wcota/rndgen-fortran>
!> Each sampler algorithm is implemented separately.
!> !> Example:
!> ```fortran
!> program example_sampler
!>     use datastructs_fortran
!>     use kinds_mod
!>     use rndgen_mod
!>     implicit none
!> 
!>     type(rndgen) :: gen
!>     class(sampler_base_t), allocatable :: my_sampler
!>     integer(kind=i4) :: i
!>     real(kind=dp), parameter :: weights(*) = [100.0_dp, 50.0_dp, 200.0_dp, 25.0_dp]
!>     integer(kind=i4), allocatable :: count_times(:)
!>     integer(kind=i4) :: selected_index
!>     integer(kind=i4), parameter :: n_samples = 10000
!> 
!>     ! choose the sampler algorithm
!>     call choose_sampler(my_sampler, 'btree')
!>     call my_sampler%init(size(weights))
!>     call my_sampler%set_weight_array(weights)
!> 
!>     ! initialize the random generator with a seed
!>     call gen%init(12345)
!> 
!>     ! count the number of times each element was selected
!>     allocate(count_times(size(weights)))
!>     count_times = 0
!> 
!>     ! Sample from the distribution
!>     do i = 1, n_samples
!>         selected_index = my_sampler%sample(gen)
!>         !write(*, fmt_general) "Selected index:", selected_index
!>         count_times(selected_index) = count_times(selected_index) + 1
!>     end do
!> 
!>     ! Print the result
!>     write(*, fmt_general) "For n_samples = ", n_samples
!>     do i = 1, size(weights)
!>         write(*, fmt_general) "@ Index:", i, " Weight:", weights(i), "Count:", count_times(i), "Prob:", 1.0_dp * count_times(i) / sum(count_times), "Expected prob:", weights(i) / sum(weights)
!>     end do
!> end program example_sampler
!> ```
module samplers_mod
    use kinds_mod
    use samplers_base_mod, only : sampler_base_t
    implicit none
    private

    !> List of available sampler algorithms
    character(len=*), parameter :: sampler_choices = 'btree,rejection,rejection_two_classes,rejection_maxheap,rejection_maxheap_two_classes,rejection_maxheap_composition'

    public :: choose_sampler, sampler_choices

contains

    !> Subroutine to allocate a `class(sampler_base_t)` to its respective object
    subroutine choose_sampler(weighted_sampler, selected_algorithm)

        ! imports the different weighted samplers based on the selected algorithm
        use samplers_btree_mod, only: weighted_sampler_btree_t => weighted_sampler_t
        use samplers_rejection_mod, only: weighted_sampler_rejection_t => weighted_sampler_t
        use samplers_rejection_two_classes_mod, only: weighted_sampler_rejection_two_classes_t => weighted_sampler_t
        use samplers_rejection_maxheap_mod, only: weighted_sampler_rejection_maxheap_t => weighted_sampler_t
        use samplers_rejection_maxheap_two_classes_mod, only: weighted_sampler_rejection_maxheap_two_classes_t => weighted_sampler_t
        use samplers_rejection_maxheap_composition_mod, only: weighted_sampler_rejection_maxheap_composition_t => weighted_sampler_t

        class(sampler_base_t), allocatable, intent(out) :: weighted_sampler
        character(len=*), intent(in) :: selected_algorithm

        select case (trim(adjustl(selected_algorithm)))
            case ('btree')
                allocate(weighted_sampler_btree_t :: weighted_sampler)
            case ('rejection')
                allocate(weighted_sampler_rejection_t :: weighted_sampler)
            case ('rejection_maxheap')
                allocate(weighted_sampler_rejection_maxheap_t :: weighted_sampler)
            case ('rejection_two_classes')
                allocate(weighted_sampler_rejection_two_classes_t :: weighted_sampler)
            case ('rejection_maxheap_two_classes')
                allocate(weighted_sampler_rejection_maxheap_two_classes_t :: weighted_sampler)
            case ('rejection_maxheap_composition')
                allocate(weighted_sampler_rejection_maxheap_composition_t :: weighted_sampler)
            case default
                error stop 'Unknown algorithm selected: '//trim(adjustl(selected_algorithm))
        end select
    end subroutine choose_sampler

end module samplers_mod
