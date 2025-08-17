module samplers_mod
    use kinds_mod
    use samplers_base_mod, only : sampler_base_t
    implicit none
    private

    character(len=*), parameter :: samplers_choices = 'btree,rejection,rejection_two_classes,rejection_maxheap,rejection_maxheap_two_classes,rejection_maxheap_composition'

    public :: choose_sampler, samplers_choices

contains

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
    end subroutine

end module
