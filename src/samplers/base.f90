!> This module implements the basic structure for weighted samplers
module datastructs_samplers_base_mod
    use datastructs_kinds_mod
    implicit none
    private

    !> Base type for all samplers
    type, abstract :: sampler_base_t
        real(dp), allocatable :: weights(:) !! Individual weights assigned to each element
        integer(i4) :: n = 0                !! Total number of weights
    contains
        procedure(i_init_n), deferred :: init_n !! Initializes with a fixed number of weights
        procedure(i_init_w), deferred  :: init_w !! Initializes with one additional parameter (e.g. max_weight or ratio)
        procedure(i_init_w2), deferred  :: init_w2 !! Initializes with two additional parameters (e.g. min_weight and max_weight)
        generic :: init => init_n, init_w, init_w2 !! Generic initialization interface

        procedure(i_reset), deferred :: reset !! Resets the sampler state (clears active weights and counts)

        procedure(i_set_weight), deferred :: set_weight !! Sets the weight for a specific index
        procedure(i_set_weight_array), deferred :: set_weight_array !! Sets the weights for all indices

        procedure(i_add_weight), deferred :: add_weight !! Adds a delta weight to a specific index

        procedure(i_sample), deferred :: sample !! Returns an index sampled proportionally to its weight
        procedure(i_remove), deferred :: remove !! Removes a specific index (sets its weight to 0)
        procedure(i_sum), deferred :: sum !! Returns the total sum of all active weights
    end type sampler_base_t

    abstract interface
        subroutine i_init_n(this, n)
            import :: sampler_base_t, i4
            class(sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: n
        end subroutine
        subroutine i_init_w(this, n, w)
            import :: sampler_base_t, dp, i4
            class(sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: n
            real(dp), intent(in) :: w
        end subroutine
        subroutine i_init_w2(this, n, w1,w2)
            import :: sampler_base_t, dp, i4
            class(sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: n
            real(dp), intent(in) :: w1,w2
        end subroutine
        subroutine i_reset(this)
            import :: sampler_base_t
            class(sampler_base_t), intent(inout) :: this
        end subroutine
        subroutine i_set_weight(this, index, weight)
            import :: sampler_base_t, i4, dp
            class(sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: index
            real(dp), intent(in) :: weight
        end subroutine
        subroutine i_set_weight_array(this, weights)
            import :: sampler_base_t, dp
            class(sampler_base_t), intent(inout) :: this
            real(dp), intent(in) :: weights(:)
        end subroutine
        subroutine i_add_weight(this, index, delta_weight)
            import :: sampler_base_t, i4, dp
            class(sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: index
            real(dp), intent(in) :: delta_weight
        end subroutine
        function i_sample(this, gen) result(index)            
            use rndgen_mod, only: rndgen
            import :: sampler_base_t, i4            
            class(sampler_base_t), intent(in) :: this
            class(rndgen), intent(inout) :: gen
            integer(i4) :: index
        end function
        subroutine i_remove(this, index)
            import :: sampler_base_t, i4
            class(sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: index
        end subroutine
        function i_sum(this) result(total_weight)
            import :: sampler_base_t, dp
            class(sampler_base_t), intent(in) :: this
            real(dp) :: total_weight
        end function
    end interface

    public :: sampler_base_t

end module datastructs_samplers_base_mod