module hoga_weighted_sampler_base_mod
    use hoga_kinds_mod
    implicit none
    private

    type, abstract :: weighted_sampler_base_t
        real(dp), allocatable :: weights(:)      ! pesos individuais
        integer(i4) :: n = 0                    ! número de pesos
    contains
        procedure(i_init_n), deferred :: init_n
        procedure(i_init_w), deferred  :: init_w
        procedure(i_init_w2), deferred  :: init_w2
        generic :: init => init_n, init_w, init_w2
        procedure(i_reset), deferred :: reset
        procedure(i_set_weight), deferred :: set_weight
        procedure(i_set_weight_array), deferred :: set_weight_array
        procedure(i_add_weight), deferred :: add_weight
        procedure(i_sample), deferred :: sample
        procedure(i_remove), deferred :: remove
        procedure(i_sum), deferred :: sum
    end type weighted_sampler_base_t

    abstract interface
        subroutine i_init_n(this, n)
            import :: weighted_sampler_base_t, i4
            class(weighted_sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: n
        end subroutine
        subroutine i_init_w(this, n, w)
            import :: weighted_sampler_base_t, dp, i4
            class(weighted_sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: n
            real(dp), intent(in) :: w
        end subroutine
        subroutine i_init_w2(this, n, w1,w2)
            import :: weighted_sampler_base_t, dp, i4
            class(weighted_sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: n
            real(dp), intent(in) :: w1,w2
        end subroutine
        subroutine i_reset(this)
            import :: weighted_sampler_base_t
            class(weighted_sampler_base_t), intent(inout) :: this
        end subroutine
        subroutine i_set_weight(this, index, weight)
            import :: weighted_sampler_base_t, i4, dp
            class(weighted_sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: index
            real(dp), intent(in) :: weight
        end subroutine
        subroutine i_set_weight_array(this, weights)
            import :: weighted_sampler_base_t, dp
            class(weighted_sampler_base_t), intent(inout) :: this
            real(dp), intent(in) :: weights(:)
        end subroutine
        subroutine i_add_weight(this, index, delta_weight)
            import :: weighted_sampler_base_t, i4, dp
            class(weighted_sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: index
            real(dp), intent(in) :: delta_weight
        end subroutine
        function i_sample(this, gen) result(index)            
            use rndgen_mod, only: rndgen
            import :: weighted_sampler_base_t, i4            
            class(weighted_sampler_base_t), intent(in) :: this
            class(rndgen), intent(inout) :: gen
            integer(i4) :: index
        end function
        subroutine i_remove(this, index)
            import :: weighted_sampler_base_t, i4
            class(weighted_sampler_base_t), intent(inout) :: this
            integer(i4), intent(in) :: index
        end subroutine
        function i_sum(this) result(total_weight)
            import :: weighted_sampler_base_t, dp
            class(weighted_sampler_base_t), intent(in) :: this
            real(dp) :: total_weight
        end function
    end interface

    public :: weighted_sampler_base_t

end module
