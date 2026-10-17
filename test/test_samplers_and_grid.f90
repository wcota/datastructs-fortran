!> Comprehensive test suite for samplers, maxheap, and time_grid
program test_samplers_and_grid
    use datastructs_mod
    use datastructs_kinds_mod
    use rndgen_mod
    implicit none

    type(rndgen) :: gen
    call gen%init(42)

    write(*, *) "=== Running test_samplers_and_grid ==="

    call test_maxheap_operations()
    call test_time_grid()
    call test_all_samplers(gen)

    write(*, *) "=== ALL TESTS PASSED SUCCESSFULLY! ==="

contains

    subroutine test_maxheap_operations()
        type(maxheap_t) :: h
        real(kind=dp), parameter :: vals(*) = [10.0_dp, 50.0_dp, 20.0_dp, 80.0_dp, 30.0_dp]
        integer(kind=i4) :: i

        write(*, *) "Testing maxheap operations..."
        call h%init(10)
        do i = 1, size(vals)
            call h%add(vals(i), i)
        end do

        if (abs(h%max_value() - 80.0_dp) > 1.0e-9_dp .or. h%max_index() /= 4) then
            error stop "test_maxheap: initial max failed"
        end if

        ! Remove the root (index 4)
        call h%remove(4)
        if (abs(h%max_value() - 50.0_dp) > 1.0e-9_dp .or. h%max_index() /= 2) then
            error stop "test_maxheap: remove root failed"
        end if

        ! Remove intermediate element
        call h%remove(2)
        if (abs(h%max_value() - 30.0_dp) > 1.0e-9_dp .or. h%max_index() /= 5) then
            error stop "test_maxheap: remove intermediate failed"
        end if

        write(*, *) "  [OK] maxheap tests passed"
    end subroutine test_maxheap_operations

    subroutine test_time_grid()
        type(time_grid_t) :: g
        type(statistical_measure_t) :: m
        integer(kind=i4) :: s_tick, e_tick

        write(*, *) "Testing time_grid_t..."
        ! 1. Uniform
        call g%init_uniform(tmin=0.0_dp, tmax=10.0_dp, dt=2.0_dp)
        if (g%size() /= 6) error stop "test_time_grid: uniform size mismatch"
        if (abs(g%time_at(1) - 0.0_dp) > 1.0e-9_dp) error stop "test_time_grid: uniform t0 mismatch"
        if (abs(g%time_at(6) - 10.0_dp) > 1.0e-9_dp) error stop "test_time_grid: uniform tmax mismatch"

        ! 2. Geometric
        call g%init_geometric(tmin=1.0_dp, tmax=16.0_dp, ratio=2.0_dp)
        if (g%size() /= 5) error stop "test_time_grid: geometric size mismatch"
        if (abs(g%time_at(5) - 16.0_dp) > 1.0e-9_dp) error stop "test_time_grid: geometric tmax mismatch"

        ! 3. Log
        call g%init_log(tmin=1.0_dp, tmax=100.0_dp, n_points=5)
        if (g%size() /= 5) error stop "test_time_grid: log size mismatch"
        if (abs(g%time_at(5) - 100.0_dp) > 1.0e-9_dp) error stop "test_time_grid: log tmax mismatch"

        ! 4. Record and collect_ticks
        call m%init(g%size())
        call g%reset()

        call g%collect_ticks(0.5_dp, s_tick, e_tick)
        if (s_tick <= e_tick) error stop "test_time_grid: collect_ticks before tmin failed"

        call g%record(m, 10.0_dp, 42.0_dp)
        ! All points with time <= 10.0 should be recorded
        if (m%n_samples(1) /= 1 .or. m%n_samples(2) /= 1) error stop "test_time_grid: record failed"

        write(*, *) "  [OK] time_grid_t tests passed"
    end subroutine test_time_grid

    subroutine test_all_samplers(gen)
        type(rndgen), intent(inout) :: gen
        class(sampler_base_t), allocatable :: s
        character(len=40), parameter :: algs(*) = [ &
            character(len=40) :: 'btree', &
            'rejection', &
            'rejection_maxheap', &
            'rejection_two_classes', &
            'rejection_maxheap_two_classes', &
            'rejection_maxheap_composition' ]
        real(kind=dp), parameter :: test_weights(*) = [10.0_dp, 50.0_dp, 20.0_dp, 5.0_dp]
        integer(kind=i4) :: a, idx
        real(kind=dp) :: tot

        write(*, *) "Testing all 6 weighted sampler algorithms..."

        do a = 1, size(algs)
            write(*, '(A, A)') "  Testing sampler: ", trim(algs(a))
            call choose_sampler(s, trim(algs(a)))

            call s%init(size(test_weights))
            call s%set_weight_array(test_weights)

            tot = s%sum()
            if (abs(tot - sum(test_weights)) > 1.0e-6_dp) then
                write(*, *) "Algorithm sum mismatch: ", trim(algs(a)), " sum=", tot, " expected=", sum(test_weights)
                error stop "Sampler sum error"
            end if

            ! Sample several times
            idx = s%sample(gen)
            if (idx < 1 .or. idx > size(test_weights)) error stop "Sample out of bounds"

            ! Remove element
            call s%remove(2) ! remove 50.0_dp
            tot = s%sum()
            if (abs(tot - 35.0_dp) > 1.0e-6_dp) then
                write(*, *) "Algorithm sum mismatch after remove: ", trim(algs(a)), " sum=", tot
                error stop "Sampler sum after remove error"
            end if

            ! Add weight back
            call s%add_weight(2, 50.0_dp)
            tot = s%sum()
            if (abs(tot - sum(test_weights)) > 1.0e-6_dp) error stop "Sampler sum after add_weight error"

            ! Reset
            call s%reset()
            if (abs(s%sum() - 0.0_dp) > 1.0e-9_dp) error stop "Sampler sum after reset error"

            deallocate(s)
        end do

        write(*, *) "  [OK] All 6 samplers passed functionality tests"
    end subroutine test_all_samplers

end program test_samplers_and_grid
