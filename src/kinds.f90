module mod_kinds
    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, &
        dp => real64, &
        i1 => int8, &
        i2 => int16, &
        i4 => int32, &
        i8 => int64
    implicit none
    private

    public :: sp, dp, i1, i2, i4, i8

end module