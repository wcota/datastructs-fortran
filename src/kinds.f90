module hOGA_kinds_mod
    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, & ! single precision, range: -3.40282347E+38 to 3.40282347E+38, smallest positive: 1.17549435E-38
        dp => real64, & ! double precision, range: -1.7976931348623157E+308 to 1.7976931348623157E+308, smallest positive: 2.2250738585072014E-308
        qp => real128, &
        i1 => int8, &   ! 1 byte integer, range: -128 to 127
        i2 => int16, &  ! 2 byte integer, range: -32768 to 32767
        i4 => int32, &  ! 4 byte integer, range: -2147483648 to 2147483647
        i8 => int64     ! 8 byte integer, range: -9223372036854775808 to 9223372036854775807
    implicit none
    private

    character(len=*), parameter :: fmt_general = '(*(g0,x))'

    public :: sp, dp, i1, i2, i4, i8, list_ranges, fmt_general

contains

    subroutine list_ranges()
        real(kind=sp) :: real_sp
        real(kind=dp) :: real_dp
        real(kind=qp) :: real_qp
        integer(kind=i1) :: int_i1
        integer(kind=i2) :: int_i2
        integer(kind=i4) :: int_i4
        integer(kind=i8) :: int_i8

        ! Print the ranges of the types
        write(*,*) "Single precision range (positive): ", tiny(real_sp), " to ", huge(real_sp)
        write(*,*) "Double precision range (positive): ", tiny(real_dp), " to ", huge(real_dp)
        write(*,*) "Quadruple precision range (positive): ", tiny(real_qp), " to ", huge(real_qp)
        write(*, *) "1 byte integer range: ", -huge(int_i1) - 1, " to ", huge(int_i1)
        write(*, *) "2 byte integer range: ", -huge(int_i2) - 1, " to ", huge(int_i2)
        write(*, *) "4 byte integer range: ", -huge(int_i4) - 1, " to ", huge(int_i4)
        write(*, *) "8 byte integer range: ", -huge(int_i8) - 1, " to ", huge(int_i8)
    end subroutine list_ranges

end module