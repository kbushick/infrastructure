!-------------------------------------------------------------------------------
!   Jared O'Neal - Personal Library of Unnecessary Code 
!-------------------------------------------------------------------------------
!
! MODULE: set_precision
!
! DESCRIPTION:
!> @brief
!> This file determines the decimal precision for f90 programs.
!> Switch which lines are commented out to change the precision.
!> Byte is also included so that you can use single byte integers.\n
!
!> @author Prof. Edward Overman
!> @author Jared O'Neal
!
! TODO:
! Nothing
!
!-------------------------------------------------------------------------------
module set_precision
    implicit none

    ! Kind types for 64-, 32-, 16-, and 8-bit signed integers
    integer, parameter :: byte  = selected_int_kind(1)
    integer, parameter :: int64 = selected_int_kind(18)
    integer, parameter :: int32 = selected_int_kind(9)
    integer, parameter :: int16 = selected_int_kind(4)
    integer, parameter :: int8  = selected_int_kind(2)

    ! Kind types for IEEE 754/IEC 60559 single- and double-precision reals
    integer, parameter :: ieee32 =  selected_real_kind(p=6,  r=37)
    integer, parameter :: ieee64 =  selected_real_kind(p=15, r=307)
!    Not certain that this is IEEE
!    integer, parameter :: ieee128 = selected_real_kind(p=18, r=4931)

    !!!!!----- Standard Fortran technique
    !integer, parameter :: sp = kind(1.0e0)  !   single precision
    !integer, parameter :: dp = kind(1.0d0)  !   double precision
    ! 1.0q0 not supported by all compilers
    !integer, parameter :: qp = kind(1.0q0)  !   quad precision

    !!!!!----- Manually setup kinds
    ! Numerical computing with Modern Fortran - 
    !     Hanson/Hopkins - Ch 2 / Table 2.1
    integer, parameter :: sp = selected_real_kind(p=6,  r=37)   ! single 
    integer, parameter :: dp = selected_real_kind(p=15, r=307)  ! double 
    integer, parameter :: qp = selected_real_kind(p=18, r=4931) ! quad (gcc)
!    integer, parameter :: qp = selected_real_kind(p=31, r=291) ! quad (Nag)

    !!!!!----- SPECIFY GENERAL-USE FLOATING POINT PRECISION HERE -----!!!!!
    integer, parameter :: wp = dp
end module set_precision

