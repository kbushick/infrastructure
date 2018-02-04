!------------------------------------------------------------------------------
! Trivedi Group - Physics Department - The Ohio State University
!------------------------------------------------------------------------------
!
! MODULE: test_modulo_mod
!
! DESCRIPTION:
!> Unittests of module_modulo
!
!> @author Jared O'Neal
!
! TODO:
! Nothing
!
! REVISION HISTORY:
! 2016-07-07  Jared O'Neal         Created
!------------------------------------------------------------------------------

module test_modulo_mod
    use fruit
    use modulo_mod, only : mod1

    implicit none
    private

    public :: test_mod1

contains

    !
    ! Confirm correct implementation of Fortran/IEEE standards wrt overflow
    !
    subroutine test_mod1
        integer :: k = 4
        integer :: seq(10) = [-4, -3, -2, -1, 0, 1, 2, 3, 4, 5]
        integer :: expected(10) = [4, 1, 2, 3, 4, 1, 2, 3, 4, 1]
        integer :: result = 0

        integer :: n = 0
        integer :: j = 0

        do j = 1, SIZE(seq)
            n = seq(j)
            result = mod1(n, k)
            CALL assert_equals(expected(j), result)
        end do
    end subroutine test_mod1

end module test_modulo_mod
