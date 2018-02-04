!-------------------------------------------------------------------------------
!   Jared O'Neal - Personal Library of Unnecessary Code 
!-------------------------------------------------------------------------------
!
! MODULE: modulo_mod
!
! DESCRIPTION:
!> Code for modulo arithmetic.
!
!> @author Jared O'Neal
!
! TODO:
! Nothing
!
!-------------------------------------------------------------------------------

module modulo_mod
    implicit none
    private

    public :: mod1

contains

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    !> @brief
    !> Perform k-modulo integer arithmetic such that wrapping is one-based.\n
    !> Ex. For k = 4, the sequence 0, 1, 2, 3, 4, 5 maps to
    !>                             4, 1, 2, 3, 4, 1
    !>     instead of the zero-based scheme, which maps to
    !>                             0, 1, 2, 3, 0, 1.
    !> This subroutine is therefore useful for modulo arithemetic applied to
    !> loop integers.
    !
    !> @author Jared O'Neal
    !
    ! TODO:
    ! Nothing
    !
    !> @param[in] n - number to which apply the modulo operation
    !> @param[in] k - base of modulo operation
    !> @returns The modulo result
    !---------------------------------------------------------------------------
    function mod1(n, k) result(m)
        integer, intent(IN) :: k
        integer, intent(IN) :: n

        integer :: m

        m = MODULO(n - 1, k) + 1;
    end function mod1

end module modulo_mod

