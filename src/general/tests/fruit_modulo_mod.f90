!------------------------------------------------------------------------------
! Trivedi Group - Physics Department - The Ohio State University
!------------------------------------------------------------------------------
!
! MODULE: fruit_modulo_mod
!
! DESCRIPTION:
!> Command line unittesting of modulo_mod
!
!> @author Jared O'Neal
!
! TODO:
! Nothing
!
! REVISION HISTORY:
! 2017-05-02  Jared O'Neal         Created
!------------------------------------------------------------------------------

program fruit_modulo_mod
    use fruit
    use test_modulo_mod

    integer :: n_failed = 0

    CALL init_fruit

    CALL test_mod1

    write(*,*) ''
    write(*,*) ''

    CALL get_failed_count(n_failed)
    
    CALL fruit_summary
    CALL fruit_finalize
    
    CALL EXIT(n_failed)
end program fruit_modulo_mod
