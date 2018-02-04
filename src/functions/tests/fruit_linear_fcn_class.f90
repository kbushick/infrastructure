!------------------------------------------------------------------------------
! Jared O'Neal - Personal Library of Unnecessary Code
!------------------------------------------------------------------------------
!
! MODULE: fruit_linear_fcn_class
!
!> @author Jared O'Neal
!
! DESCRIPTION:
!> Command line unittesting of the linear function class.
!
! TODO:
! Nothing
!
!------------------------------------------------------------------------------

program fruit_linear_fcn_class
    use fruit

    use test_linear_fcn_class

    integer :: n_failed = 0

    CALL init_fruit

    CALL test_evaluate
    CALL test_from_least_sqrs_1
    CALL test_from_least_sqrs_2
    CALL test_from_least_sqrs_3

    write(*,*)
    write(*,*)

    CALL get_failed_count(n_failed)

    CALL fruit_summary
    CALL fruit_finalize

    CALL EXIT(n_failed)
end program fruit_linear_fcn_class
