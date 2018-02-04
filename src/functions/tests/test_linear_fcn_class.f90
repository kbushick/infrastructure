!------------------------------------------------------------------------------
! Jared O'Neal - Personal Library of Unnecessary Code
!------------------------------------------------------------------------------
!
! MODULE: test_linear_fcn_class
!
! DESCRIPTION:
!> Unittests of class that manages linear functions including least-sqrs fits
!
!> @author Jared O'Neal
!
! TODO:
! Nothing
!
!------------------------------------------------------------------------------

module test_linear_fcn_class
    use fruit

    use set_precision, only : wp
    use linear_fcn_class

    implicit none
    private

    public :: test_evaluate
    public :: test_from_least_sqrs_1
    public :: test_from_least_sqrs_2
    public :: test_from_least_sqrs_3

contains

    !
    ! Check evaluation of a line at given points
    !
    subroutine test_evaluate
        real(wp), parameter :: a1 = -4.1234_wp
        real(wp), parameter :: a0 = 10.5432_wp

        type(linear_fcn_t) :: fcn = linear_fcn_t(a1=0.0_wp, a0=0.0_wp)
        real(wp)           :: x_data(4) = [-1.1_wp, &
                                            0.0_wp, &
                                            2.2_wp, &
                                            10.4_wp]
        real(wp)           :: result(SIZE(x_data)) = 0.0_wp
        real(wp)           :: expected(SIZE(x_data)) = 0.0_wp

        CALL assert_false(MAXVAL(ABS(result - expected)) > 0.0_wp)

        ! Test both
        fcn%a1 = a1
        fcn%a0 = a0
        expected = [15.07894_wp, 10.5432_wp, 1.47172_wp, -32.34016_wp]
        CALL fcn%evaluate(x_data, result)

        CALL assert_true(MAXVAL(ABS(result - expected)) < 1.0e-14_wp)
    end subroutine test_evaluate

    !
    ! Check precision with trivial problem
    !
    subroutine test_from_least_sqrs_1
        real(wp), parameter :: a1 = -2.1_wp
        real(wp), parameter :: a0 =  1.1_wp

        type(linear_fcn_t) :: fit = linear_fcn_t(a1=a1, a0=a0)
        real(wp)           :: x_data(6) = [0.0_wp, &
                                           1.0_wp, &
                                           3.0_wp, &
                                           4.0_wp, &
                                           5.0_wp, &
                                           8.0_wp]
        real(wp)           :: y_data(SIZE(x_data))
        real(wp)           :: sigma_n(SIZE(x_data))

        ! If we are using double precision correctly in the module and here,
        ! these results should be *VERY* precise
        CALL fit%evaluate(x_data, y_data)

        ! USE UNIFORM INTERFACE
        CALL linear_fcn_from_least_squares(x_data, y_data, 0.0_wp, fit)

        CALL assert_true(ABS(fit%a1 - a1) < 5.0e-15_wp)
        CALL assert_true(ABS(fit%a0 - a0) < 5.0e-15_wp)

        CALL assert_true(ABS(fit%sigma_a1) < 5.0e-15_wp)
        CALL assert_true(ABS(fit%sigma_a0) < 5.0e-15_wp)

        CALL assert_true(ABS(fit%r2 - 1.0_wp) < 5.0e-15_wp)
        CALL assert_true(ABS(fit%chi2 - 1.0_wp) < 5.0e-15_wp)

        ! USE NON-UNIFORM INTERFACE
        sigma_n(:) = 5.0e-16_wp
        CALL linear_fcn_from_least_squares(x_data, y_data, sigma_n, fit)

        CALL assert_true(ABS(fit%a1 - a1) < 5.0e-15_wp)
        CALL assert_true(ABS(fit%a0 - a0) < 5.0e-15_wp)

        CALL assert_true(ABS(fit%sigma_a1) < 5.0e-15_wp)
        CALL assert_true(ABS(fit%sigma_a0) < 5.0e-15_wp)

        CALL assert_true(ABS(fit%r2 - 1.0_wp) < 5.0e-15_wp)
    end subroutine test_from_least_sqrs_1

    !
    ! Check linear least squares fit
    !
    subroutine test_from_least_sqrs_2
        ! Data Taken From "Data Reduction & Error Analysis for the Physical Sciences",
        !              Bevington, Philip & D. Keith Richardson.  Second Edtn, Pp. 97.
        !              ISBN:  0-07-911243-9
        real(wp), parameter :: sigma_u = 0.05_wp

        type(linear_fcn_t) :: fit_u = linear_fcn_t(0.0_wp, 0.0_wp)
        type(linear_fcn_t) :: fit_n = linear_fcn_t(0.0_wp, 0.0_wp)
        real(wp)           :: x_data(9) = [10.0_wp, &
                                           20.0_wp, &
                                           30.0_wp, &
                                           40.0_wp, &
                                           50.0_wp, &
                                           60.0_wp, &
                                           70.0_wp, &
                                           80.0_wp, &
                                           90.0_wp]
        real(wp)           :: y_data(SIZE(x_data)) = [0.37_wp, &
                                                      0.58_wp, &
                                                      0.83_wp, &
                                                      1.15_wp, &
                                                      1.36_wp, &
                                                      1.62_wp, &
                                                      1.90_wp, &
                                                      2.18_wp, &
                                                      2.45_wp]
        real(wp)           :: sigma_n(SIZE(x_data))

        ! USE UNIFORM INTERFACE
        CALL linear_fcn_from_least_squares(x_data, y_data, sigma_u, fit_u)

        CALL assert_true(ABS(fit_u%a1 - 0.0262_wp) < 5.0e-5_wp)
        CALL assert_true(ABS(fit_u%a0 - 0.0714_wp) < 5.0e-5_wp)

        CALL assert_true(ABS(fit_u%sigma_a1 - 0.00065_wp) < 7.5e-6_wp)
        CALL assert_true(ABS(fit_u%sigma_a0 - 0.036_wp) < 5.0e-4_wp)

        CALL assert_true(ABS(fit_u%r2 - (0.9994_wp * 0.9994_wp)) < 5.0e-5_wp)
        CALL assert_true(ABS(fit_u%chi2 - (1.95_wp / 7.0_wp)) < 7.5e-5_wp)

        ! USE NON-UNIFORM INTERFACE
        sigma_n(:) = sigma_u
        CALL linear_fcn_from_least_squares(x_data, y_data, sigma_n, fit_n)

        CALL assert_true(ABS(fit_n%a1 - fit_u%a1) < 5.0e-15_wp)
        CALL assert_true(ABS(fit_n%a0 - fit_u%a0) < 5.0e-15_wp)

        CALL assert_true(ABS(fit_n%sigma_a1 - fit_u%sigma_a1) < 7.5e-15_wp)
        CALL assert_true(ABS(fit_n%sigma_a0 - fit_u%sigma_a0) < 5.0e-15_wp)

        CALL assert_true(ABS(fit_n%r2 - fit_u%r2) < 5.0e-15_wp)
        CALL assert_true(ABS(fit_n%chi2 - fit_n%chi2) < 7.5e-15_wp)
    end subroutine test_from_least_sqrs_2

    !
    ! Check linear least squares fit
    !
    subroutine test_from_least_sqrs_3
        ! Data Taken From "Data Reduction & Error Analysis for the Physical Sciences",
        !              Bevington, Philip & D. Keith Richardson.  Second Edtn, Pp. 98.
        !              ISBN:  0-07-911243-9
        type(linear_fcn_t) :: fit = linear_fcn_t(0.0_wp, 0.0_wp)
        real(wp)           :: x_data(10) = [25.00_wp, &
                                            16.00_wp, &
                                            11.11_wp, &
                                             8.16_wp, &
                                             6.25_wp, &
                                             4.94_wp, &
                                             4.00_wp, &
                                             2.78_wp, &
                                             1.78_wp, &
                                             1.00_wp]
        real(wp)           :: y_data(10) = [901.0_wp, &
                                            652.0_wp, &
                                            443.0_wp, &
                                            339.0_wp, &
                                            283.0_wp, &
                                            281.0_wp, &
                                            240.0_wp, &
                                            220.0_wp, &
                                            180.0_wp, &
                                            154.0_wp]
        real(wp)           :: sigma(10) = [30.0_wp, &
                                           25.5_wp, &
                                           21.0_wp, &
                                           18.4_wp, &
                                           16.8_wp, &
                                           16.8_wp, &
                                           15.5_wp, &
                                           14.8_wp, &
                                           13.4_wp, &
                                           12.4_wp]

        CALL linear_fcn_from_least_squares(x_data, y_data, sigma, fit)

        CALL assert_true(ABS(fit%a1 - 30.7_wp) < 2.5e-3_wp)
        CALL assert_true(ABS(fit%a0 - 119.5_wp) < 5.0e-2_wp)

        CALL assert_true(ABS(fit%sigma_a1 - 1.1_wp) < 7.5e-2_wp)
        CALL assert_true(ABS(fit%sigma_a0 - 7.6_wp) < 7.5e-2_wp)

        CALL assert_true(ABS(fit%r2 - (0.9938_wp * 0.9938_wp)) < 2.5e-4_wp)
        CALL assert_true(ABS(fit%chi2 - (11.1_wp / 8.0_wp)) < 5.0e-2_wp)
    end subroutine test_from_least_sqrs_3

end module test_linear_fcn_class

