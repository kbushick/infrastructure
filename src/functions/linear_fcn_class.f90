#include "errormsg.inc"

!------------------------------------------------------------------------------
! Jared O'Neal - Personal Library of Unnecessary Code
!------------------------------------------------------------------------------
!
! MODULE: linear_fcn_class
!
! DESCRIPTION:
!> Code for managing a linear function and creating a particular linear
!> function from direct linear least-squares.  The function is defined by 
!> \f[
!> y(x) = a_1x + a_0.
!> \f]
!
!> @athor Jared O'Neal
!
! TODO:
! Nothing
!
!------------------------------------------------------------------------------

module linear_fcn_class
    use set_precision, only : wp

    implicit none
    private

    !---------------------------------------------------------------------------
    !> @brief
    !> Class that houses the necessary parameters for specifying a line.
    !> If the function was created as a fit to data, the object can optionally
    !> contain fit information.
    !---------------------------------------------------------------------------
    type, public :: linear_fcn_t
        real(wp) :: a1 = 0.0_wp         !<    Slope
        real(wp) :: a0 = 0.0_wp         !<    Y-intercept

        ! [OPTIONAL] These are used when an instance is created from a fit
        real(wp) :: sigma_a1 = 0.0_wp   !<    Slope fit error (optional)
        real(wp) :: sigma_a0 = 0.0_wp   !<    Y-intercept fit error (optional)
        real(wp) :: r2 = 1.0_wp         !<    R-squared quality of fit (optional)
        real(wp) :: chi2 = 1.0_wp       !<    Reduced chi-squared goodness-of-fit (optional)
    contains 
        generic :: evaluate => evaluate_singleton, evaluate_set
        procedure, private :: evaluate_singleton
        procedure, private :: evaluate_set
    end type linear_fcn_t

    !---------------------------------------------------------------------------
    !> @brief
    !> Generic interface for doing linear least-squares on data with uniform
    !> or non-uniform estimated measurement error
    !
    !> @param[in] x_data - the points in the domain at which we evaluate
    !> @param[in] y_data - the results of the function evaluation
    !> @param[in] sigma - the estimated measurement error as a real or an array
    !> @returns The linear function derived type containing the data
    !---------------------------------------------------------------------------
    interface linear_fcn_from_least_squares
        module procedure from_least_sqrs_uniform
        module procedure from_least_sqrs_nonuniform
    end interface linear_fcn_from_least_squares

    public :: linear_fcn_from_least_squares

contains

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    !> @brief
    !> Evaluate the linear function at a single given point.
    !
    !> @author Jared O'Neal
    !
    ! TODO:
    ! Nothing
    !
    !> @param[in] x_data - the point in the domain at which we evaluate
    !> @param[out] y_data - the result of the function evaluation
    !---------------------------------------------------------------------------
    subroutine evaluate_singleton(this, x_data, y_data)
        class(linear_fcn_t), intent(IN)  :: this
        real(wp),            intent(IN)  :: x_data
        real(wp),            intent(OUT) :: y_data

        y_data = this%a1 * x_data + this%a0
    end subroutine evaluate_singleton

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    !> @brief
    !> Evaluate the linear function at a set of given points.
    !
    !> @author Jared O'Neal
    !
    ! TODO:
    ! Nothing
    !
    !> @param[in] x_data - the points in the domain at which we evaluate
    !> @param[out] y_data - the results of the function evaluation
    !---------------------------------------------------------------------------
    subroutine evaluate_set(this, x_data, y_data)
        class(linear_fcn_t), intent(IN)  :: this
        real(wp),            intent(IN)  :: x_data(:)
        real(wp),            intent(OUT) :: y_data(:)

        y_data = this%a1 * x_data + this%a0
    end subroutine evaluate_set

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    !> @brief
    !> Create a linear fit to given data using the direct linear least-squares
    !> method.  This should only be applied to data where the estimated error
    !> for measuring data is the same at all x_data values.
    !
    !> @author Jared O'Neal
    !
    ! TODO:
    !  Nothing
    !
    !> @param[in] x_data - the points in the domain at which to evaluate
    !> @param[in] y_data - the results of the function evaluation
    !> @param[in] sigma - the estimated measurement error
    !> @returns A linear function object that contains the fit results.
    !---------------------------------------------------------------------------
    subroutine from_least_sqrs_uniform(x_data, y_data, sigma, linear_fcn)
        real(wp),           intent(IN)  :: x_data(:)
        real(wp),           intent(IN)  :: y_data(:)
        real(wp),           intent(IN)  :: sigma
        type(linear_fcn_t), intent(OUT) :: linear_fcn

        real(wp) :: S
        real(wp) :: Sx
        real(wp) :: Sy
        real(wp) :: Sxx
        real(wp) :: Sxy
        real(wp) :: Syy

        real(wp) :: delta
        real(wp) :: delta_inv
        integer  :: n_pts

        real(wp) :: r2_tmp
        real(wp) :: y_fit(SIZE(x_data))
        real(wp) :: error

        real(wp) :: x_j
        real(wp) :: y_j

        integer :: j

        if (SIZE(x_data) /= SIZE(y_data)) then
            ERRORMSG("Need at least two data points")
            stop
        end if

        n_pts = SIZE(x_data)
        if (n_pts < 2) then
            ERRORMSG("Need at least two data points")
            stop
        end if

        if (sigma < 0.0_wp) then
            ERRORMSG("sigma must be non-negative")
            stop
        end if
     
        !a random comment     
     
        S   = n_pts
        Sx  = 0.0_wp
        Sy  = 0.0_wp
        Sxx = 0.0_wp
        Sxy = 0.0_wp
        Syy = 0.0_wp
        do j = 1, n_pts
            x_j = x_data(j)
            y_j = y_data(j)

            Sx  = Sx  + x_j
            Sy  = Sy  + y_j
            Sxx = Sxx + x_j * x_j
            Sxy = Sxy + x_j * y_j
            Syy = Syy + y_j * y_j
        end do

        ! Error distribution same for all x values
        delta = S*Sxx - Sx*Sx
        if (delta == 0.0_wp) then
            ERRORMSG("Cannot do linear least-sqrs.  Divide by zero.")
            stop
        end if
        delta_inv = 1.0_wp / delta

        ! Model parameters
        linear_fcn%a1 = (S  *Sxy - Sx*Sy ) * delta_inv;
        linear_fcn%a0 = (Sxx*Sy  - Sx*Sxy) * delta_inv;

        ! Error associated with estimation of model parameters
        linear_fcn%sigma_a1 = SQRT(n_pts * sigma * sigma * delta_inv);
        linear_fcn%sigma_a0 = SQRT(Sxx   * sigma * sigma * delta_inv);

        ! R2 goodness of fit
        r2_tmp = (S*Sxx - Sx*Sx) * (S*Syy - Sy*Sy)
        if (r2_tmp == 0.0_wp) then
            ERRORMSG("Cannot do linear least-sqrs R2.  Divide by zero.")
            stop
        end if
        linear_fcn%r2 = (S*Sxy - Sx*Sy) * (S*Sxy - Sx*Sy) / r2_tmp

        ! Reduced chi-squared goodness of fit
        CALL linear_fcn%evaluate(x_data, y_fit)
        error = 0.0_wp
        do j = 1, n_pts
            error = error + (y_data(j) - y_fit(j)) ** 2
        end do

        if ((n_pts > 2) .AND. (sigma > 0.0_wp)) then
            linear_fcn%chi2 = error / (sigma * sigma * (n_pts - 2))
        else
            linear_fcn%chi2 = 1.0_wp
        end if

    end subroutine from_least_sqrs_uniform

    !---------------------------------------------------------------------------
    ! DESCRIPTION:
    !> @brief
    !> Create a linear fit to given data using the direct linear least-squares
    !> method. This should be applied to data where the estimated error
    !> for measuring data at the different x_data values can be different.
    !
    !> @author Jared O'Neal
    !
    ! TODO:
    !  Nothing
    !
    !> @param[in] x_data - the points in the domain at which we evaluate
    !> @param[in] y_data - the results of the function evaluation
    !> @param[in] sigma - the estimated measurement error at each x value
    !> @returns A linear function object that contains the fit results.
    !---------------------------------------------------------------------------
    subroutine from_least_sqrs_nonuniform(x_data, y_data, sigma, linear_fcn)
        real(wp),           intent(IN)  :: x_data(:)
        real(wp),           intent(IN)  :: y_data(:)
        real(wp),           intent(IN)  :: sigma(:)
        type(linear_fcn_t), intent(OUT) :: linear_fcn

        real(wp) :: S
        real(wp) :: Sx
        real(wp) :: Sy
        real(wp) :: Sxx
        real(wp) :: Sxy
        real(wp) :: Syy

        real(wp) :: delta
        real(wp) :: delta_inv
        integer  :: n_pts

        real(wp) :: x_j
        real(wp) :: y_j
        real(wp) :: sigma_j_sqr_inv

        real(wp) :: r2_tmp
        real(wp) :: y_fit(SIZE(x_data))
        real(wp) :: error

        integer :: j

        if      (SIZE(x_data) /= SIZE(y_data)) then
            ERRORMSG("x and y data must be same length")
            stop
        else if (SIZE(x_data) /= SIZE(sigma)) then
            ERRORMSG("x and sigma data must be same length")
            stop
        end if

        n_pts = SIZE(x_data)
        if (n_pts < 2) then
            ERRORMSG("Need at least two data points")
            stop
        end if

        S   = 0.0_wp
        Sx  = 0.0_wp
        Sy  = 0.0_wp
        Sxx = 0.0_wp
        Sxy = 0.0_wp
        Syy = 0.0_wp
        do j = 1, n_pts
            x_j = x_data(j)
            y_j = y_data(j)

            if (sigma(j) <= 0.0_wp) then
                ERRORMSG("Sigma values must be positive")
                stop
            end if
            sigma_j_sqr_inv = 1.0_wp / (sigma(j) * sigma(j))

            S   = S   + sigma_j_sqr_inv
            Sx  = Sx  + sigma_j_sqr_inv * x_j
            Sy  = Sy  + sigma_j_sqr_inv * y_j
            Sxx = Sxx + sigma_j_sqr_inv * x_j * x_j
            Sxy = Sxy + sigma_j_sqr_inv * x_j * y_j
            Syy = Syy + sigma_j_sqr_inv * y_j * y_j
        end do

        ! Error distribution same for all x values
        delta = S*Sxx - Sx*Sx
        if (delta == 0.0_wp) then
            ERRORMSG("Cannot do linear least-sqrs.  Divide by zero.")
            stop
        end if
        delta_inv = 1.0_wp / delta

        ! Model parameters
        linear_fcn%a1 = (S  *Sxy - Sx*Sy ) * delta_inv;
        linear_fcn%a0 = (Sxx*Sy -  Sx*Sxy) * delta_inv;

        ! Error associated with estimation of model parameters
        linear_fcn%sigma_a1 = SQRT(S   * delta_inv);
        linear_fcn%sigma_a0 = SQRT(Sxx * delta_inv);

        ! R2 goodness of fit
        r2_tmp = (S*Sxx - Sx*Sx) * (S*Syy - Sy*Sy)
        if (r2_tmp == 0.0_wp) then
            write(stderr,*) "Cannot do linear least-sqrs R2.  Divide by zero."
            stop
        end if
        linear_fcn%r2 = (S*Sxy - Sx*Sy) * (S*Sxy - Sx*Sy) / r2_tmp

        ! Reduced chi-squared goodness of fit
        CALL linear_fcn%evaluate(x_data, y_fit)
        error = 0.0_wp
        do j = 1, n_pts
            error = error + ( (y_data(j) - y_fit(j)) / sigma(j) ) ** 2
        end do

        if (n_pts <= 2) then
            linear_fcn%chi2 = 1.0_wp
        else
            linear_fcn%chi2 = error / (n_pts - 2)
        end if

    end subroutine from_least_sqrs_nonuniform

end module linear_fcn_class

