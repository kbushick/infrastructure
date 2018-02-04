/**
 * \file    LinearFunction.cpp
 *
 * \details A class that encapsulates linear functions and allows for creating
 *          these with direct linear least squares.  The function is defined by
 *  \f[
 *  f(x) = a_1x + a_0.
 *  \f]
 *
 * \author  Jared O'Neal
 */

#include "LinearFunction.h"

#include <cmath>
#include <iostream>
#include <stdexcept>

/**
 * \brief
 * Instantiate a lineaer function by setting its coefficients only.
 *
 * @param a0 - coefficient
 * @param a1 - coefficient
 */
LinearFunction::LinearFunction(const double a0, const double a1)
	: _a0(a0),
	  _a1(a1),
	  _a0Error(0.0),
	  _a1Error(0.0),
	  _rSquared(1.0),
      _chiSquared(1.0) {
}

/**
 * Evaluate the linear function at the given single point. 
 *
 * @param x - the point at which to evaluate
 * @returns the function value f(x)
 */
double LinearFunction::evaluate(const double x) const {
	return _a1*x + _a0;
}

/**
 * Evaluate the linear function at each of the given points.
 *
 * @param x - the point at which to evaluate
 * @returns the set of function evaluations
 */
std::vector<double> LinearFunction::evaluate(const std::vector<double>& x) const {
	std::vector<double> y(x.size());

	for (size_t j=0; j<x.size(); ++j) {
		y[j] = _a1*x[j] + _a0;
	}

	return y;
}

/**
 * Given a set of two or more data points, construct a LinearFunction object
 * that describes the linear least squared fit to the data.
 *
 * @param x - the independent variable portion of the data points
 * @param y - the dependent variable portion of the data points
 * @param sigma - the single error value associated to all data points
 * @returns the object
 */
LinearFunction LinearFunction::from_least_squares(const std::vector<double>& x,
											      const std::vector<double>& y,
                                                  const double sigma) {
	LinearFunction fit(0.0, 0.0);

	size_t nPts = x.size();
	if (y.size() != nPts) {
    	throw std::length_error("x & y must be same length"); 
    }
    if (sigma <= 0.0) {
        throw std::invalid_argument("Sigma must be positive");
    }

	double x_j = 0.0;
	double y_j = 0.0;

	double S = nPts; 
	double Sx = 0.0;
	double Sy = 0.0;
	double Sxx = 0.0;
	double Sxy = 0.0;
	double Syy = 0.0;
	for (size_t j=0; j<nPts; ++j) {
		x_j = x[j];
		y_j = y[j];

        Sx  += x_j;
        Sy  += y_j;
		Sxx += x_j * x_j;
        Sxy += x_j * y_j;
        Syy += y_j * y_j;
 	}

	double delta = S*Sxx - Sx*Sx;
	if (delta == 0.0) {
		throw std::runtime_error("Divide by zero");
	}
	double deltaInv = 1.0 / delta;

    fit._a1 = (S*Sxy  - Sx*Sy)  * deltaInv;
    fit._a0 = (Sxx*Sy - Sx*Sxy) * deltaInv;
    fit._a0Error = sqrt(Sxx * sigma * sigma * deltaInv);
	fit._a1Error = sqrt(S   * sigma * sigma * deltaInv);

	double r2Tmp = (S*Sxx - Sx*Sx)*(S*Syy - Sy*Sy);
	if (r2Tmp == 0.0) {
		throw std::runtime_error("Divide by zero");
	}
    fit._rSquared = (S*Sxy - Sx*Sy)*(S*Sxy - Sx*Sy) / r2Tmp;

	double error = 0.0;
	std::vector<double> y_fit = fit.evaluate(x);
	for (size_t j=0; j<nPts; ++j) {
		error += (y[j] - y_fit[j]) * (y[j] - y_fit[j]);
    }

	if ((nPts > 2) && (sigma != 0.0)) {
		fit._chiSquared = error / (sigma * sigma * (nPts - 2));
	} else {
		fit._chiSquared = 1.0;
	}

	return fit;
}

/**
 * Given a set of two or more data points, construct a LinearFunction object
 * that describes the linear least squared fit to the data.
 *
 * @param x - the independent variable portion of the data points
 * @param y - the dependent variable portion of the data points
 * @param sigma - the single error value associated to all data points
 * @returns the object
 */
LinearFunction LinearFunction::from_least_squares(const std::vector<double>& x,
											      const std::vector<double>& y,
                                                  const std::vector<double>& sigma) {

	size_t nPts = x.size();
	if (y.size() != nPts) {
    	throw std::length_error("x & y must be same length"); 
    } else if (sigma.size() != nPts) {
    	throw std::length_error("x & sigma must be same length"); 
    }

	LinearFunction fit(0.0, 0.0);

	double x_j = 0.0;
	double y_j = 0.0;
    double sigma_j_sqr_inv = 0.0;
	
    double S   = 0.0; 
	double Sx  = 0.0;
	double Sy  = 0.0;
	double Sxx = 0.0;
	double Sxy = 0.0;
	double Syy = 0.0;
	for (size_t j=0; j<nPts; ++j) {
		x_j = x[j];
		y_j = y[j];

        if (sigma[j] <= 0.0) {
            throw std::invalid_argument("Sigma values must be positive");
        }
        sigma_j_sqr_inv = 1.0 / (sigma[j] * sigma[j]);

        S   += sigma_j_sqr_inv;
        Sx  += sigma_j_sqr_inv * x_j;
        Sy  += sigma_j_sqr_inv * y_j; 
		Sxx += sigma_j_sqr_inv * x_j * x_j;
        Sxy += sigma_j_sqr_inv * x_j * y_j;
        Syy += sigma_j_sqr_inv * y_j * y_j;
 	}

	double delta = S*Sxx - Sx*Sx;
	if (delta == 0.0) {
		throw std::runtime_error("Divide by zero");
	}
	double deltaInv = 1.0 / delta;

    fit._a1 = (S  *Sxy  - Sx*Sy ) * deltaInv;
    fit._a0 = (Sxx*Sy   - Sx*Sxy) * deltaInv;
    fit._a0Error = sqrt(Sxx * deltaInv);
	fit._a1Error = sqrt(S   * deltaInv);

	double r2Tmp = (S*Sxx - Sx*Sx)*(S*Syy - Sy*Sy);
	if (r2Tmp == 0.0) {
		throw std::runtime_error("Divide by zero");
	}
    fit._rSquared = (S*Sxy - Sx*Sy)*(S*Sxy - Sx*Sy) / r2Tmp;

	std::vector<double> y_fit = fit.evaluate(x);
	double error = 0.0;
	for (size_t j=0; j<nPts; ++j) {
		error += (y[j] - y_fit[j]) * (y[j] - y_fit[j]) / (sigma[j] * sigma[j]);
    }

	if (nPts > 2) {
		fit._chiSquared = error / (nPts - 2);
	} else {
		fit._chiSquared = 1.0;
	}

	return fit;
}

