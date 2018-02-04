/**
 * \file    LinearFunction.h
 *
 * \details A class that encapsulates linear functions and allows for creating
 *          these with direct linear least squares.
 *
 * \author  Jared O'Neal
 * \version 1.0
 */

#ifndef LINEAR_FUNCTION_H__
#define LINEAR_FUNCTION_H__

#include <vector>

class LinearFunction {
public:
    LinearFunction(const double a0, const double a1);
    ~LinearFunction(void) {};

	double a0(void) const { return _a0; }
	double a1(void) const { return _a1; }
	double a0Error(void) const { return _a0Error; }
	double a1Error(void) const { return _a1Error; }
	double chiSquared(void) const { return _chiSquared; }
	double rSquared(void) const { return _rSquared; }

    double              evaluate(const double x) const;
    std::vector<double> evaluate(const std::vector<double>& x) const;

	static LinearFunction from_least_squares(const std::vector<double>& x, 
											 const std::vector<double>& y,
                                             const double sigma);	
	static LinearFunction from_least_squares(const std::vector<double>& x, 
											 const std::vector<double>& y,
                                             const std::vector<double>& sigma);	

private:
    double  _a0;
    double  _a1;
    double  _a0Error;
    double  _a1Error;
    double  _chiSquared;
    double  _rSquared;
};

#endif

