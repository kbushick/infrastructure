#include <iostream>
#include <vector>
#include <cmath>

#include "LinearFunction.h"

int assertAlmostEqual(const double lhs, const double rhs, const double tolerance) {
    using namespace std;

    if (abs(lhs - rhs) > tolerance) {
        cout << lhs << " != "<< rhs << " within tolerance " << tolerance << "\n";
        return 1;
    }

    return 0;
}

int testEvaluate(void) {
    using namespace std;

    const double A1 = -4.1234;
    const double A0 = 10.5432;

    vector<double> x_data(4);
    vector<double> y_data(x_data.size());
    x_data[0] = -1.1;
    x_data[1] =  0.0;
    x_data[2] =  2.2;
    x_data[3] = 10.4;
    
    vector<double> y_expected(x_data.size());
    y_expected[0] =  15.07894;
    y_expected[1] =  10.5432;
    y_expected[2] =   1.47172;
    y_expected[3] = -32.34016;

    int nFailed = 0;
    LinearFunction fcn(0.0, 0.0);

    fcn = LinearFunction(A0, A1);
    y_data = fcn.evaluate(x_data);
    for (size_t j=0; j<x_data.size(); ++j) {
        nFailed += assertAlmostEqual(y_data[j], y_expected[j], 1.0e-14);
    }
 
    return nFailed;
}   // end testEvaluate()

int ohmsLaw(void) {
    using namespace std;

    const double A0       = 0.0714;
    const double A1       = 0.0262;
    const double A0_ERROR = 0.036;
    const double A1_ERROR = 0.00065;
    const double CHI2     = 1.95 / 7.0;
    const double R2       = 0.9994 * 0.9994;

	vector<double> x = vector<double>(9);
	vector<double> y = vector<double>(x.size());
	vector<double> sigma = vector<double>(x.size());

	// Data Taken From "Data Reduction & Error Analysis for the Physical Sciences",
	//		Bevington, Philip & D. Keith Richardson.  Second Edtn, Pp. 97.
	//		ISBN:  0-07-911243-9
	x[0] = 10;	y[0] = 0.37;	sigma[0] = 0.05;
	x[1] = 20;	y[1] = 0.58;	sigma[1] = 0.05;
	x[2] = 30;	y[2] = 0.83;	sigma[2] = 0.05;
	x[3] = 40;	y[3] = 1.15;	sigma[3] = 0.05;
	x[4] = 50;	y[4] = 1.36;	sigma[4] = 0.05;
	x[5] = 60;	y[5] = 1.62;	sigma[5] = 0.05;
	x[6] = 70;	y[6] = 1.9;		sigma[6] = 0.05;
	x[7] = 80;	y[7] = 2.18;	sigma[7] = 0.05;
	x[8] = 90;	y[8] = 2.45;	sigma[8] = 0.05;

	int nFailed = 0;
    LinearFunction	fitU = LinearFunction::from_least_squares(x, y, sigma[0]);
    nFailed += assertAlmostEqual(fitU.a0(),         A0,       5.0e-5);
    nFailed += assertAlmostEqual(fitU.a1(),         A1,       5.0e-5);
    nFailed += assertAlmostEqual(fitU.a0Error(),    A0_ERROR, 5.0e-4);
    nFailed += assertAlmostEqual(fitU.a1Error(),    A1_ERROR, 7.5e-6);
    nFailed += assertAlmostEqual(fitU.rSquared(),   R2,       5.0e-5);
    nFailed += assertAlmostEqual(fitU.chiSquared(), CHI2,     7.5e-5);

    LinearFunction	fitN = LinearFunction::from_least_squares(x, y, sigma);
    nFailed += assertAlmostEqual(fitN.a0(),         fitU.a0(),         5.0e-15);
    nFailed += assertAlmostEqual(fitN.a1(),         fitU.a1(),         5.0e-15);
    nFailed += assertAlmostEqual(fitN.a0Error(),    fitU.a0Error(),    5.0e-15);
    nFailed += assertAlmostEqual(fitN.a1Error(),    fitU.a1Error(),    5.0e-15);
    nFailed += assertAlmostEqual(fitN.rSquared(),   fitU.rSquared(),   5.0e-15);
    nFailed += assertAlmostEqual(fitN.chiSquared(), fitU.chiSquared(), 5.0e-15);

	return nFailed;
} // end ohmsLaw()

int decayRateVsDistance(void) {
    using namespace std;

    const double A0       = 119.5;
    const double A1       = 30.7;
    const double A0_ERROR = 7.6;
    const double A1_ERROR = 1.1;
    const double CHI2     = 11.1 / 8.0;
    const double R2       = 0.9938 * 0.9938;

	vector<double> x = vector<double>(10);
	vector<double> y = vector<double>(x.size());
	vector<double> sigma = vector<double>(x.size());

	// Data Taken From "Data Reduction & Error Analysis for the Physical Sciences",
	//		Bevington, Philip & D. Keith Richardson.  Second Edtn, Pp. 97.
	//		ISBN:  0-07-911243-9
	x[0] = 10;	y[0] = 0.37;	sigma[0] = 0.05;
	// Data Taken From "Data Reduction & Error Analysis for the Physical Sciences",
	//		Bevington, Philip & D. Keith Richardson.  Second Edtn, Pp. 98.
	//		ISBN:  0-07-911243-9
	x[0] = 25.00;	y[0] = 901;	sigma[0] = 30;
	x[1] = 16.00;	y[1] = 652;	sigma[1] = 25.5;
	x[2] = 11.11;	y[2] = 443;	sigma[2] = 21;
	x[3] = 8.16;	y[3] = 339;	sigma[3] = 18.4;
	x[4] = 6.25;	y[4] = 283;	sigma[4] = 16.8;
	x[5] = 4.94;	y[5] = 281;	sigma[5] = 16.8;
	x[6] = 4.00;	y[6] = 240;	sigma[6] = 15.5;
	x[7] = 2.78;	y[7] = 220;	sigma[7] = 14.8;
	x[8] = 1.78;	y[8] = 180;	sigma[8] = 13.4;
	x[9] = 1.00;	y[9] = 154;	sigma[9] = 12.4;

	int nFailed = 0;
    LinearFunction	fit = LinearFunction::from_least_squares(x, y, sigma);
    nFailed += assertAlmostEqual(fit.a0(),         A0,       5.0e-2);
    nFailed += assertAlmostEqual(fit.a1(),         A1,       2.5e-3);
    nFailed += assertAlmostEqual(fit.a0Error(),    A0_ERROR, 7.5e-2);
    nFailed += assertAlmostEqual(fit.a1Error(),    A1_ERROR, 7.5e-2);
    nFailed += assertAlmostEqual(fit.rSquared(),   R2,       2.5e-4);
    nFailed += assertAlmostEqual(fit.chiSquared(), CHI2,     5.0e-2);

	return nFailed;
} // end ohmsLaw()

int main (int argc, char * const argv[]) {
	using namespace	std;

    int nFailed = 0;
    nFailed += testEvaluate();
	nFailed += ohmsLaw();
    nFailed  += decayRateVsDistance();

    if (nFailed == 0) {
        cout << "SUCCESS" << endl;
    } else {
        cout << nFailed << " tests failed\n";
        cout << "FAILURE" << endl;
    }

    return nFailed;
}	// end main()

