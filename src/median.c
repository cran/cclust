/* median.f -- translated by f2c (version 19960717).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "R_ext/f2c.h"

/* Subroutine */ double mdian1_(x, n)
double *x;
integer *n;
{
  double xmed;
  extern /* Subroutine */ int sort_();
    static integer n2;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    sort_(n, &x[1]);
    n2 = *n / 2;
    if (n2 << 1 == *n) {
	xmed = (x[n2] + x[n2 + 1]) * (float).5;
    } else {
	xmed = x[n2 + 1];
    }
    return xmed;
} /* mdian1_ */

/* Subroutine */ int sort_(n, ra)
integer *n;
double *ra;
{

    static integer i__, j, l, ir;
    static double rra;
    /* Parameter adjustments */
    --ra;
    /* Function Body */
    l = *n / 2 + 1;
    ir = *n;
L10:

    if (l > 1) {
	--l;
	rra = ra[l];
    } else {
	rra = ra[ir];
	ra[ir] = ra[1];
	--ir;
	if (ir == 1) {
	    ra[1] = rra;
	    return 0;
	}
    }
    i__ = l;
    j = l + l;
L20:
    if (j <= ir) {
	if (j < ir) {
	    if (ra[j] < ra[j + 1]) {
		++j;
	    }
	}
	if (rra < ra[j]) {
	    ra[i__] = ra[j];
	    i__ = j;
	    j += j;
	} else {
	    j = ir + 1;
	}
	goto L20;
    }
    ra[i__] = rra;
    goto L10;
} /* sort_ */

