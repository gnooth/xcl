/* mpfr_sech - Hyperbolic secant function = 1/cosh.

Copyright 2005, 2006 Free Software Foundation, Inc.

This file is part of the MPFR Library.

The MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or (at your
option) any later version.

The MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the MPFR Library; see the file COPYING.LIB.  If not, write to
the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
MA 02110-1301, USA. */

/* The hyperbolic secant function is defined by sech(x)=1/cosh(x):
    csc (NaN) = NaN.
    csc (+Inf) = csc (-Inf) = 0+.
    csc (+0) = csc (-0) = 1.
 */

#define FUNCTION mpfr_sech
#define INVERSE  mpfr_cosh
#define ACTION_NAN(y) do { MPFR_SET_NAN(y); MPFR_RET_NAN; } while (1)
#define ACTION_INF(y) return mpfr_set_ui (y, 0, GMP_RNDN)
#define ACTION_ZERO(y,x) return mpfr_set_ui (y, 1, GMP_RNDN)

#include "gen_inverse.h"
