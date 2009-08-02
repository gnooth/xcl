/* mpfr_csch - Hyperbolic cosecant function.

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

/* the hyperbolic cosecant is defined by csch(x) = 1/sinh(x).
   csch (NaN) = NaN.
   csch (+Inf) = +0.
   csch (-Inf) = -0.
   csch (+0) = +Inf.
   csch (-0) = -Inf.
*/

#define FUNCTION mpfr_csch
#define INVERSE  mpfr_sinh
#define ACTION_NAN(y) do { MPFR_SET_NAN(y); MPFR_RET_NAN; } while (1)
#define ACTION_INF(y) do { MPFR_SET_SAME_SIGN(y,x); MPFR_SET_ZERO (y); \
                           MPFR_RET(0); } while (1)
#define ACTION_ZERO(y,x) do { MPFR_SET_SAME_SIGN(y,x); MPFR_SET_INF(y); \
                              MPFR_RET(0); } while (1)

#include "gen_inverse.h"
