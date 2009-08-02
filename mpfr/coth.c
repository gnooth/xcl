/* mpfr_coth - Hyperbolic cotangent function.

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

/* the hyperbolic cotangent is defined by coth(x) = 1/tanh(x)
   coth (NaN) = NaN.
   coth (+Inf) = 1
   coth (-Inf) = -1
   coth (+0) = +0.
   coth (-0) = -0.
*/

#define FUNCTION mpfr_coth
#define INVERSE  mpfr_tanh
#define ACTION_NAN(y) do { MPFR_SET_NAN(y); MPFR_RET_NAN; } while (1)
#define ACTION_INF(y) return mpfr_set_si (y, MPFR_IS_POS(x) ? 1 : -1, GMP_RNDN)
#define ACTION_ZERO(y,x) do { MPFR_SET_SAME_SIGN(y,x); MPFR_SET_ZERO(y); \
                              MPFR_RET(0); } while (1)

/* We know |coth(x)| > 1, thus if the approximation z is such that
   1 <= z <= 1 + 2^(-p) where p is the target precision, then the
   result is either 1 or nextabove(1) = 1 + 2^(1-p). */
#define ACTION_SPECIAL                                                  \
  if (MPFR_GET_EXP(z) == 1) /* 1 <= |z| < 2 */                          \
    {                                                                   \
      /* the following is exact by Sterbenz theorem */                  \
      mpfr_sub_si (z, z, MPFR_SIGN(z) > 0 ? 1 : -1, GMP_RNDN);          \
      if (MPFR_IS_ZERO(z) || MPFR_GET_EXP(z) <= - (mp_exp_t) precy)     \
        {                                                               \
          mpfr_add_si (z, z, MPFR_SIGN(z) > 0 ? 1 : -1, GMP_RNDN);      \
          break;                                                        \
        }                                                               \
    }

#include "gen_inverse.h"
