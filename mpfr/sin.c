/* mpfr_sin -- sine of a floating-point number

Copyright 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

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

#define MPFR_NEED_LONGLONG_H
#include "mpfr-impl.h"

/* determine the sign of sin(x) using argument reduction.
   Assumes x is not an exact multiple of Pi (this excludes x=0). */
static int
mpfr_sin_sign (mpfr_srcptr x)
{
  mpfr_t c, k;
  mp_exp_t K;
  int sign;
  mp_prec_t m;
  mpfr_srcptr y;
  MPFR_ZIV_DECL (loop);

  K = MPFR_GET_EXP(x);
  if (K < 0)  /* Trivial case if ABS(x) < 1 */
    return MPFR_SIGN (x);

  m = K + BITS_PER_MP_LIMB;
  mpfr_init2 (c, m);
  mpfr_init2 (k, m);

  MPFR_ZIV_INIT (loop, m);
  for (;;)
    {
      /* first determine round(x/Pi): does not have to be exact since
         the result is an integer */
      mpfr_const_pi (c, GMP_RNDN); /* err <= 1/2*ulp(c) = 2^(1-m) */
      /* we need that k is not-to-badly rounded to an integer,
         i.e. ulp(k) <= 1, so m >= EXP(k). */
      mpfr_div (k, x, c, GMP_RNDN);
      mpfr_round (k, k);

      sign = 1;

      if (!MPFR_IS_ZERO (k)) /* subtract k*approx(Pi) */
        {
          /* determine parity of k for sign */
          if (MPFR_GET_EXP (k) <= 0 || (mpfr_uexp_t) MPFR_GET_EXP (k) <= m)
            {
              mp_size_t j = BITS_PER_MP_LIMB * MPFR_LIMB_SIZE(k)
                - MPFR_GET_EXP(k);
              mp_size_t l = j / BITS_PER_MP_LIMB;
              /* parity bit is j-th bit starting from least significant bits */
              if ((MPFR_MANT(k)[l] >> (j % BITS_PER_MP_LIMB)) & 1)
                sign = -1; /* k is odd */
            }
          K = MPFR_GET_EXP (k); /* k is an integer, thus K >= 1, k < 2^K */
          mpfr_mul (k, k, c, GMP_RNDN); /* err <= oldk*err(c) + 1/2*ulp(k)
                                               <= 2^(K+2-m) */
          mpfr_sub (k, x, k, GMP_RNDN);
          /* assuming |k| <= Pi, err <= 2^(1-m)+2^(K+2-m) < 2^(K+3-m) */
          MPFR_ASSERTN (MPFR_IS_ZERO (k) || MPFR_GET_EXP (k) <= 2);
          y = k;
        }
      else
        {
          K = 1;
          y = x;
        }
      /* sign of sign(y) is uncertain if |y| <= err < 2^(K+3-m),
         thus EXP(y) < K+4-m */
      if (MPFR_LIKELY (!MPFR_IS_ZERO (y)
                       && MPFR_GET_EXP (y) >= K + 4 - (mp_exp_t) m))
        break;
      MPFR_ZIV_NEXT (loop, m);
      mpfr_set_prec (c, m);
      mpfr_set_prec (k, m);
    }

  if (MPFR_IS_NEG (y))
    sign = -sign;

  mpfr_clear (k);
  mpfr_clear (c);

  return sign;
}

int
mpfr_sin (mpfr_ptr y, mpfr_srcptr x, mp_rnd_t rnd_mode)
{
  mpfr_t c;
  mp_exp_t e;
  mp_prec_t precy, m;
  int inexact, sign;
  MPFR_ZIV_DECL (loop);

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                  ("y[%#R]=%R inexact=%d", y, y, inexact));

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN (x) || MPFR_IS_INF (x))
        {
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;

        }
      else /* x is zero */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          MPFR_SET_ZERO (y);
          MPFR_SET_SAME_SIGN (y, x);
          MPFR_RET (0);
        }
    }

  /* sin(x) = x - x^3/6 + ... so the error is < 2^(3*EXP(x)-2) */
  MPFR_FAST_COMPUTE_IF_SMALL_INPUT (y, x, -2*MPFR_GET_EXP (x)+2,0,rnd_mode,{});

  /* Compute initial precision */
  precy = MPFR_PREC (y);
  m = precy + MPFR_INT_CEIL_LOG2 (precy) + 13;
  e = MPFR_GET_EXP (x);
  m += (e < 0) ? -2*e : e;

  sign = mpfr_sin_sign (x);
  mpfr_init2 (c, m);

  MPFR_ZIV_INIT (loop, m);
  for (;;)
    {
      mpfr_cos (c, x, GMP_RNDZ);    /* can't be exact */
      mpfr_nexttoinf (c);           /* now c = cos(x) rounded away */
      mpfr_mul (c, c, c, GMP_RNDU); /* away */
      mpfr_ui_sub (c, 1, c, GMP_RNDZ);
      mpfr_sqrt (c, c, GMP_RNDZ);
      if (MPFR_IS_NEG_SIGN(sign))
        MPFR_CHANGE_SIGN(c);

      /* Warning c may be 0 ! */
      if (MPFR_UNLIKELY (MPFR_IS_ZERO (c)))
        {
          /* Huge cancellation: increase prec a lot! */
          m = MAX (m, MPFR_PREC (x));
          m = 2*m;
        }
      else
        {
          /* the absolute error on c is at most 2^(3-m-EXP(c)) */
          e = 2 * MPFR_GET_EXP (c) + m - 3;
          if (mpfr_can_round (c, e, GMP_RNDN, GMP_RNDZ,
                              precy + (rnd_mode == GMP_RNDN)))
            /* WARNING: even if we know c <= sin(x), don't give GMP_RNDZ
               as 3rd argument to mpfr_can_round, since if c is exactly
               representable to the target precision (inexact = 0 below),
               we would have to add one ulp when rounding away from 0. */
            break;

          /* check for huge cancellation (Near 0) */
          if (e < (mp_exp_t) MPFR_PREC (y))
            m += MPFR_PREC (y) - e;
          /* Check if near 1 */
          if (MPFR_GET_EXP (c) == 1)
            m += m;
        }

      /* Else generic increase */
      MPFR_ZIV_NEXT (loop, m);
      mpfr_set_prec (c, m);
    }
  MPFR_ZIV_FREE (loop);

  inexact = mpfr_set (y, c, rnd_mode);
  /* inexact cannot be 0, since this would mean that c was representable
     within the target precision, but in that case mpfr_can_round will fail */

  mpfr_clear (c);

  return inexact; /* inexact */
}
