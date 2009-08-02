/* mpfr_sin_cos -- sine and cosine of a floating-point number

Copyright 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

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

/* (y, z) <- (sin(x), cos(x)), return value is 0 iff both results are exact
   ie, iff x = 0 */
int
mpfr_sin_cos (mpfr_ptr y, mpfr_ptr z, mpfr_srcptr x, mp_rnd_t rnd_mode)
{
  mp_prec_t prec, m;
  int neg;
  mpfr_t c, k;
  mp_exp_t e;
  MPFR_ZIV_DECL (loop);

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN(x) || MPFR_IS_INF(x))
        {
          MPFR_SET_NAN (y);
          MPFR_SET_NAN (z);
          MPFR_RET_NAN;
        }
      else /* x is zero */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          MPFR_SET_ZERO (y);
          MPFR_SET_SAME_SIGN (y, x);
          mpfr_set_ui (z, 1, GMP_RNDN);
          MPFR_RET (0);
        }
    }

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                  ("sin[%#R]=%R cos[%#R]=%R", y, y, z, z));

  prec = MAX (MPFR_PREC (y), MPFR_PREC (z));
  m = prec + MPFR_INT_CEIL_LOG2 (prec) + 13;
  e = MPFR_GET_EXP (x);
  m += (e < 0) ? -2*e : e;

  mpfr_init2 (c, m);

  /* first determine sign of sinus */
  if (MPFR_GET_EXP (x) > 0)
    {
      mpfr_init2 (k, m);
      mpfr_const_pi (c, GMP_RNDN);
      mpfr_mul_2ui (c, c, 1, GMP_RNDN);    /* 2*Pi */
      mpfr_div (k, x, c, GMP_RNDN);        /* x/(2*Pi) */
      mpfr_floor (k, k);                   /* floor(x/(2*Pi)) */
      mpfr_mul (c, k, c, GMP_RNDN);
      mpfr_sub (k, x, c, GMP_RNDN);        /* 0 <= k < 2*Pi */
      mpfr_const_pi (c, GMP_RNDN);         /* PI is cached */
      neg = mpfr_cmp (k, c) > 0;
      mpfr_clear (k);
    }
  else
    neg = MPFR_IS_NEG (x);

  MPFR_ZIV_INIT (loop, m);
  for (;;)
    {
      mpfr_cos (c, x, GMP_RNDZ);
      if (!mpfr_can_round (c, m, GMP_RNDZ, rnd_mode, MPFR_PREC (z)))
        goto next_step;
      mpfr_set (z, c, rnd_mode);
      mpfr_sqr (c, c, GMP_RNDU);
      mpfr_ui_sub (c, 1, c, GMP_RNDN);
      e = 2 + (- MPFR_GET_EXP (c)) / 2;
      mpfr_sqrt (c, c, GMP_RNDN);
      if (neg)
        MPFR_CHANGE_SIGN (c);

      /* the absolute error on c is at most 2^(e-m) = 2^(EXP(c)-err) */
      e = MPFR_GET_EXP (c) + m - e;
      if (mpfr_can_round (c, e, GMP_RNDN, rnd_mode, MPFR_PREC (y)))
        break;
      /* check for huge cancellation */
      if (e < (mp_exp_t) MPFR_PREC (y))
        m += MPFR_PREC (y) - e;
      /* Check if near 1 */
      if (MPFR_GET_EXP (c) == 1
          && MPFR_MANT (c)[MPFR_LIMB_SIZE (c)-1] == MPFR_LIMB_HIGHBIT)
        m = 2*m;

    next_step:
      MPFR_ZIV_NEXT (loop, m);
      mpfr_set_prec (c, m);
    }
  MPFR_ZIV_FREE (loop);

  mpfr_set (y, c, rnd_mode);

  mpfr_clear (c);

  MPFR_RET (1); /* Always inexact */
}
