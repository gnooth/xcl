/* mpfr_erf -- error function of a floating-point number

Copyright 2001, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
Contributed by Ludovic Meunier and Paul Zimmermann.

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

#define EXP1 2.71828182845904523536 /* exp(1) */

static int mpfr_erf_0 (mpfr_ptr, mpfr_srcptr, double, mp_rnd_t);

int
mpfr_erf (mpfr_ptr y, mpfr_srcptr x, mp_rnd_t rnd_mode)
{
  double xf;
  int inex;
  MPFR_SAVE_EXPO_DECL (expo);

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                 ("y[%#R]=%R inexact=%d", y, y, inex));

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN (x))
        {
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;
        }
      else if (MPFR_IS_INF (x)) /* erf(+inf) = +1, erf(-inf) = -1 */
        return mpfr_set_si (y, MPFR_INT_SIGN (x), GMP_RNDN);
      else /* erf(+0) = +0, erf(-0) = -0 */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          return mpfr_set (y, x, GMP_RNDN); /* should keep the sign of x */
        }
    }

  /* now x is neither NaN, Inf nor 0 */

  xf = mpfr_get_d (x, GMP_RNDN);
  xf = xf * xf; /* xf ~ x^2 */

  /* use expansion at x=0 when e*x^2 <= n (target precision)
     otherwise use asymptotic expansion */
  MPFR_SAVE_EXPO_MARK (expo);

  if (MPFR_UNLIKELY (xf > LOG2 * ((double) MPFR_PREC (y))))
    /* |erf x| = 1 or 1- */
    {
      mp_rnd_t rnd2 = MPFR_IS_POS (x) ? rnd_mode : MPFR_INVERT_RND(rnd_mode);
      if (rnd2 == GMP_RNDN || rnd2 == GMP_RNDU)
        {
          inex = MPFR_INT_SIGN (x);
          mpfr_set_si (y, inex, rnd2);
        }
      else /* round to zero */
        {
          inex = -MPFR_INT_SIGN (x);
          mpfr_setmax (y, 0); /* warning: setmax keeps the old sign of y */
          MPFR_SET_SAME_SIGN (y, x);
        }
    }
  else  /* use Taylor */
    inex = mpfr_erf_0 (y, x, xf, rnd_mode);

  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (y, inex, rnd_mode);
}

/* return x*2^e */
static double
mul_2exp (double x, mp_exp_t e)
{
  if (e > 0)
    {
      while (e--)
        x *= 2.0;
    }
  else
    {
      while (e++)
        x /= 2.0;
    }

  return x;
}

/* evaluates erf(x) using the expansion at x=0:

   erf(x) = 2/sqrt(Pi) * sum((-1)^k*x^(2k+1)/k!/(2k+1), k=0..infinity)

   Assumes x is neither NaN nor infinite nor zero.
   Assumes also that e*x^2 <= n (target precision).
 */
static int
mpfr_erf_0 (mpfr_ptr res, mpfr_srcptr x, double xf2, mp_rnd_t rnd_mode)
{
  mp_prec_t n, m;
  mp_exp_t nuk, sigmak;
  double tauk;
  mpfr_t y, s, t, u;
  unsigned int k;
  int log2tauk;
  int inex;
  MPFR_ZIV_DECL (loop);

  n = MPFR_PREC (res); /* target precision */

  /* initial working precision */
  m = n + (mp_prec_t) (xf2 / LOG2) + 8 + MPFR_INT_CEIL_LOG2 (n);

  mpfr_init2 (y, m);
  mpfr_init2 (s, m);
  mpfr_init2 (t, m);
  mpfr_init2 (u, m);

  MPFR_ZIV_INIT (loop, m);
  for (;;)
    {
      mpfr_mul (y, x, x, GMP_RNDU); /* err <= 1 ulp */
      mpfr_set_ui (s, 1, GMP_RNDN);
      mpfr_set_ui (t, 1, GMP_RNDN);
      tauk = 0.0;

      for (k = 1; ; k++)
        {
          mpfr_mul (t, y, t, GMP_RNDU);
          mpfr_div_ui (t, t, k, GMP_RNDU);
          mpfr_div_ui (u, t, 2 * k + 1, GMP_RNDU);
          sigmak = MPFR_GET_EXP (s);
          if (k % 2)
            mpfr_sub (s, s, u, GMP_RNDN);
          else
            mpfr_add (s, s, u, GMP_RNDN);
          sigmak -= MPFR_GET_EXP(s);
          nuk = MPFR_GET_EXP(u) - MPFR_GET_EXP(s);

          if ((nuk < - (mp_exp_t) m) && ((double) k >= xf2))
            break;

          /* tauk <- 1/2 + tauk * 2^sigmak + (1+8k)*2^nuk */
          tauk = 0.5 + mul_2exp (tauk, sigmak)
            + mul_2exp (1.0 + 8.0 * (double) k, nuk);
        }

      mpfr_mul (s, x, s, GMP_RNDU);
      MPFR_SET_EXP (s, MPFR_GET_EXP (s) + 1);

      mpfr_const_pi (t, GMP_RNDZ);
      mpfr_sqrt (t, t, GMP_RNDZ);
      mpfr_div (s, s, t, GMP_RNDN);
      tauk = 4.0 * tauk + 11.0; /* final ulp-error on s */
      log2tauk = __gmpfr_ceil_log2 (tauk);

      if (MPFR_LIKELY (MPFR_CAN_ROUND (s, m - log2tauk, n, rnd_mode)))
        break;

      /* Actualisation of the precision */
      MPFR_ZIV_NEXT (loop, m);
      mpfr_set_prec (y, m);
      mpfr_set_prec (s, m);
      mpfr_set_prec (t, m);
      mpfr_set_prec (u, m);

    }
  MPFR_ZIV_FREE (loop);

  inex = mpfr_set (res, s, rnd_mode);

  mpfr_clear (y);
  mpfr_clear (t);
  mpfr_clear (u);
  mpfr_clear (s);

  return inex;
}
