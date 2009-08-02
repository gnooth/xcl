/* mpfr_cos -- cosine of a floating-point number

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

/* s <- 1 - r/2! + r^2/4! + ... + (-1)^l r^l/(2l)! + ...
   Assumes |r| < 1.
   Returns the index l0 of the last term (-1)^l r^l/(2l)!.
   The absolute error on s is at most 2 * l0 * 2^(-m).
*/
static int
mpfr_cos2_aux (mpfr_ptr s, mpfr_srcptr r)
{
  unsigned int l, b = 2;
  mp_exp_t prec, m = MPFR_PREC (s);
  mpfr_t t;

  MPFR_ASSERTD (MPFR_GET_EXP (r) <= 0);

  mpfr_init2 (t, m);

  /* First step for l==1 can be simplified,
     futhermore multiply by 1 is not efficient since it is an exact
     multiplication (mulhigh failed and we must do a complete mul) */
  mpfr_div_2ui (t, r, 1, GMP_RNDN); /* exact */
  mpfr_sub (s, __gmpfr_one, t, GMP_RNDD);
  MPFR_ASSERTD (MPFR_GET_EXP (s) == 0);        /* check 1/2 <= s < 1 */

  for (l = 2; MPFR_GET_EXP (t) + m >= 0; l++)
    {
      mpfr_mul (t, t, r, GMP_RNDU);                /* err <= (3l-1) ulp */
      mpfr_div_ui (t, t, (unsigned long) (2*l-1)*(2*l), GMP_RNDU);
                                                   /* err <= 3l ulp */
      MPFR_ASSERTD (MPFR_IS_POS (t));
      MPFR_ASSERTD (MPFR_IS_POS (s));
      if (l % 2 == 0)
        mpfr_add (s, s, t, GMP_RNDD);
      else
        mpfr_sub (s, s, t, GMP_RNDD);
      MPFR_ASSERTD (MPFR_GET_EXP (s) == 0);        /* check 1/2 <= s < 1 */
      /* err(s) <= l * 2^(-m) */
      if (MPFR_UNLIKELY (3 * l > (1U << b)))
        b++;
      /* now 3l <= 2^b, we want 3l*ulp(t) <= 2^(-m)
         i.e. b+EXP(t)-PREC(t) <= -m */
      prec = m + MPFR_GET_EXP (t) + b;
      if (MPFR_LIKELY (prec >= MPFR_PREC_MIN))
        mpfr_prec_round (t, prec, GMP_RNDN);
    }
  mpfr_clear (t);

  return l;
}

int
mpfr_cos (mpfr_ptr y, mpfr_srcptr x, mp_rnd_t rnd_mode)
{
  mp_prec_t K0, K, precy, m, k, l;
  int inexact;
  mpfr_t r, s;
  mp_exp_t exps, cancel = 0;
  MPFR_ZIV_DECL (loop);
  MPFR_SAVE_EXPO_DECL (expo);
  MPFR_GROUP_DECL (group);

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                 ("y[%#R]=%R inexact=%d", y, y, inexact));

  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN (x) || MPFR_IS_INF (x))
        {
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;
        }
      else
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          return mpfr_set_ui (y, 1, GMP_RNDN);
        }
    }

  MPFR_SAVE_EXPO_MARK (expo);

  /* cos(x) = 1-x^2/2 + ..., so error < 2^(2*EXP(x)-1) */
  MPFR_FAST_COMPUTE_IF_SMALL_INPUT (y, __gmpfr_one, 0-2*MPFR_GET_EXP (x)+1,0,
                                    rnd_mode, inexact = _inexact; goto end);

  /* Compute initial precision */
  precy = MPFR_PREC (y);
  /* We can choose everything we want for K0.
     This formula has been created by trying many things...
     and is far from perfect */
  K0 = (MPFR_GET_EXP (x) > 0) ? (MPFR_GET_EXP (x)) : 0 ;
  K0 = __gmpfr_isqrt (precy / (2+2*K0+MPFR_INT_CEIL_LOG2 (precy)/4) );
  m = precy + 3*K0 + 4;
  if (MPFR_GET_EXP (x) >= 0)
    m += 5*MPFR_GET_EXP (x);
  else
    m += -MPFR_GET_EXP (x);

  MPFR_GROUP_INIT_2 (group, m, r, s);
  MPFR_ZIV_INIT (loop, m);
  for (;;)
    {
      mpfr_mul (r, x, x, GMP_RNDU); /* err <= 1 ulp */

      /* we need that |r| < 1 for mpfr_cos2_aux, i.e. up(x^2)/2^(2K) < 1 */
      K = K0 + MAX (MPFR_GET_EXP (r), 0);

      /*mpfr_div_2ui (r, r, 2 * K, GMP_RNDN); r = (x/2^K)^2, err <= 1 ulp */
      MPFR_SET_EXP (r, MPFR_GET_EXP (r)-2*K); /* Can't overflow! */

      /* s <- 1 - r/2! + ... + (-1)^l r^l/(2l)! */
      l = mpfr_cos2_aux (s, r);
      MPFR_SET_ONE (r);
      for (k = 0; k < K; k++)
        {
          mpfr_mul (s, s, s, GMP_RNDU);       /* err <= 2*olderr */
          MPFR_SET_EXP (s, MPFR_GET_EXP (s)+1); /* Can't overflow */
          mpfr_sub (s, s, r, GMP_RNDN);       /* err <= 4*olderr */
          MPFR_ASSERTD (MPFR_GET_EXP (s) <= 1);
        }

      /* absolute error on s is bounded by (2l+1/3)*2^(2K-m)
         2l+1/3 <= 2l+1 */
      k = MPFR_INT_CEIL_LOG2 (2*l+1) + 2*K;
      /* now the error is bounded by 2^(k-m) = 2^(EXP(s)-err) */

      exps = MPFR_GET_EXP (s);
      if (MPFR_LIKELY (MPFR_CAN_ROUND (s, exps + m - k, precy, rnd_mode)))
        break;

      if (MPFR_UNLIKELY (exps == 1))
        /* s = 1 or -1, and except x=0 which was
           already checked above, cos(x) cannot
           be 1 or -1, so we can round */
        {
          if (exps + m - k > precy
              /* if round to nearest or away, result is s,
                 otherwise it is round(nexttoward (s, 0)) */
              && MPFR_IS_LIKE_RNDZ (rnd_mode, MPFR_IS_NEG (s)))
            mpfr_nexttozero (s);
          break;
        }

      if (exps < cancel)
        {
          m += cancel - exps;
          cancel = exps;
        }

      MPFR_ZIV_NEXT (loop, m);
      MPFR_GROUP_REPREC_2 (group, m, r, s);
    }
  MPFR_ZIV_FREE (loop);
  inexact = mpfr_set (y, s, rnd_mode);
  MPFR_GROUP_CLEAR (group);

 end:
  MPFR_SAVE_EXPO_FREE (expo);
  MPFR_RET (mpfr_check_range (y, inexact, rnd_mode));
}

