/* mpfr_atan -- arc-tangent of a floating-point number

Copyright 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

This file is part of the MPFR Library, and was contributed by Mathieu Dutour.

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

/*
#define A
#define A1 1
#define A2 2
#define C
#define C1  3
#define C2  2
#define NO_FACTORIAL
#define GENERIC mpfr_atan_aux
#include "generic.c"
*/
/* This is the code of 'generic.c' slighty optimized for mpfr_atan
   Compute y = atan (p/2^r) using 2^m terms for the series expansion */
static void
mpfr_atan_aux (mpfr_ptr y, mpz_ptr p, long r, int m, mpz_t *tab)
{
  mpz_t *S, *T, *ptoj;
  mp_limb_t *d;
  unsigned long n, i, k, j, l;
  mp_exp_t diff, expo;
  int im;

  /* Set Tables */
  S    = tab;           /* S */
  ptoj = S + 1*(m+1);   /* p^2^j Precomputed table */
  T    = S + 2*(m+1);   /* Product of Odd integer  table  */

  /* From p to p^2 */
  mpz_mul (p, p, p);

  /* Normalize p */
  d = PTR (p);
  for (n = 0 ; MPFR_UNLIKELY (*d == 0) ; d++, n+= BITS_PER_MP_LIMB);
  MPFR_ASSERTD (*d != 0);
  count_trailing_zeros (im, *d);
  /* Simplify p/2^r */
  if (n+im > 0) {
    mpz_tdiv_q_2exp (p, p, n+im);
    MPFR_ASSERTD (r > n+im);
    r -= n+im;
  }

  MPFR_ASSERTD (mpz_sgn (p) > 0);
  MPFR_ASSERTD (m > 0);

  /* Check if P==1 (Special case) */
  l = 0;
  if (mpz_cmp_ui (p, 1) != 0) {
    /* P!= 1: Precomputed ptoj table */
    mpz_set (ptoj[0], p);
    for (im = 1 ; im < m ; im++)
      mpz_mul (ptoj[im], ptoj[im-1], ptoj[im-1]);
    /* Main loop */
    n = 1UL << m;
    for (i = k = 0; i < n; i+=2, k++) {
      mpz_set_ui (T[k+1], 1+2*i+2);
      mpz_mul_ui (S[k+1], p, 1+2*i);
      mpz_mul_2exp (S[k], T[k+1], r);
      mpz_sub (S[k], S[k], S[k+1]);
      mpz_mul_ui (T[k], T[k+1], 1+2*i);
      for (j = (i+2)>>1, l = 1; (j & 1) == 0; l++, j>>=1, k--) {
        MPFR_ASSERTD (k > 0);
        mpz_mul (S[k], S[k], ptoj[l]);
        mpz_mul (S[k], S[k], T[k-1]);
        mpz_mul (S[k-1], S[k-1], T[k]);
        mpz_mul_2exp (S[k-1], S[k-1], r<<l);
        mpz_add (S[k-1], S[k-1], S[k]);
        mpz_mul (T[k-1], T[k-1], T[k]);
      }
    }
  } else {
    n = 1UL << m;
    for (i = k = 0; i < n; i+=2, k++) {
      mpz_set_ui (T[k+1], 1+2*i+2);
      mpz_mul_2exp (S[k], T[k+1], r);
      mpz_sub_ui (S[k], S[k], 1+2*i);
      mpz_mul_ui (T[k], T[k+1], 1+2*i);
      for (j = (i+2)>>1, l = 1; (j & 1) == 0; l++, j>>=1, k--) {
        MPFR_ASSERTD (k > 0);
        mpz_mul (S[k], S[k], T[k-1]);
        mpz_mul (S[k-1], S[k-1], T[k]);
        mpz_mul_2exp (S[k-1], S[k-1], r<<l);
        mpz_add (S[k-1], S[k-1], S[k]);
        mpz_mul (T[k-1], T[k-1], T[k]);
      }
    }
  }

  MPFR_ASSERTD (l == m && i == n);
  MPFR_MPZ_SIZEINBASE2 (diff, S[0]);
  diff -= 2*MPFR_PREC (y);
  expo = diff + ((1<<m) - 1);
  if (diff >=0)
    mpz_tdiv_q_2exp (S[0], S[0], diff);
  else
    mpz_mul_2exp (S[0], S[0], -diff);

  MPFR_MPZ_SIZEINBASE2 (diff, T[0]);
  diff -= MPFR_PREC (y);
  expo -= (diff + n -1);
  if (diff >= 0)
    mpz_tdiv_q_2exp (T[0], T[0],diff);
  else
    mpz_mul_2exp (T[0], T[0],-diff);

  mpz_tdiv_q (S[0], S[0], T[0]);
  mpfr_set_z (y, S[0], GMP_RNDD);
  MPFR_SET_EXP (y, MPFR_EXP (y) + expo - r*(n-1) );
}

int
mpfr_atan (mpfr_ptr atan, mpfr_srcptr x, mp_rnd_t rnd_mode)
{
  mpfr_t xp, arctgt, sk, tmp, tmp2;
  mpz_t  ukz;
  mpz_t *tabz;
  mp_exp_t exptol;
  mp_prec_t prec, realprec;
  unsigned long twopoweri;
  int comparaison, inexact, inexact2;
  int i, n0, oldn0;
  MPFR_GROUP_DECL (group);
  MPFR_SAVE_EXPO_DECL (expo);
  MPFR_ZIV_DECL (loop);

  MPFR_LOG_FUNC (("x[%#R]=%R rnd=%d", x, x, rnd_mode),
                 ("atan[%#R]=%R inexact=%d", atan, atan, inexact));

  /* Singular cases */
  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (x)))
    {
      if (MPFR_IS_NAN (x))
        {
          MPFR_SET_NAN (atan);
          MPFR_RET_NAN;
        }
      else if (MPFR_IS_INF (x))
        {
          if (MPFR_IS_POS (x))  /* arctan(+inf) = Pi/2 */
            inexact = mpfr_const_pi (atan, rnd_mode);
          else /* arctan(-inf) = -Pi/2 */
            {
              inexact = -mpfr_const_pi (atan,
                                        MPFR_INVERT_RND (rnd_mode));
              MPFR_CHANGE_SIGN (atan);
            }
          inexact2 = mpfr_div_2ui (atan, atan, 1, rnd_mode);
          if (MPFR_UNLIKELY (inexact2))
            inexact = inexact2; /* An underflow occurs */
          MPFR_RET (inexact);
        }
      else /* x is necessarily 0 */
        {
          MPFR_ASSERTD (MPFR_IS_ZERO (x));
          MPFR_SET_ZERO (atan);
          MPFR_SET_SAME_SIGN (atan, x);
          MPFR_RET (0);
        }
    }

  /* atan(x) = x - x^3/3 + x^5/5...
     so the error is < 2^(3*EXP(x)-1)
     so `EXP(x)-(3*EXP(x)-1)` = -2*EXP(x)+1 */
  MPFR_FAST_COMPUTE_IF_SMALL_INPUT (atan,x, -2*MPFR_GET_EXP (x)+1,0,rnd_mode,{});

  /* Set x_p=|x| */
  MPFR_TMP_INIT_ABS (xp, x);

  /* Other simple case arctang(-+1)=-+pi/4 */
  comparaison = mpfr_cmp_ui (xp, 1);
  if (MPFR_UNLIKELY (comparaison == 0))
    {
      int neg = MPFR_IS_NEG (x);
      inexact = mpfr_const_pi (atan, MPFR_IS_POS (x) ? rnd_mode
                               : MPFR_INVERT_RND (rnd_mode));
      if (neg)
        {
          inexact = -inexact;
          MPFR_CHANGE_SIGN (atan);
        }
      inexact2 = mpfr_div_2ui (atan, atan, 2, rnd_mode);
      if (MPFR_UNLIKELY (inexact2))
        inexact = inexact2; /* an underflow occurs */
      return inexact;
    }

  realprec = MPFR_PREC (atan) + MPFR_INT_CEIL_LOG2 (MPFR_PREC (atan)) + 4;
  prec = realprec + BITS_PER_MP_LIMB;

  MPFR_SAVE_EXPO_MARK (expo);

  /* Initialisation    */
  mpz_init (ukz);
  MPFR_GROUP_INIT_4 (group, prec, sk, tmp, tmp2, arctgt);
  oldn0 = 0;
  tabz = (mpz_t *) 0;

  MPFR_ZIV_INIT (loop, prec);
  for (;;)
    {
      /* First, if |x| < 1, we need to have more prec to be able to round (sup)
         n0 = ceil(log(prec_requested + 2 + 1+ln(2.4)/ln(2))/log(2)) */
      mp_prec_t sup;
#if 0
      sup = 1;
      if (MPFR_GET_EXP (xp) < 0
          && (mpfr_uexp_t) (2-MPFR_GET_EXP (xp)) > realprec)
        sup = (mpfr_uexp_t) (2-MPFR_GET_EXP (xp)) - realprec;
#else
      sup = MPFR_GET_EXP (xp) < 0 ? 2-MPFR_GET_EXP (xp) : 1;
#endif
      n0 = MPFR_INT_CEIL_LOG2 ((realprec + sup) + 3);
      MPFR_ASSERTD (3*n0 > 2);
      prec = (realprec + sup) + 1 + MPFR_INT_CEIL_LOG2 (3*n0-2);

      /* Initialisation */
      MPFR_GROUP_REPREC_4 (group, prec, sk, tmp, tmp2, arctgt);
      if (MPFR_LIKELY (oldn0 == 0))
        {
          oldn0 = 3*(n0+1);
          tabz = (mpz_t *) (*__gmp_allocate_func) (oldn0*sizeof (mpz_t));
          for (i = 0; i < oldn0; i++)
            mpz_init (tabz[i]);
        }
      else if (MPFR_UNLIKELY (oldn0 < 3*n0+1))
        {
          tabz = (mpz_t *) (*__gmp_reallocate_func)
            (tabz, oldn0*sizeof (mpz_t), 3*(n0+1)*sizeof (mpz_t));
          for (i = oldn0; i < 3*(n0+1); i++)
            mpz_init (tabz[i]);
          oldn0 = 3*(n0+1);
        }

      if (comparaison > 0)
        mpfr_ui_div (sk, 1, xp, GMP_RNDN);
      else
        mpfr_set (sk, xp, GMP_RNDN);

      /* sk is 1/|x| if |x| > 1, and |x| otherwise, i.e. min(|x|, 1/|x|) */

      /* Assignation  */
      MPFR_SET_ZERO (arctgt);
      twopoweri = 1<<0;
      MPFR_ASSERTD (n0 >= 4);
      for (i = 0 ; i < n0; i++)
        {
          if (MPFR_UNLIKELY (MPFR_IS_ZERO (sk)))
            break;
          /* Calculation of trunc(tmp) --> mpz */
          mpfr_mul_2ui (tmp, sk, twopoweri, GMP_RNDN);
          mpfr_trunc (tmp, tmp);
          if (!MPFR_IS_ZERO (tmp))
            {
              exptol = mpfr_get_z_exp (ukz, tmp);
              /* since the s_k are decreasing (see algorithms.tex),
                 and s_0 = min(|x|, 1/|x|) < 1, we have sk < 1,
                 thus exptol < 0 */
              MPFR_ASSERTD (exptol < 0);
              mpz_tdiv_q_2exp (ukz, ukz, (unsigned long int) (-exptol));
              /* Calculation of arctan(Ak) */
              mpfr_set_z (tmp, ukz, GMP_RNDN);
              mpfr_div_2ui (tmp, tmp, twopoweri, GMP_RNDN);
              MPFR_ASSERTD (2*twopoweri > twopoweri);
              mpfr_atan_aux (tmp2, ukz, 2*twopoweri, n0 - i, tabz);
              mpfr_mul (tmp2, tmp2, tmp, GMP_RNDN);
              /* Addition */
              mpfr_add (arctgt, arctgt, tmp2, GMP_RNDN);
              /* Next iteration */
              mpfr_sub (tmp2, sk, tmp, GMP_RNDN);
              mpfr_mul (sk, sk, tmp, GMP_RNDN);
              mpfr_add_ui (sk, sk, 1, GMP_RNDN);
              mpfr_div (sk, tmp2, sk, GMP_RNDN);
            }
          twopoweri <<= 1;
        }
      /* Add last step (Arctan(sk) ~= sk */
      mpfr_add (arctgt, arctgt, sk, GMP_RNDN);
      if (comparaison > 0)
        {
          mpfr_const_pi (tmp, GMP_RNDN);
          mpfr_div_2ui (tmp, tmp, 1, GMP_RNDN);
          mpfr_sub (arctgt, tmp, arctgt, GMP_RNDN);
        }
      MPFR_SET_POS (arctgt);

      if (MPFR_LIKELY (MPFR_CAN_ROUND (arctgt, realprec, MPFR_PREC (atan),
                                       rnd_mode)))
        break;
      MPFR_ZIV_NEXT (loop, realprec);
    }
  MPFR_ZIV_FREE (loop);

  inexact = mpfr_set4 (atan, arctgt, rnd_mode, MPFR_SIGN (x));

  for (i = 0 ; i < oldn0 ; i++)
    mpz_clear (tabz[i]);
  mpz_clear (ukz);
  (*__gmp_free_func) (tabz, oldn0*sizeof (mpz_t));
  MPFR_GROUP_CLEAR (group);

  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (arctgt, inexact, rnd_mode);
}
