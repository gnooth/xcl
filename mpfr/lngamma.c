/* mpfr_lngamma -- lngamma function

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

#define MPFR_NEED_LONGLONG_H
#include "mpfr-impl.h"

/* assuming b[0]...b[2(n-1)] are computed, computes and stores B[2n]*(2n+1)!

   t/(exp(t)-1) = sum(B[j]*t^j/j!, j=0..infinity)
   thus t = (exp(t)-1) * sum(B[j]*t^j/j!, n=0..infinity).
   Taking the coefficient of degree n+1 > 1, we get:
   0 = sum(1/(n+1-k)!*B[k]/k!, k=0..n)
   which gives:
   B[n] = -sum(binomial(n+1,k)*B[k], k=0..n-1)/(n+1).

   Let C[n] = B[n]*(n+1)!.
   Then C[n] = -sum(binomial(n+1,k)*C[k]*n!/(k+1)!,  k=0..n-1),
   which proves that the C[n] are integers.
*/
static mpz_t*
bernoulli (mpz_t *b, unsigned long n)
{
  if (n == 0)
    {
      b = (mpz_t *) (*__gmp_allocate_func) (sizeof (mpz_t));
      mpz_init_set_ui (b[0], 1);
    }
  else
    {
      mpz_t t;
      unsigned long k;

      b = (mpz_t *) (*__gmp_reallocate_func)
        (b, n * sizeof (mpz_t), (n + 1) * sizeof (mpz_t));
      mpz_init (b[n]);
      /* b[n] = -sum(binomial(2n+1,2k)*C[k]*(2n)!/(2k+1)!,  k=0..n-1) */
      mpz_init_set_ui (t, 2 * n + 1);
      mpz_mul_ui (t, t, 2 * n - 1);
      mpz_mul_ui (t, t, 2 * n);
      mpz_mul_ui (t, t, n);
      mpz_div_ui (t, t, 3); /* exact: t=binomial(2*n+1,2*k)*(2*n)!/(2*k+1)!
                               for k=n-1 */
      mpz_mul (b[n], t, b[n-1]);
      for (k = n - 1; k-- > 0;)
        {
          mpz_mul_ui (t, t, 2 * k + 1);
          mpz_mul_ui (t, t, 2 * k + 2);
          mpz_mul_ui (t, t, 2 * k + 2);
          mpz_mul_ui (t, t, 2 * k + 3);
          mpz_div_ui (t, t, 2 * (n - k) + 1);
          mpz_div_ui (t, t, 2 * (n - k));
          mpz_addmul (b[n], t, b[k]);
        }
      /* take into account C[1] */
      mpz_mul_ui (t, t, 2 * n + 1);
      mpz_div_2exp (t, t, 1);
      mpz_sub (b[n], b[n], t);
      mpz_neg (b[n], b[n]);
      mpz_clear (t);
    }
  return b;
}

/* given a precision p, return alpha, such that the argument reduction
   will use k = alpha*p*log(2).

   Warning: we should always have alpha >= log(2)/(2Pi) ~ 0.11,
   and the smallest value of alpha multiplied by the smallest working
   precision should be >= 4.
*/
static double
mpfr_gamma_alpha (mp_prec_t p)
{
  if (p <= 100)
    return 0.6;
  else if (p <= 200)
    return 0.8;
  else if (p <= 500)
    return 0.8;
  else if (p <= 1000)
    return 1.3;
  else if (p <= 2000)
    return 1.7;
  else if (p <= 5000)
    return 2.2;
  else if (p <= 10000)
    return 3.4;
  else /* heuristic fit from above */
    return 0.26 * (double) MPFR_INT_CEIL_LOG2 ((unsigned long) p);
}

/* lngamma(x) = log(gamma(x)).
   We use formula [6.1.40] from Abramowitz&Stegun:
   lngamma(z) = (z-1/2)*log(z) - z + 1/2*log(2*Pi)
              + sum (Bernoulli[2n]/(2m)/(2m-1)/z^(2m-1),m=1..infinity)
   According to [6.1.42], if the sum is truncated after m=n, the error
   R_n(z) is bounded by |B[2n+2]|*K(z)/(2n+1)/(2n+2)/|z|^(2n+1)
   where K(z) = max (z^2/(u^2+z^2)) for u >= 0.
   For z real, |K(z)| <= 1 thus R_n(z) is bounded by the first neglected term.
 */
#ifdef IS_GAMMA
static int
#define GAMMA_FUNC mpfr_gamma_aux
#else
int
#define GAMMA_FUNC mpfr_lngamma
#endif
GAMMA_FUNC (mpfr_ptr y, mpfr_srcptr z0, mp_rnd_t rnd)
{
  mp_prec_t precy, w; /* working precision */
  mpfr_t s, t, u, v, z;
  unsigned long m, k, maxm;
  mpz_t *B;
  int inexact, compared;
  mp_exp_t err_s, err_t;
  unsigned long Bm = 0; /* number of allocated B[] */
  unsigned long oldBm;
  double d;
  MPFR_SAVE_EXPO_DECL (expo);

#ifndef IS_GAMMA
  /* special cases */
  if (MPFR_UNLIKELY (MPFR_IS_SINGULAR (z0)))
    {
      if (MPFR_IS_NAN (z0) || MPFR_IS_NEG (z0))
        {
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;
        }
      else /* lngamma(+Inf) = lngamma(+0) = +Inf */
        {
          MPFR_SET_INF (y);
          MPFR_SET_POS (y);
          MPFR_RET (0);  /* exact */
        }
    }

  /* if x < 0 and -2k-1 <= x <= -2k, then lngamma(x) = NaN */
  if (MPFR_IS_NEG (z0))
    {
      MPFR_SAVE_EXPO_MARK (expo);
      if (mpfr_get_si (z0, GMP_RNDZ) % 2 == 0 || mpfr_integer_p (z0))
        {
          MPFR_SAVE_EXPO_FREE (expo);
          MPFR_SET_NAN (y);
          MPFR_RET_NAN;
        }
      MPFR_SAVE_EXPO_FREE (expo);
    }
#endif

  precy = MPFR_PREC(y);

  compared = mpfr_cmp_ui (z0, 1);

#ifndef IS_GAMMA
  if (compared == 0 || (compared > 0 && mpfr_cmp_ui (z0, 2) == 0))
    return mpfr_set_ui (y, 0, GMP_RNDN);  /* lngamma(1 or 2) = +0 */
#endif

  mpfr_init2 (s, MPFR_PREC_MIN);
  mpfr_init2 (t, MPFR_PREC_MIN);
  mpfr_init2 (u, MPFR_PREC_MIN);
  mpfr_init2 (v, MPFR_PREC_MIN);
  mpfr_init2 (z, MPFR_PREC_MIN);

  MPFR_SAVE_EXPO_MARK (expo);

  if (compared < 0)
    {
      mp_exp_t err_u;

      /* use reflection formula:
         gamma(x) = Pi*(x-1)/sin(Pi*(2-x))/gamma(2-x)
         thus lngamma(x) = log(Pi*(x-1)/sin(Pi*(2-x))) - lngamma(2-x) */

      w = precy + MPFR_INT_CEIL_LOG2 (precy);
      while (1)
        {
          w += MPFR_INT_CEIL_LOG2 (w) + 13;
          MPFR_ASSERTD(w >= 3);
          mpfr_set_prec (s, w);
          mpfr_set_prec (t, w);
          mpfr_set_prec (u, w);
          mpfr_set_prec (v, w);
          mpfr_ui_sub (s, 2, z0, GMP_RNDD); /* s = (2-z0) * (1+2u) */
          mpfr_const_pi (t, GMP_RNDN);      /* t = Pi * (1+u) */
          mpfr_lngamma (u, s, GMP_RNDN); /* lngamma(2-x) */
          /* Let s = (2-z0) + h. By construction, -(2-z0)*(2u) <= h <= 0.
             We have lngamma(s) = lngamma(2-z0) + h*Psi(z), z in [2-z0+h,2-z0].
             Since 2-z0+h >= 1 and |Psi(x)| <= max(1,log(x)) for x >= 1,
             the error on u is bounded by
             ulp(u)/2 + (2-z0)*max(1,log(2-z0))*2^(1-w). */
          d = (double) MPFR_GET_EXP(s) * 0.694; /* upper bound for log(2-z0) */
          err_u = MPFR_GET_EXP(s) + __gmpfr_ceil_log2 (d) + 1 - MPFR_GET_EXP(u);
          err_u = (err_u >= 0) ? err_u + 1 : 0;
          /* now the error on u is bounded by 2^err_u ulps */

          mpfr_mul (s, s, t, GMP_RNDN); /* Pi*(2-x), (1+u)^4 */
          err_s = MPFR_GET_EXP(s); /* 2-x <= 2^err_s */
          mpfr_sin (s, s, GMP_RNDN); /* sin(Pi*(2-x)) */
          /* the error on s is bounded by 1/2*ulp(s) + [(1+u)^4-1]*(2-x)
             <= 1/2*ulp(s) + 5*2^(-w)*(2-x) for w >= 3 */
          err_s += 3 - MPFR_GET_EXP(s);
          err_s = (err_s >= 0) ? err_s + 1 : 0;
          /* the error on s is bounded by 2^err_s ulps, thus the relative
             error is bounded by 2^(err_s+1) */
          err_s ++; /* relative error */

          mpfr_sub_ui (v, z0, 1, GMP_RNDN); /* v = (x-1) * (1+u) */
          mpfr_mul (v, v, t, GMP_RNDN); /* v = Pi*(x-1) * (1+u)^3 */
          mpfr_div (v, v, s, GMP_RNDN); /* Pi*(x-1)/sin(Pi*(2-x)) */
          /* (1+u)^(4+2^err_s+1) */
          err_s = (err_s <= 2) ? 3 + (err_s / 2) : err_s + 1;
          MPFR_ASSERTD(MPFR_IS_POS(v));
          mpfr_log (v, v, GMP_RNDN);
          /* log(v*(1+e)) = log(v)+log(1+e) where |e| <= 2^(err_s-w).
             Since |log(1+e)| <= 2*e for |e| <= 1/4, the error on v is
             bounded by ulp(v)/2 + 2^(err_s+1-w). */
          if (err_s + 2 > w)
            {
              w += err_s + 2;
            }
          else
            {
              err_s += 1 - MPFR_GET_EXP(v);
              err_s = (err_s >= 0) ? err_s + 1 : 0;
              /* the error on v is bounded by 2^err_s ulps */
              err_u += MPFR_GET_EXP(u); /* absolute error on u */
              err_s += MPFR_GET_EXP(v); /* absolute error on v */
              mpfr_sub (s, v, u, GMP_RNDN);
              /* the total error on s is bounded by ulp(s)/2 + 2^(err_u-w)
                 + 2^(err_s-w) <= ulp(s)/2 + 2^(max(err_u,err_s)+1-w) */
              err_s = (err_s >= err_u) ? err_s : err_u;
              err_s += 1 - MPFR_GET_EXP(s); /* error is 2^err_s ulp(s) */
              err_s = (err_s >= 0) ? err_s + 1 : 0;
              if (mpfr_can_round (s, w - err_s, GMP_RNDN, GMP_RNDZ, precy
                                  + (rnd == GMP_RNDN)))
                goto end;
            }
        }
    }

  /* now z0 > 1 */

  MPFR_ASSERTD (compared > 0);

  /* since k is O(w), the value of log(z0*...*(z0+k-1)) is about w*log(w),
     so there is a cancellation of ~log(w) in the argument reconstruction */
  w = precy + MPFR_INT_CEIL_LOG2 (precy);

  do
    {
      w += MPFR_INT_CEIL_LOG2 (w) + 13;
      MPFR_ASSERTD (w >= 3);

      mpfr_set_prec (s, 53);
      /* we need z >= w*log(2)/(2*Pi) to get an absolute error less than 2^(-w)
         but the optimal value is about 0.155665*w */
      /* FIXME: replace double by mpfr_t types. */
      mpfr_set_d (s, mpfr_gamma_alpha (precy) * (double) w, GMP_RNDU);
      if (mpfr_cmp (z0, s) < 0)
        {
          mpfr_sub (s, s, z0, GMP_RNDU);
          k = mpfr_get_ui (s, GMP_RNDU);
          if (k < 3)
            k = 3;
        }
      else
        k = 3;

      mpfr_set_prec (s, w);
      mpfr_set_prec (t, w);
      mpfr_set_prec (u, w);
      mpfr_set_prec (v, w);
      mpfr_set_prec (z, w);

      mpfr_add_ui (z, z0, k, GMP_RNDN);
      /* z = (z0+k)*(1+t1) with |t1| <= 2^(-w) */

      /* z >= 4 ensures the relative error on log(z) is small,
         and also (z-1/2)*log(z)-z >= 0 */
      MPFR_ASSERTD (mpfr_cmp_ui (z, 4) >= 0);

      mpfr_log (s, z, GMP_RNDN); /* log(z) */
      /* we have s = log((z0+k)*(1+t1))*(1+t2) with |t1|, |t2| <= 2^(-w).
         Since w >= 2 and z0+k >= 4, we can write log((z0+k)*(1+t1))
         = log(z0+k) * (1+t3) with |t3| <= 2^(-w), thus we have
         s = log(z0+k) * (1+t4)^2 with |t4| <= 2^(-w) */
      mpfr_mul_2exp (t, z, 1, GMP_RNDN); /* t = 2z * (1+t5) */
      mpfr_sub_ui (t, t, 1, GMP_RNDN); /* t = 2z-1 * (1+t6)^3 */
      /* since we can write 2z*(1+t5) = (2z-1)*(1+t5') with
         t5' = 2z/(2z-1) * t5, thus |t5'| <= 8/7 * t5 */
      mpfr_mul (s, s, t, GMP_RNDN); /* (2z-1)*log(z) * (1+t7)^6 */
      mpfr_div_2exp (s, s, 1, GMP_RNDN); /* (z-1/2)*log(z) * (1+t7)^6 */
      mpfr_sub (s, s, z, GMP_RNDN); /* (z-1/2)*log(z)-z */
      /* s = [(z-1/2)*log(z)-z]*(1+u)^14, s >= 1/2 */

      mpfr_ui_div (u, 1, z, GMP_RNDN); /* 1/z * (1+u), u <= 1/4 since z >= 4 */

      /* the first term is B[2]/2/z = 1/12/z: t=1/12/z, C[2]=1 */
      mpfr_div_ui (t, u, 12, GMP_RNDN); /* 1/(12z) * (1+u)^2, t <= 3/128 */
      mpfr_set (v, t, GMP_RNDN);        /* (1+u)^2, v < 2^(-5) */
      mpfr_add (s, s, v, GMP_RNDN);     /* (1+u)^15 */

      mpfr_mul (u, u, u, GMP_RNDN); /* 1/z^2 * (1+u)^3 */

      if (Bm == 0)
        {
          B = bernoulli ((mpz_t *) 0, 0);
          B = bernoulli (B, 1);
          Bm = 2;
        }

      /* m <= maxm ensures that 2*m*(2*m+1) <= ULONG_MAX */
      maxm = 1UL << (BITS_PER_MP_LIMB / 2 - 1);

      /* s:(1+u)^15, t:(1+u)^2, t <= 3/128 */

      for (m = 2; MPFR_GET_EXP(v) + (mp_exp_t) w >= MPFR_GET_EXP(s); m++)
        {
          mpfr_mul (t, t, u, GMP_RNDN); /* (1+u)^(10m-14) */
          if (m <= maxm)
            {
              mpfr_mul_ui (t, t, 2*(m-1)*(2*m-3), GMP_RNDN);
              mpfr_div_ui (t, t, 2*m*(2*m-1), GMP_RNDN);
              mpfr_div_ui (t, t, 2*m*(2*m+1), GMP_RNDN);
            }
          else
            {
              mpfr_mul_ui (t, t, 2*(m-1), GMP_RNDN);
              mpfr_mul_ui (t, t, 2*m-3, GMP_RNDN);
              mpfr_div_ui (t, t, 2*m, GMP_RNDN);
              mpfr_div_ui (t, t, 2*m-1, GMP_RNDN);
              mpfr_div_ui (t, t, 2*m, GMP_RNDN);
              mpfr_div_ui (t, t, 2*m+1, GMP_RNDN);
            }
          /* (1+u)^(10m-8) */
          /* invariant: t=1/(2m)/(2m-1)/z^(2m-1)/(2m+1)! */
          if (Bm <= m)
            {
              B = bernoulli (B, m); /* B[2m]*(2m+1)!, exact */
              Bm ++;
            }
          mpfr_mul_z (v, t, B[m], GMP_RNDN); /* (1+u)^(10m-7) */
          MPFR_ASSERTD(MPFR_GET_EXP(v) <= - (2 * m + 3));
          mpfr_add (s, s, v, GMP_RNDN);
        }
      /* m <= 1/2*Pi*e*z ensures that |v[m]| < 1/2^(2m+3) */
      MPFR_ASSERTD ((double) m <= 4.26 * mpfr_get_d (z, GMP_RNDZ));

      /* We have sum([(1+u)^(10m-7)-1]*1/2^(2m+3), m=2..infinity)
         <= 1.46*u for u <= 2^(-3).
         We have 0 < lngamma(z) - [(z - 1/2) ln(z) - z + 1/2 ln(2 Pi)] < 0.021
         for z >= 4, thus since the initial s >= 0.85, the different values of
         s differ by at most one binade, and the total rounding error on s
         in the for-loop is bounded by 2*(m-1)*ulp(final_s).
         The error coming from the v's is bounded by
         1.46*2^(-w) <= 2*ulp(final_s).
         Thus the total error so far is bounded by [(1+u)^15-1]*s+2m*ulp(s)
         <= (2m+47)*ulp(s).
         Taking into account the truncation error (which is bounded by the last
         term v[] according to 6.1.42 in A&S), the bound is (2m+48)*ulp(s).
      */

      /* add 1/2*log(2*Pi) and subtract log(z0*(z0+1)*...*(z0+k-1)) */
      mpfr_const_pi (v, GMP_RNDN); /* v = Pi*(1+u) */
      mpfr_mul_2exp (v, v, 1, GMP_RNDN); /* v = 2*Pi * (1+u) */
      if (k)
        {
          unsigned long l;
          mpfr_set (t, z0, GMP_RNDN); /* t = z0*(1+u) */
          for (l = 1; l < k; l++)
            {
              mpfr_add_ui (u, z0, l, GMP_RNDN); /* u = (z0+l)*(1+u) */
              mpfr_mul (t, t, u, GMP_RNDN);     /* (1+u)^(2l+1) */
            }
          /* now t: (1+u)^(2k-1) */
          /* instead of computing log(sqrt(2*Pi)/t), we compute
             1/2*log(2*Pi/t^2), which trades a square root for a square */
          mpfr_mul (t, t, t, GMP_RNDN); /* (z0*...*(z0+k-1))^2, (1+u)^(4k-1) */
          mpfr_div (v, v, t, GMP_RNDN);
          /* 2*Pi/(z0*...*(z0+k-1))^2 (1+u)^(4k+1) */
        }
#ifdef IS_GAMMA
      err_s = MPFR_GET_EXP(s);
      mpfr_exp (s, s, GMP_RNDN);
      /* before the exponential, we have s = s0 + h where
         |h| <= (2m+48)*ulp(s), thus exp(s0) = exp(s) * exp(-h).
         For |h| <= 1/4, we have |exp(h)-1| <= 1.2*|h| thus
         |exp(s) - exp(s0)| <= 1.2 * exp(s) * (2m+48)* 2^(EXP(s)-w). */
      d = 1.2 * (2.0 * (double) m + 48.0);
      /* the error on s is bounded by d*2^err_s * 2^(-w) */
      mpfr_sqrt (t, v, GMP_RNDN);
      /* let v0 be the exact value of v. We have v = v0*(1+u)^(4k+1),
         thus t = sqrt(v0)*(1+u)^(2k+3/2). */
      mpfr_mul (s, s, t, GMP_RNDN);
      /* the error on input s is bounded by (1+u)^(d*2^err_s),
         and that on t is (1+u)^(2k+3/2), thus the
         total error is (1+u)^(d*2^err_s+2k+5/2) */
      err_s += __gmpfr_ceil_log2 (d);
      err_t = __gmpfr_ceil_log2 (2.0 * (double) k + 2.5);
      err_s = (err_s >= err_t) ? err_s + 1 : err_t + 1;
#else
      mpfr_log (t, v, GMP_RNDN);
      /* let v0 be the exact value of v. We have v = v0*(1+u)^(4k+1),
         thus log(v) = log(v0) + (4k+1)*log(1+u). Since |log(1+u)/u| <= 1.07
         for |u| <= 2^(-3), the absolute error on log(v) is bounded by
         1.07*(4k+1)*u, and the rounding error by ulp(t). */
      mpfr_div_2exp (t, t, 1, GMP_RNDN);
      /* the error on t is now bounded by ulp(t) + 0.54*(4k+1)*2^(-w).
         We have sqrt(2*Pi)/(z0*(z0+1)*...*(z0+k-1)) <= sqrt(2*Pi)/k! <= 0.5
         since k>=3, thus t <= -0.5 and ulp(t) >= 2^(-w).
         Thus the error on t is bounded by (2.16*k+1.54)*ulp(t). */
      err_t = MPFR_GET_EXP(t) + (mp_exp_t)
        __gmpfr_ceil_log2 (2.2 * (double) k + 1.6);
      err_s = MPFR_GET_EXP(s) + (mp_exp_t)
        __gmpfr_ceil_log2 (2.0 * (double) m + 48.0);
      mpfr_add (s, s, t, GMP_RNDN); /* this is a subtraction in fact */
      /* the final error in ulp(s) is
         <= 1 + 2^(err_t-EXP(s)) + 2^(err_s-EXP(s))
         <= 2^(1+max(err_t,err_s)-EXP(s)) if err_t <> err_s
         <= 2^(2+max(err_t,err_s)-EXP(s)) if err_t = err_s */
      err_s = (err_t == err_s) ? 1 + err_s : ((err_t > err_s) ? err_t : err_s);
      err_s += 1 - MPFR_GET_EXP(s);
#endif
    }
  while (MPFR_UNLIKELY (!MPFR_CAN_ROUND (s, w - err_s, precy, rnd)));

  oldBm = Bm;
  while (Bm--)
    mpz_clear (B[Bm]);
  (*__gmp_free_func) (B, oldBm * sizeof (mpz_t));

 end:
  inexact = mpfr_set (y, s, rnd);

  mpfr_clear (s);
  mpfr_clear (t);
  mpfr_clear (u);
  mpfr_clear (v);
  mpfr_clear (z);

  MPFR_SAVE_EXPO_FREE (expo);
  return mpfr_check_range (y, inexact, rnd);
}
