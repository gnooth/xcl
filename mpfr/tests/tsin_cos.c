/* Test file for mpfr_sin_cos.

Copyright 2000, 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

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

#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"

static void
large_test (int prec, int N)
{
  int i;
  mpfr_t x, s, c;

  mpfr_init2 (x, prec);
  mpfr_init2 (s, prec);
  mpfr_init2 (c, prec);
  mpfr_set_ui (x, 3, GMP_RNDN);
  mpfr_sqrt (x, x, GMP_RNDN);
  for (i=0; i<N; i++)
    mpfr_sin_cos (s, c, x, GMP_RNDN);
  mpfr_out_str (stdout, 10, 0, s, GMP_RNDN); puts ("");
  mpfr_clear (x);
  mpfr_clear (s);
  mpfr_clear (c);
}

static void
check53 (const char *xs, const char *sin_xs, const char *cos_xs,
         mp_rnd_t rnd_mode)
{
  mpfr_t xx, s, c;

  mpfr_inits2 (53, xx, s, c, NULL);
  mpfr_set_str1 (xx, xs); /* should be exact */
  mpfr_sin_cos (s, c, xx, rnd_mode);
  if (mpfr_cmp_str1 (s, sin_xs))
    {
      printf ("mpfr_sin_cos failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode (rnd_mode));
      printf ("mpfr_sin_cos gives sin(x)=");
      mpfr_out_str(stdout, 10, 0, s, GMP_RNDN);
      printf(", expected %s\n", sin_xs);
      exit (1);
    }
  if (mpfr_cmp_str1 (c, cos_xs))
    {
      printf ("mpfr_sin_cos failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode (rnd_mode));
      printf ("mpfr_sin_cos gives cos(x)=");
      mpfr_out_str(stdout, 10, 0, c, GMP_RNDN);
      printf(", expected %s\n", cos_xs);
      exit (1);
    }
  mpfr_clears (xx, s, c, NULL);
}

static void
check53sin (const char *xs, const char *sin_xs, mp_rnd_t rnd_mode)
{
  mpfr_t xx, s, c;

  mpfr_inits2 (53, xx, s, c, NULL);
  mpfr_set_str1 (xx, xs); /* should be exact */
  mpfr_sin_cos (s, c, xx, rnd_mode);
  if (mpfr_cmp_str1 (s, sin_xs))
    {
      printf ("mpfr_sin_cos failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode (rnd_mode));
      printf ("mpfr_sin_cos gives sin(x)=");
      mpfr_out_str(stdout, 10, 0, s, GMP_RNDN);
      printf(", expected %s\n", sin_xs);
      exit (1);
    }
  mpfr_clears (xx, s, c, NULL);
}

static void
check53cos (const char *xs, const char *cos_xs, mp_rnd_t rnd_mode)
{
  mpfr_t xx, c, s;

  mpfr_inits2 (53, xx, s, c, NULL);
  mpfr_set_str1 (xx, xs); /* should be exact */
  mpfr_sin_cos (s, c, xx, rnd_mode);
  if (mpfr_cmp_str1 (c, cos_xs))
    {
      printf ("mpfr_sin_cos failed for x=%s, rnd=%s\n",
              xs, mpfr_print_rnd_mode (rnd_mode));
      printf ("mpfr_sin_cos gives cos(x)=");
      mpfr_out_str(stdout, 10, 0, c, GMP_RNDN);
      printf(", expected %s\n", cos_xs);
      exit (1);
    }
  mpfr_clears (xx, s, c, NULL);
}

static void
check_nans (void)
{
  mpfr_t  x, s, c;

  mpfr_init2 (x, 123L);
  mpfr_init2 (s, 123L);
  mpfr_init2 (c, 123L);

  /* sin(NaN)==NaN, cos(NaN)==NaN */
  mpfr_set_nan (x);
  mpfr_sin_cos (s, c, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (s));
  MPFR_ASSERTN (mpfr_nan_p (c));

  /* sin(+Inf)==NaN, cos(+Inf)==NaN */
  mpfr_set_inf (x, 1);
  mpfr_sin_cos (s, c, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (s));
  MPFR_ASSERTN (mpfr_nan_p (c));

  /* sin(-Inf)==NaN, cos(-Inf)==NaN */
  mpfr_set_inf (x, -1);
  mpfr_sin_cos (s, c, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_nan_p (s));
  MPFR_ASSERTN (mpfr_nan_p (c));

  /* check zero */
  mpfr_set_ui  (x, 0, GMP_RNDN);
  mpfr_sin_cos (s, c, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_cmp_ui (s, 0) == 0 && MPFR_IS_POS (s));
  MPFR_ASSERTN (mpfr_cmp_ui (c, 1) == 0);
  mpfr_neg (x, x, GMP_RNDN);
  mpfr_sin_cos (s, c, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_cmp_ui (s, 0) == 0 && MPFR_IS_NEG (s));
  MPFR_ASSERTN (mpfr_cmp_ui (c, 1) == 0);

  /* coverage test */
  mpfr_set_prec (x, 2);
  mpfr_set_ui (x, 4, GMP_RNDN);
  mpfr_set_prec (s, 2);
  mpfr_set_prec (c, 2);
  mpfr_sin_cos (s, c, x, GMP_RNDN);
  MPFR_ASSERTN (mpfr_cmp_si_2exp (s, -3, -2) == 0);
  MPFR_ASSERTN (mpfr_cmp_si_2exp (c, -3, -2) == 0);

  mpfr_clear (x);
  mpfr_clear (s);
  mpfr_clear (c);
}

/* tsin_cos prec [N] performs N tests with prec bits */
int
main(int argc, char *argv[])
{
  tests_start_mpfr ();

  check_nans ();

  if (argc > 1)
    {
      large_test (atoi (argv[1]), (argc > 2) ? atoi (argv[2]) : 1);
    }

  /* worst case from PhD thesis of Vincent Lefe`vre: x=8980155785351021/2^54 */
  check53 ("4.984987858808754279e-1", "4.781075595393330379e-1",
           "8.783012931285841817e-1", GMP_RNDN);
  check53 ("4.984987858808754279e-1", "4.781075595393329824e-1",
           "8.783012931285840707e-1", GMP_RNDD);
  check53 ("4.984987858808754279e-1", "4.781075595393329824e-1",
           "8.783012931285840707e-1", GMP_RNDZ);
  check53 ("4.984987858808754279e-1", "4.781075595393330379e-1",
           "8.783012931285841817e-1", GMP_RNDU);
  check53 ("1.00031274099908640274",  "8.416399183372403892e-1",
           "0.540039116973283217504", GMP_RNDN);
  check53 ("1.00229256850978698523",  "8.427074524447979442e-1",
           "0.538371757797526551137", GMP_RNDZ);
  check53 ("1.00288304857059840103",  "8.430252033025980029e-1",
           "0.537874062022526966409", GMP_RNDZ);
  check53 ("1.00591265847407274059",  "8.446508805292128885e-1",
           "0.53531755997839769456",  GMP_RNDN);

  /* check one argument only */
  check53sin ("1.00591265847407274059", "8.446508805292128885e-1", GMP_RNDN);
  check53cos ("1.00591265847407274059", "0.53531755997839769456",  GMP_RNDN);

  tests_end_mpfr ();
  return 0;
}
