/* Test file for mpfr_log10.

Copyright 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
Adapted from tsinh.c.

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

#ifdef CHECK_EXTERNAL
static int
test_log10 (mpfr_ptr a, mpfr_srcptr b, mp_rnd_t rnd_mode)
{
  int res;
  int ok = rnd_mode == GMP_RNDN && mpfr_number_p (b) && mpfr_get_prec (a)>=53;
  if (ok)
    {
      mpfr_print_raw (b);
    }
  res = mpfr_log10 (a, b, rnd_mode);
  if (ok)
    {
      printf (" ");
      mpfr_print_raw (a);
      printf ("\n");
    }
  return res;
}
#else
#define test_log10 mpfr_log10
#endif

#define TEST_FUNCTION test_log10
#include "tgeneric.c"

int
main (int argc, char *argv[])
{
  mpfr_t x, y;
  unsigned int n;

  tests_start_mpfr ();

  test_generic (2, 100, 20);

  mpfr_init2 (x, 53);
  mpfr_init2 (y, 53);

  /* check NaN */
  mpfr_set_nan (x);
  test_log10 (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_nan_p (y));

  /* check Inf */
  mpfr_set_inf (x, -1);
  test_log10 (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_nan_p (y));

  mpfr_set_inf (x, 1);
  test_log10 (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_inf_p (y) && mpfr_sgn (y) > 0);

  /* check negative argument */
  mpfr_set_si (x, -1, GMP_RNDN);
  test_log10 (y, x, GMP_RNDN);
  MPFR_ASSERTN(mpfr_nan_p (y));

  /* check log10(1) = 0 */
  mpfr_set_ui (x, 1, GMP_RNDN);
  test_log10 (y, x, GMP_RNDN);
  MPFR_ASSERTN((mpfr_cmp_ui (y, 0) == 0) && (MPFR_IS_POS (y)));

  /* check log10(10^n)=n */
  mpfr_set_ui (x, 1, GMP_RNDN);
  for (n = 1; n <= 15; n++)
    {
      mpfr_mul_ui (x, x, 10, GMP_RNDN); /* x = 10^n */
      test_log10 (y, x, GMP_RNDN);
      if (mpfr_cmp_ui (y, n) )
        {
          printf ("log10(10^n) <> n for n=%u\n", n);
          exit (1);
        }
    }

  mpfr_clear (x);
  mpfr_clear (y);

  tests_end_mpfr ();
  return 0;
}
