/* Test file for mpfr_cmp_ui and mpfr_cmp_si.

Copyright 1999, 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

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

int
main (void)
{
  mpfr_t x;
  unsigned long i;
  long s;
  int c;

  tests_start_mpfr ();

  mpfr_init(x);

  /* tests for cmp_ui */
  mpfr_set_ui (x, 3, GMP_RNDZ);
  if ((mpfr_cmp_ui) (x, i = 3) != 0)
    {
      printf ("Error in mpfr_cmp_ui(3.0, 3)\n");
      exit (1);
    }
  if (mpfr_cmp_ui (x, i = 2) <= 0)
    {
      printf ("Error in mpfr_cmp_ui(3.0,2)\n");
      exit (1);
    }
  if (mpfr_cmp_ui (x, i = 4) >= 0)
    {
      printf ("Error in mpfr_cmp_ui(3.0,4)\n");
      exit (1);
    }
  mpfr_set_ui (x, 0, GMP_RNDZ);
  mpfr_neg (x, x, GMP_RNDZ);
  if (mpfr_cmp_ui (x, i = 0))
    {
      printf ("Error in mpfr_cmp_ui(0.0,0)\n");
      exit (1);
    }
  mpfr_set_ui (x, 1, GMP_RNDZ);
  if (mpfr_cmp_ui (x, i = 0) == 0)
    {
      printf ("Error in mpfr_cmp_ui(1.0,0)\n");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  if (mpfr_cmp_ui (x, 1) <= 0)
    {
      printf ("Error in mpfr_cmp_ui (Inf, 0)\n");
      exit (1);
    }
  mpfr_set_inf (x, -1);
  if (mpfr_cmp_ui (x, 1) >= 0)
    {
      printf ("Error in mpfr_cmp_ui (-Inf, 0)\n");
      exit (1);
    }

  mpfr_set_si (x, -1, GMP_RNDN);
  MPFR_ASSERTN(mpfr_cmp_ui (x, 1) < 0);
  MPFR_ASSERTN(mpfr_cmp_ui (x, 0) < 0);

  mpfr_set_ui (x, 1, GMP_RNDN);
  MPFR_ASSERTN(mpfr_cmp_ui (x, 0) > 0);

  /* tests for cmp_si */
  (mpfr_set_si) (x, -3, GMP_RNDZ);
  if ((mpfr_cmp_si) (x, s = -3) != 0)
    {
      printf ("Error in mpfr_cmp_si(-3.0,-3)\n");
      exit (1);
    }
  if (mpfr_cmp_si (x, s = -4) <= 0)
    {
      printf ("Error in mpfr_cmp_si(-3.0,-4)\n");
      exit (1);
    }
  if (mpfr_cmp_si (x, s = 1) >= 0)
    {
      printf ("Error in mpfr_cmp_si(-3.0,1)\n");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  if (mpfr_cmp_si (x, -1) <= 0)
    {
      printf ("Error in mpfr_cmp_si (Inf, 0)\n");
      exit (1);
    }
  mpfr_set_inf (x, -1);
  if (mpfr_cmp_si (x, -1) >= 0)
    {
      printf ("Error in mpfr_cmp_si (-Inf, 0)\n");
      exit (1);
    }

  /* case b=0 */
  mpfr_set_ui (x, 0, GMP_RNDZ);
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) == 0);
  MPFR_ASSERTN(mpfr_cmp_si (x, 1) < 0);
  MPFR_ASSERTN(mpfr_cmp_si (x, -1) > 0);

  /* case i=0 */
  mpfr_set_ui (x, 1, GMP_RNDZ);
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) > 0);
  mpfr_set_ui (x, 0, GMP_RNDZ);
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) == 0);
  mpfr_neg (x, x, GMP_RNDZ);
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) == 0);
  mpfr_set_si (x, -1, GMP_RNDZ);
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) < 0);

  /* case large x */
  mpfr_set_str_binary (x, "1E100");
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) > 0);
  MPFR_ASSERTN(mpfr_cmp_si (x, 1) > 0);
  MPFR_ASSERTN(mpfr_cmp_si (x, -1) > 0);
  mpfr_set_str_binary (x, "-1E100");
  MPFR_ASSERTN(mpfr_cmp_si (x, 0) < 0);
  MPFR_ASSERTN(mpfr_cmp_si (x, 1) < 0);
  MPFR_ASSERTN(mpfr_cmp_si (x, -1) < 0);

  /* corner case */
  mpfr_set_ui (x, 1, GMP_RNDZ);
  mpfr_mul_2exp (x, x, BITS_PER_MP_LIMB - 1, GMP_RNDZ);
  /* now EXP(x)=BITS_PER_MP_LIMB */
  MPFR_ASSERTN(mpfr_cmp_si (x, 1) > 0);

  /* Check NAN */
  mpfr_set_nan (x);
  mpfr_clear_erangeflag ();
  c = mpfr_cmp_ui (x, 12);
  if (c != 0 || !mpfr_erangeflag_p () )
    {
      printf ("NAN error (1)\n");
      exit (1);
    }
  mpfr_clear_erangeflag ();
  c = mpfr_cmp_si (x, -12);
  if (c != 0 || !mpfr_erangeflag_p () )
    {
      printf ("NAN error (2)\n");
      exit (1);
    }

  mpfr_clear (x);

  tests_end_mpfr ();
  return 0;
}
