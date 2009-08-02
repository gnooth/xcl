/* Test file for mpfr_copysign.

Copyright 2004, 2006 Free Software Foundation, Inc.

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
  mpfr_t x, y, z;

  tests_start_mpfr ();

  mpfr_init (x);
  mpfr_init (y);
  mpfr_init (z);

  /* case y=NaN */
  mpfr_set_nan (y);
  mpfr_set_ui (x, 1250, GMP_RNDN);
  mpfr_copysign (z, x, y, GMP_RNDN);
  if (!mpfr_nan_p (z))
    {
      printf ("Error in mpfr_copysign (NaN)\n");
      exit (1);
    }
  /* case y!=NaN */
  mpfr_set_ui (y, 123, GMP_RNDN);
  mpfr_set_ui (x, 1250, GMP_RNDN);
  mpfr_copysign (z, x, y, GMP_RNDN);
  if (mpfr_cmp_ui (z, 1250))
    {
      printf ("Error in mpfr_copysign (1250)\n");
      exit (1);
    }
  mpfr_set_si (y, -17, GMP_RNDN);
  mpfr_set_ui (x, 42, GMP_RNDN);
  mpfr_copysign (z, x, y, GMP_RNDN);
  if (mpfr_cmp_si (z, -42))
    {
      printf ("Error in mpfr_copysign (-42)\n");
      exit (1);
    }

  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);

  tests_end_mpfr ();
  return 0;
}
