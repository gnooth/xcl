/* Test file for mpfr_fma.

Copyright 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
Adapted from tarctan.c.

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
main (int argc, char *argv[])
{
  mpfr_t x, y,z,s;

  tests_start_mpfr ();

  mpfr_init (x);
  mpfr_init (s);
  mpfr_init (y);
  mpfr_init (z);

  /* check special cases */
  mpfr_set_prec (x, 2);
  mpfr_set_prec (y, 2);
  mpfr_set_prec (z, 2);
  mpfr_set_prec (s, 2);
  mpfr_set_str (x, "-0.75", 10, GMP_RNDN);
  mpfr_set_str (y, "0.5", 10, GMP_RNDN);
  mpfr_set_str (z, "0.375", 10, GMP_RNDN);
  mpfr_fma (s, x, y, z, GMP_RNDU); /* result is 0 */
  if (mpfr_cmp_ui(s, 0))
    {
      printf("Error: -0.75 * 0.5 + 0.375 should be equal to 0 for prec=2\n");
      exit(1);
    }

  mpfr_set_prec (x, 27);
  mpfr_set_prec (y, 27);
  mpfr_set_prec (z, 27);
  mpfr_set_prec (s, 27);
  mpfr_set_str_binary (x, "1.11111111111111111111111111e-1");
  mpfr_set (y, x, GMP_RNDN);
  mpfr_set_str_binary (z, "-1.00011110100011001011001001e-1");
  if (mpfr_fma (s, x, y, z, GMP_RNDN) >= 0)
    {
      printf ("Wrong inexact flag for x=y=1-2^(-27)\n");
      exit (1);
    }

  mpfr_set_nan (x);
  mpfr_random (y);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_nan_p (s))
    {
      printf ("evaluation of function in x=NAN does not return NAN");
      exit (1);
    }

  mpfr_set_nan (y);
  mpfr_random (x);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if (!mpfr_nan_p(s))
    {
      printf ("evaluation of function in y=NAN does not return NAN");
      exit (1);
    }

  mpfr_set_nan (z);
  mpfr_random (y);
  mpfr_random (x);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if (!mpfr_nan_p (s))
    {
      printf ("evaluation of function in z=NAN does not return NAN");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  mpfr_set_inf (y, 1);
  mpfr_set_inf (z, 1);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if (!mpfr_inf_p (s) || mpfr_sgn (s) < 0)
    {
      printf ("Error for (+inf) * (+inf) + (+inf)\n");
      exit (1);
    }

  mpfr_set_inf (x, -1);
  mpfr_set_inf (y, -1);
  mpfr_set_inf (z, 1);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if (!mpfr_inf_p (s) || mpfr_sgn (s) < 0)
    {
      printf ("Error for (-inf) * (-inf) + (+inf)\n");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  mpfr_set_inf (y, -1);
  mpfr_set_inf (z, -1);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if (!mpfr_inf_p (s) || mpfr_sgn (s) > 0)
    {
      printf ("Error for (+inf) * (-inf) + (-inf)\n");
      exit (1);
    }

  mpfr_set_inf (x, -1);
  mpfr_set_inf (y, 1);
  mpfr_set_inf (z, -1);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if (!mpfr_inf_p (s) || mpfr_sgn (s) > 0)
    {
      printf ("Error for (-inf) * (+inf) + (-inf)\n");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  mpfr_set_ui (y, 0, GMP_RNDN);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_nan_p (s))
    {
      printf ("evaluation of function in x=INF y=0  does not return NAN");
      exit (1);
    }

  mpfr_set_inf (y, 1);
  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_nan_p (s))
    {
      printf ("evaluation of function in x=0 y=INF does not return NAN");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  mpfr_random (y); /* always positive */
  mpfr_set_inf (z, -1);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_nan_p (s))
    {
      printf ("evaluation of function in x=INF y>0 z=-INF does not return NAN");
      exit (1);
    }

  mpfr_set_inf (y, 1);
  mpfr_random (x);
  mpfr_set_inf (z, -1);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_nan_p (s))
    {
      printf ("evaluation of function in x>0 y=INF z=-INF does not return NAN");
      exit (1);
    }

  mpfr_set_inf (x, 1);
  mpfr_random (y);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_inf_p (s) || mpfr_sgn (s) < 0)
    {
      printf ("evaluation of function in x=INF does not return INF");
      exit (1);
    }

  mpfr_set_inf (y, 1);
  mpfr_random (x);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_inf_p (s) || mpfr_sgn (s) < 0)
    {
      printf ("evaluation of function in y=INF does not return INF");
      exit (1);
    }

  mpfr_set_inf (z, 1);
  mpfr_random (x);
  mpfr_random (y);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(!mpfr_inf_p (s) || mpfr_sgn (s) < 0)
    {
      printf ("evaluation of function in z=INF does not return INF");
      exit (1);
    }

  mpfr_set_ui (x, 0, GMP_RNDN);
  mpfr_random (y);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(mpfr_cmp (s, z))
    {
      printf ("evaluation of function in x=0 does not return z\n");
      exit (1);
    }

  mpfr_set_ui (y, 0, GMP_RNDN);
  mpfr_random (x);
  mpfr_random (z);
  mpfr_fma (s, x, y, z, GMP_RNDN);
  if(mpfr_cmp (s, z))
    {
      printf ("evaluation of function in y=0 does not return z\n");
      exit (1);
    }

  {
    mp_prec_t prec;
    mpfr_t t, slong;
    mp_rnd_t rnd;
    int inexact, compare;
    unsigned int n;

    mp_prec_t p0=2, p1=200;
    unsigned int N=200;

    mpfr_init (t);
    mpfr_init (slong);

    /* generic test */
    for (prec = p0; prec <= p1; prec++)
    {
      mpfr_set_prec (x, prec);
      mpfr_set_prec (y, prec);
      mpfr_set_prec (z, prec);
      mpfr_set_prec (s, prec);
      mpfr_set_prec (t, prec);

      for (n=0; n<N; n++)
        {
          mpfr_random (x);
          mpfr_random (y);
          mpfr_random (z);

          if (randlimb () % 2)
            mpfr_neg (x, x, GMP_RNDN);
          if (randlimb () % 2)
            mpfr_neg (y, y, GMP_RNDN);
          if (randlimb () % 2)
            mpfr_neg (z, z, GMP_RNDN);

          rnd = (mp_rnd_t) RND_RAND ();
          mpfr_set_prec (slong, 2 * prec);
          if (mpfr_mul (slong, x, y, rnd))
            {
              printf ("x*y should be exact\n");
              exit (1);
            }
          compare = mpfr_add (t, slong, z, rnd);
          inexact = mpfr_fma (s, x, y, z, rnd);
          if (mpfr_cmp (s, t))
            {
              printf ("results differ for x=");
              mpfr_out_str (stdout, 2, prec, x, GMP_RNDN);
              printf ("  y=");
              mpfr_out_str (stdout, 2, prec, y, GMP_RNDN);
              printf ("  z=");
              mpfr_out_str (stdout, 2, prec, z, GMP_RNDN);
              printf (" prec=%u rnd_mode=%s\n", (unsigned int) prec,
                      mpfr_print_rnd_mode (rnd));
              printf ("got      ");
              mpfr_out_str (stdout, 2, prec, s, GMP_RNDN);
              puts ("");
              printf ("expected ");
              mpfr_out_str (stdout, 2, prec, t, GMP_RNDN);
              puts ("");
              printf ("approx  ");
              mpfr_print_binary (slong);
              puts ("");
              exit (1);
            }
          if (((inexact == 0) && (compare != 0)) ||
              ((inexact < 0) && (compare >= 0)) ||
              ((inexact > 0) && (compare <= 0)))
            {
              printf ("Wrong inexact flag for rnd=%s: expected %d, got %d\n",
                      mpfr_print_rnd_mode (rnd), compare, inexact);
              printf ("x="); mpfr_out_str (stdout, 2, 0, x, GMP_RNDN);
              printf (" y="); mpfr_out_str (stdout, 2, 0, y, GMP_RNDN);
              printf (" z="); mpfr_out_str (stdout, 2, 0, z, GMP_RNDN);
              printf (" s="); mpfr_out_str (stdout, 2, 0, s, GMP_RNDN);
              printf ("\n");
              exit (1);
            }
        }
    }
  mpfr_clear (t);
  mpfr_clear (slong);

  }
  mpfr_clear (x);
  mpfr_clear (y);
  mpfr_clear (z);
  mpfr_clear (s);

  tests_end_mpfr ();
  return 0;
}
