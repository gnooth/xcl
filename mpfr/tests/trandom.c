/* Test file for mpfr_urandomb

Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
Contributed by the Arenaire and Cacao projects, INRIA.

This file is part of the GNU MPFR Library.

The GNU MPFR Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MPFR Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MPFR Library; see the file COPYING.LESSER.  If not, see
http://www.gnu.org/licenses/ or write to the Free Software Foundation, Inc.,
51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA. */

#include <stdio.h>
#include <stdlib.h>

#include "mpfr-test.h"


static void
test_urandomb (long nbtests, mpfr_prec_t prec, int verbose)
{
  mpfr_t x;
  int *tab, size_tab, k, sh, xn;
  double d, av = 0, var = 0, chi2 = 0, th;
  mpfr_exp_t emin;

  size_tab = (nbtests >= 1000 ? nbtests / 50 : 20);
  tab = (int *) calloc (size_tab, sizeof(int));
  if (tab == NULL)
    {
      fprintf (stderr, "trandom: can't allocate memory in test_urandomb\n");
      exit (1);
    }

  mpfr_init2 (x, prec);
  xn = 1 + (prec - 1) / mp_bits_per_limb;
  sh = xn * mp_bits_per_limb - prec;

  for (k = 0; k < nbtests; k++)
    {
      mpfr_urandomb (x, RANDS);
      /* check that lower bits are zero */
      if (MPFR_MANT(x)[0] & MPFR_LIMB_MASK(sh))
        {
          printf ("Error: mpfr_urandomb() returns invalid numbers:\n");
          mpfr_print_binary (x); puts ("");
          exit (1);
        }
      d = mpfr_get_d1 (x); av += d; var += d*d;
      tab[(int)(size_tab * d)]++;
    }

  /* coverage test */
  emin = mpfr_get_emin ();
  set_emin (1); /* the generated number in [0,1[ is not in the exponent
                        range, except if it is zero */
  k = mpfr_urandomb (x, RANDS);
  if (MPFR_IS_ZERO(x) == 0 && (k == 0 || mpfr_nan_p (x) == 0))
    {
      printf ("Error in mpfr_urandomb, expected NaN, got ");
      mpfr_dump (x);
      exit (1);
    }
  set_emin (emin);

  mpfr_clear (x);
  if (!verbose)
    {
      free(tab);
      return;
    }

  av /= nbtests;
  var = (var / nbtests) - av * av;

  th = (double)nbtests / size_tab;
  printf("Average = %.5f\nVariance = %.5f\n", av, var);
  printf("Repartition for urandomb. Each integer should be close to %d.\n",
         (int)th);

  for (k = 0; k < size_tab; k++)
    {
      chi2 += (tab[k] - th) * (tab[k] - th) / th;
      printf("%d ", tab[k]);
      if (((k+1) & 7) == 0)
        printf("\n");
    }

  printf("\nChi2 statistics value (with %d degrees of freedom) : %.5f\n\n",
         size_tab - 1, chi2);

  free(tab);
  return;
}

int
main (int argc, char *argv[])
{
  long nbtests;
  mpfr_prec_t prec;
  int verbose = 0;

  tests_start_mpfr ();

  if (argc > 1)
    verbose = 1;

  nbtests = 10000;
  if (argc > 1)
    {
      long a = atol(argv[1]);
      if (a != 0)
        nbtests = a;
    }

  if (argc <= 2)
    prec = 1000;
  else
    prec = atol(argv[2]);

  test_urandomb (nbtests, prec, verbose);

  if (argc == 1)  /* check also small precision */
    {
      test_urandomb (nbtests, 2, 0);
    }

  tests_end_mpfr ();
  return 0;
}
