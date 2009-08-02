/* mpfr_set_si_2exp -- set a MPFR number from a machine signed integer with
   a shift

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

#include "mpfr-impl.h"

int
mpfr_set_si_2exp (mpfr_ptr x, long i, mp_exp_t e, mp_rnd_t rnd_mode)
{
  int res;
  MPFR_SAVE_EXPO_DECL (expo);

  MPFR_SAVE_EXPO_MARK (expo);
  res = mpfr_set_si (x, i, rnd_mode);
  mpfr_mul_2si (x, x, e, rnd_mode); /* Should be exact */
  MPFR_SAVE_EXPO_FREE (expo);
  res = mpfr_check_range(x, res, rnd_mode);
  return res;
}
