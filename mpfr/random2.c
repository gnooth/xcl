/* mpfr_random2 -- Generate a positive random mpfr_t of specified size, with
   long runs of consecutive ones and zeros in the binary representation.
   Intended for testing.

Copyright 1999, 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

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

void
mpfr_random2 (mpfr_ptr x, mp_size_t size, mp_exp_t exp)
{
  mp_size_t xn, k;
  unsigned long sh;
  mp_ptr xp;
  mp_limb_t elimb;

  MPFR_CLEAR_FLAGS (x);

  if (MPFR_UNLIKELY(size == 0))
    {
      MPFR_SET_ZERO (x);
      MPFR_SET_POS (x);
      return ;
    }
  else if (size > 0)
    {
      MPFR_SET_POS (x);
    }
  else
    {
      MPFR_SET_NEG (x);
      size = -size;
    }

  xn = MPFR_LIMB_SIZE (x);
  xp = MPFR_MANT (x);
  if (size > xn)
    size = xn;
  k = xn - size;

  /* k   : # of 0 limbs at the end
     size: # of limbs to fill
     xn  : Size of mantissa */

  /* Generate random mantissa.  */
  mpn_random2 (xp+k, size);

  /* Set mandatory most significant bit.  */
  xp[xn - 1] |= MPFR_LIMB_HIGHBIT;

  if (k != 0)
    {
      /* Clear last limbs */
      MPN_ZERO (xp, k);
    }
  else
    {
      /* Mask off non significant bits in the low limb.  */
      MPFR_UNSIGNED_MINUS_MODULO (sh, MPFR_PREC (x));
      xp[0] &= ~MPFR_LIMB_MASK (sh);
    }

  /* Generate random exponent.  */
  _gmp_rand (&elimb, RANDS, BITS_PER_MP_LIMB);
  exp = ABS (exp);
  MPFR_SET_EXP (x, elimb % (2 * exp + 1) - exp);

  return ;
}
