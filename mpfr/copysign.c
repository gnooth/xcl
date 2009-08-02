/* mpfr_copysign -- Produce a value with the magnitude of x and sign of y

Copyright 2001, 2002, 2003, 2004, 2006 Free Software Foundation, Inc.

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

 /*
   The computation of z with magnitude of x and sign of y
   z = sign(y) * abs(x)
 */

int
mpfr_copysign (mpfr_ptr z, mpfr_srcptr x ,mpfr_srcptr y , mp_rnd_t rnd_mode)
{
  if (MPFR_UNLIKELY( MPFR_IS_NAN(y)))
    {
      MPFR_SET_NAN(z);
      MPFR_RET_NAN;
    }
  else
    return mpfr_set4(z, x, rnd_mode, MPFR_SIGN(y));
}
