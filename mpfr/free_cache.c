/* mpfr_free_cache - Free the cache used by MPFR for internal consts.

Copyright 2004, 2005, 2006 Free Software Foundation, Inc.

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

void
mpfr_free_cache (void)
{
  mpfr_clear_cache (__gmpfr_cache_const_pi);
  mpfr_clear_cache (__gmpfr_cache_const_log2);
  mpfr_clear_cache (__gmpfr_cache_const_euler);
  mpfr_clear_cache (__gmpfr_cache_const_catalan);
}
