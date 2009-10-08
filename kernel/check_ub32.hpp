// check_ub32.hpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#ifndef __CHECK_UB32_HPP
#define __CHECK_UB32_HPP

inline unsigned int check_ub32(Value value)
{
#ifdef __x86_64__
  if (fixnump(value))
    {
      long n = xlong(value);
      if (n >= 0 && n < 4294967296)
        return (unsigned int) n;
    }
#else
  // 32-bit Lisp
  if (fixnump(value))
    {
      long n = xlong(value);
      if (n >= 0)
        return (unsigned int) n;
    }
  else if (bignump(value))
    {
      Bignum * b = the_bignum(value);
      if (mpz_sgn(b->_z) >= 0 && mpz_fits_ulong_p(b->_z))
        return mpz_get_ui(b->_z);
    }
#endif
  signal_type_error(value, UB32_TYPE);
  // not reached
  return 0;
}

#endif // check_ub32.hpp
