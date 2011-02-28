// require-type.cpp
//
// Copyright (C) 2007-2011 Peter Graves <gnooth@gmail.com>
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "HashTable.hpp"
#include "Package.hpp"
#include "keywordp.hpp"

// ### require-boolean arg => boolean
Value SYS_require_boolean(Value arg)
{
  if (arg == T || arg == NIL)
    return arg;
  return signal_type_error(arg, S_boolean);
}

// ### require-cons arg => arg
Value SYS_require_cons(Value arg)
{
  if (consp(arg))
    return arg;
  return SYS_error_not_cons(arg);
}

// ### require-character arg => arg
Value SYS_require_character(Value arg)
{
  if (characterp(arg))
    return arg;
  return signal_type_error(arg, S_character);
}

// ### require-fixnum arg => arg
Value SYS_require_fixnum(Value arg)
{
  if (fixnump(arg))
    return arg;
  return SYS_error_not_fixnum(arg);
}

// ### require-function arg => arg
Value SYS_require_function(Value arg)
{
  if (functionp(arg))
    return arg;
  return SYS_error_not_function(arg);
}

// ### require-hash-table arg => arg
Value SYS_require_hash_table(Value arg)
{
  if (hash_table_p(arg))
    return arg;
  return SYS_error_not_hash_table(arg);
}

// ### require-integer arg => arg
Value SYS_require_integer(Value arg)
{
  if (integerp(arg))
    return arg;
  return SYS_error_not_integer(arg);
}

// ### require-keyword arg => arg
Value SYS_require_keyword(Value arg)
{
  if (keywordp(arg))
    return arg;
  return signal_type_error(arg, S_keyword);
}

// ### require-list arg => arg
Value SYS_require_list(Value arg)
{
  if (listp(arg))
    return arg;
  return SYS_error_not_list(arg);
}

// ### require-number arg => arg
Value SYS_require_number(Value arg)
{
  if (numberp(arg))
    return arg;
  return SYS_error_not_number(arg);
}

// ### require-stream arg => arg
Value SYS_require_stream(Value arg)
{
  if (ansi_stream_p(arg))
    return arg;
  return signal_type_error(arg, S_stream);
}

// ### require-simple-bit-vector arg => arg
Value SYS_require_simple_bit_vector(Value arg)
{
  if (simple_bit_vector_p(arg))
    return arg;
  return SYS_error_not_simple_bit_vector(arg);
}

// ### require-simple-string arg => arg
Value SYS_require_simple_string(Value arg)
{
  if (simple_string_p(arg))
    return arg;
  return SYS_error_not_simple_string(arg);
}

// ### require-string arg => arg
Value SYS_require_string(Value arg)
{
  if (stringp(arg))
    return arg;
  return SYS_error_not_string(arg);
}

// ### require-symbol arg => arg
Value SYS_require_symbol(Value arg)
{
  if (symbolp(arg))
    return arg;
  return SYS_error_not_symbol(arg);
}

// ### require-simple-vector arg => arg
Value SYS_require_simple_vector(Value arg)
{
  if (simple_vector_p(arg))
    return arg;
  return SYS_error_not_simple_vector(arg);
}

// ### require-vector arg => arg
Value SYS_require_vector(Value arg)
{
  if (vectorp(arg))
    return arg;
  return SYS_error_not_vector(arg);
}

// ### require-unsigned-byte arg => arg
Value SYS_require_unsigned_byte(Value arg)
{
  if (fixnump(arg))
    if (arg >= 0)
      return arg;
  if (bignump(arg))
    if (the_bignum(arg)->plusp())
      return arg;
  return signal_type_error(arg, S_unsigned_byte);
}

// ### require-ub32 arg => arg
Value SYS_require_ub32(Value arg)
{
#ifdef __x86_64__
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n >= 0 && n < 4294967296)
        return arg;
    }
#else
  // 32-bit Lisp
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n >= 0)
        return arg;
    }
  else if (bignump(arg))
    {
      Bignum * b = the_bignum(arg);
      if (mpz_sgn(b->_z) >= 0 && mpz_fits_ulong_p(b->_z))
        return arg;
    }
#endif
  return signal_type_error(arg, UB32_TYPE);
}

// ### check-fixnum-bounds n low high => n
// bounds are inclusive
Value SYS_check_fixnum_bounds(Value arg1, Value arg2, Value arg3)
{
  long n = fixnum_value(arg1);
  if (n >= fixnum_value(arg2) && n <= fixnum_value(arg3))
    return arg1;
  return signal_type_error(arg1, list3(S_integer, arg2, arg3));
}
