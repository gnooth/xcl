// DoubleFloat.cpp
//
// Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

#include <float.h>

#include "lisp.hpp"
#include "Ratio.hpp"
#include "FloatingPointOverflow.hpp"

#include "../mpfr/mpfr.h"

DoubleFloat::DoubleFloat(double d)
  : TypedObject(WIDETAG_DOUBLE_FLOAT), _d(d)
{
  if (d > DBL_MAX || d < -DBL_MAX)
    signal_lisp_error(new FloatingPointOverflow());
}

Value DoubleFloat::class_of() const
{
  return C_float;
}

bool DoubleFloat::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_double_float || type == S_long_float || type == S_float
            || type == S_real || type == S_number || type == S_atom || type == T);
  else
    return (type == C_float || type == C_real || type == C_number || type == C_t);
}

bool DoubleFloat::eql(Value value) const
{
  if (double_float_p(value))
    {
      double d = the_double_float(value)->_d;
      if (d == _d)
        // "If an implementation supports positive and negative zeros as
        // distinct values, then (EQL 0.0 -0.0) returns false."
        return d != 0 || the_double_float(value)->sign() == this->sign();
    }
  return false;
}

bool DoubleFloat::equal(Value value) const
{
  // same as EQL
  if (double_float_p(value))
    {
      double d = the_double_float(value)->_d;
      if (d == _d)
        // "If an implementation supports positive and negative zeros as
        // distinct values, then (EQL 0.0 -0.0) returns false."
        return d != 0 || the_double_float(value)->sign() == this->sign();
    }
  return false;
}

Value DoubleFloat::rational() const
{
  mpq_t q;
  mpq_init(q);
  mpq_set_d(q, _d);
  Value value = normalize(q);
  MPQ_CLEAR(q);
  return value;
}

unsigned long DoubleFloat::hash()
{
  // (SXHASH 0.0) and (SXHASH -0.0) must be eql for all float types
  if (_d == 0)
    return 0; // REVIEW
  return (high_bits() ^ low_bits()) & MOST_POSITIVE_FIXNUM;
}

unsigned long DoubleFloat::equalp_hash()
{
  if (_d == (long) _d)
    return ::hash(make_number((long)_d));
  else
    return (high_bits() ^ low_bits()) & MOST_POSITIVE_FIXNUM;
}

AbstractString * DoubleFloat::write_to_string()
{
  Thread * thread = current_thread();
  bool is_default_format = memq(thread->symbol_value(S_read_default_float_format),
                                list2(S_double_float, S_long_float));
  String * s = new String();
  if (_d == 0)
    {
      if (high_bits() < 0)
        s->append_char('-');
      s->append("0.0");
      if (!is_default_format || thread->symbol_value(S_print_fasl) != NIL)
        s->append("d0");
      return s;
    }
  mpfr_t x;
  mpfr_init2(x, 53); // 53-bit precision
  mpfr_set_d(x, _d, GMP_RNDN);
  mp_exp_t exp; // signed long
  String * temp = new String(mpfr_get_str(NULL, &exp, 10, 0, x, GMP_RNDN));
  mpfr_clear(x);
  // strip trailing zeros
  for (INDEX i = temp->length(); i-- > 0;)
    {
      BASE_CHAR c = temp->fast_char_at(i);
      if (c == '0')
        temp->set_length(i);
      else
        break;
    }
  const INDEX temp_len = temp->length();
  INDEX i = 0;
  BASE_CHAR c = temp->char_at(i++);
  if ((_d >= 0.001 && _d <= 10000000) || (_d <= -0.001 && _d >= -10000000))
    {
      if (c == '-')
        {
          s->append_char(c);
          c = temp->char_at(i++);
        }
      if (exp < 0)
        {
          s->append_char('0');
          s->append_char('.');
          while (exp < 0)
            {
              s->append_char('0');
              ++exp;
            }
          s->append_char(c);
        }
      else if (exp > 0)
        {
          s->append_char(c);
          --exp;
          while (exp > 0 && i < temp_len)
            {
              s->append_char(temp->fast_char_at(i++));
              --exp;
            }
          while (exp > 0)
            {
              s->append_char('0');
              --exp;
            }
          s->append_char('.');
          if (i == temp_len)
            s->append_char('0');
        }
      else // exp == 0
        {
          s->append_char('0');
          s->append_char('.');
          s->append_char(c);
        }
      while (i < temp_len)
        s->append_char(temp->fast_char_at(i++));
      if (!is_default_format || thread->symbol_value(S_print_fasl) != NIL)
        s->append("d0");
    }
  else
    {
      s->append_char(c);
      if (c == '-')
        s->append_char(temp->char_at(i++));
      s->append_char('.');
      if (i == temp_len)
        s->append_char('0');
      else
        {
          while (i < temp_len)
            s->append_char(temp->fast_char_at(i++));
        }
      if (!is_default_format || thread->symbol_value(S_print_fasl) != NIL)
        s->append_char('d');
      else
        s->append_char('e');
      s->append_long(exp - 1);
    }
  return s;
}

// ### double-float-high-bits
Value SYS_double_float_high_bits(Value arg)
{
  if (double_float_p(arg))
    return make_number(the_double_float(arg)->high_bits());
  else
    return signal_type_error(arg, S_double_float);
}

// ### double-float-low-bits
Value SYS_double_float_low_bits(Value arg)
{
  if (double_float_p(arg))
    return make_number(the_double_float(arg)->low_bits());
  else
    return signal_type_error(arg, S_double_float);
}

// ### double-float-p
Value SYS_double_float_p(Value arg)
{
  return double_float_p(arg) ? T : NIL;
}

// ### %double-float-+
Value SYS_double_float_add_internal(Value arg1, Value arg2)
{
  return the_double_float(arg1)->add(the_double_float(arg2));
}

// ### %double-float--
Value SYS_double_float_subtract_internal(Value arg1, Value arg2)
{
  return the_double_float(arg1)->subtract(the_double_float(arg2));
}
