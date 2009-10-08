// SingleFloat.cpp
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

SingleFloat::SingleFloat(double d)
  : TypedObject(WIDETAG_SINGLE_FLOAT), _f(d)
{
  if (d > FLT_MAX || d < -FLT_MAX)
    signal_lisp_error(new FloatingPointOverflow());
}

Value SingleFloat::class_of() const
{
  return C_float;
}

bool SingleFloat::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_single_float || type == S_short_float || type == S_float
            || type == S_real || type == S_number || type == S_atom || type == T);
  else
    return (type == C_float || type == C_real || type == C_number || type == C_t);
}

bool SingleFloat::eql(Value value) const
{
  if (single_float_p(value))
    {
      float f = the_single_float(value)->_f;
      if (f == _f)
        // "If an implementation supports positive and negative zeros as
        // distinct values, then (EQL 0.0 -0.0) returns false."
        return f != 0 || the_single_float(value)->sign() == this->sign();
    }
  return false;
}

bool SingleFloat::equal(Value value) const
{
  // same as EQL
  if (single_float_p(value))
    {
      float f = the_single_float(value)->_f;
      if (f == _f)
        // "If an implementation supports positive and negative zeros as
        // distinct values, then (EQL 0.0 -0.0) returns false."
        return f != 0 || the_single_float(value)->sign() == this->sign();
    }
  return false;
}

Value SingleFloat::rational() const
{
  mpq_t q;
  mpq_init(q);
  mpq_set_d(q, _f); // REVIEW
  Value value = normalize(q);
  MPQ_CLEAR(q);
  return value;
}

unsigned long SingleFloat::hash()
{
  // (SXHASH 0.0) and (SXHASH -0.0) must be eql for all float types
  if (_f == 0)
    return 0; // REVIEW
  return bits() & MOST_POSITIVE_FIXNUM;
}

unsigned long SingleFloat::equalp_hash()
{
  if (_f == (long) _f)
    return ::hash(make_number((long)_f));
  else
    return bits() & MOST_POSITIVE_FIXNUM;
}

AbstractString * SingleFloat::write_to_string()
{
  Thread * thread = current_thread();
  bool is_default_format = memq(thread->symbol_value(S_read_default_float_format),
                                list2(S_single_float, S_short_float));
  String * s = new String();
  if (_f == 0)
    {
      if (bits() < 0)
        s->append_char('-');
      s->append("0.0");
      if (!is_default_format || thread->symbol_value(S_print_fasl) != NIL)
        s->append("s0");
      return s;
    }
  mpfr_t x;
  mpfr_init2(x, 24); // 24-bit precision
  mpfr_set_d(x, _f, GMP_RNDN);
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
  INDEX j = 0;
  BASE_CHAR c = temp->char_at(j++);
  if ((_f >= 0.001 && _f <= 10000000) || (_f <= -0.001 && _f >= -10000000))
    {
      if (c == '-')
        {
          s->append_char(c);
          c = temp->char_at(j++);
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
          while (exp > 0 && j < temp_len)
            {
              s->append_char(temp->fast_char_at(j++));
              --exp;
            }
          while (exp > 0)
            {
              s->append_char('0');
              --exp;
            }
          s->append_char('.');
          if (j == temp_len)
            s->append_char('0');
        }
      else // exp == 0
        {
          s->append_char('0');
          s->append_char('.');
          s->append_char(c);
        }
      while (j < temp_len)
        s->append_char(temp->fast_char_at(j++));
      if (!is_default_format || thread->symbol_value(S_print_fasl) != NIL)
        s->append("f0");
    }
  else
    {
      s->append_char(c);
      if (c == '-')
        s->append_char(temp->char_at(j++));
      s->append_char('.');
      if (j == temp_len)
        s->append_char('0');
      else
        {
          while (j < temp_len)
            s->append_char(temp->fast_char_at(j++));
        }
      if (!is_default_format || thread->symbol_value(S_print_fasl) != NIL)
        s->append_char('f');
      else
        s->append_char('e');
      s->append_long(exp - 1);
    }
  return s;
}

// ### single-float-bits
Value SYS_single_float_bits(Value arg)
{
  if (single_float_p(arg))
    return make_number(the_single_float(arg)->bits());
  else
    return signal_type_error(arg, S_single_float);
}

// ### single-float-p
Value SYS_single_float_p(Value arg)
{
  return single_float_p(arg) ? T : NIL;
}
