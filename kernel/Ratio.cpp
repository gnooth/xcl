// Ratio.cpp
//
// Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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
#include "Ratio.hpp"

bool Ratio::typep(Value type) const
{
  if (symbolp(type))
    return  (type == S_ratio || type == S_rational || type == S_real
             || type == S_number || type == S_atom || type == T);
  else
    return  (type == C_ratio || type == C_rational || type == C_real
             || type == C_number || type == C_t);
}

Value Ratio::numerator()
{
  mpz_t z;
  mpz_init_set(z, mpq_numref(_q));
  Value value = normalize(z);
  MPZ_CLEAR(z);
  return value;
}

Value Ratio::denominator()
{
  mpz_t z;
  mpz_init_set(z, mpq_denref(_q));
  Value value = normalize(z);
  MPZ_CLEAR(z);
  return value;
}

Value Ratio::add(long n) const
{
  mpq_t q;
  mpq_init(q);
  mpq_set_si(q, n, 1);
  mpq_t sum;
  mpq_init(sum);
  mpq_add(sum, _q, q);
  Value value = normalize(sum);
  MPQ_CLEAR(sum);
  return value;
}

Value Ratio::add(Bignum * b)
{
  mpq_t q;
  mpq_init(q);
  mpq_set_z(q, b->_z);
  mpq_t sum;
  mpq_init(sum);
  mpq_add(sum, _q, q);
  Value value = normalize(sum);
  MPQ_CLEAR(sum);
  return value;
}

Value Ratio::add(Ratio * r)
{
  mpq_t result;
  mpq_init(result);
  mpq_add(result, _q, r->_q);
  Value value = normalize(result);
  MPQ_CLEAR(result);
  return value;
}

Value Ratio::subtract(long n) const
{
  return add(-n);
}

Value Ratio::subtract(Ratio * r) const
{
  mpq_t result;
  mpq_init(result);
  mpq_sub(result, _q, r->_q);
  Value value = normalize(result);
  MPQ_CLEAR(result);
  return value;
}

Value Ratio::negate() const
{
  mpq_t q;
  mpq_init(q);
  mpq_neg(q, _q);
  Value value = make_value(new Ratio(q));
  MPQ_CLEAR(q);
  return value;
}

bool Ratio::eql(Value value) const
{
  if (ratiop(value))
    return mpq_equal(_q, the_ratio(value)->_q);
  else
    return false;
}

bool Ratio::equal(Value value) const
{
  if (ratiop(value))
    return mpq_equal(_q, the_ratio(value)->_q);
  else
    return false;
}

unsigned long Ratio::hash()
{
  return mix(::hash(numerator()), ::hash(denominator()));
}

AbstractString * Ratio::write_to_string()
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_radix) != NIL)
    {
      int radix = check_index(thread->symbol_value(S_print_base), 2, 36);
      String * s = new String("#");
      switch (radix)
        {
        case 2:
          s->append_char('b');
          break;
        case 8:
          s->append_char('o');
          break;
        case 16:
          s->append_char('x');
          break;
        default:
          s->append_long(radix);
          s->append_char('r');
          break;
        }
      void * last_special_binding = thread->last_special_binding();
      thread->bind_special(S_print_radix, NIL);
      s->append(::write_to_string(numerator()));
      s->append_char('/');
      s->append(::write_to_string(denominator()));
      thread->set_last_special_binding(last_special_binding);
      return s;
    }
  else
    {
      String * s = new String(::write_to_string(numerator()));
      s->append_char('/');
      s->append(::write_to_string(denominator()));
      return s;
    }
}

Value make_ratio(const char *s, long base)
{
  mpq_t q;
  mpq_init(q);
  long error = mpq_set_str(q, s, base);
  if (error)
    {
      MPQ_CLEAR(q);
      return NULL_VALUE;
    }
  // conversion succeeded
  Value value = make_value(new Ratio(q));
  MPQ_CLEAR(q);
  return value;
}

// ### ratiop object => boolean
Value SYS_ratiop(Value arg)
{
  return ratiop(arg) ? T : NIL;
}

// ### numerator rational => numerator
Value CL_numerator(Value arg)
{
  if (ratiop(arg))
    return the_ratio(arg)->numerator();
  if (integerp(arg))
    return arg;
  return signal_type_error(arg, S_rational);
}

// ### denominator rational => denominator
Value CL_denominator (Value arg)
{
  if (ratiop(arg))
    return the_ratio(arg)->denominator();
  if (integerp(arg))
    return FIXNUM_ONE;
  return signal_type_error(arg, S_rational);
}
