// Bignum.cpp
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
#include "primitives.hpp"

Value Bignum::type_of() const
{
  if (mpz_sgn(_z) > 0)
    return list2(S_integer, make_number(MOST_POSITIVE_FIXNUM + 1));
  else
    return S_bignum;
}

Value Bignum::class_of() const
{
  return C_integer;
}

bool Bignum::typep(Value type) const
{
  if (type == S_bignum || type == S_integer || type == S_rational
      || type == S_real || type == S_number || type == S_atom || type == T)
    return true;
  if (type == C_integer || type == C_rational || type == C_real
      || type == C_number || type == T)
    return true;
  if (consp(type))
    {
      if (xcar(type) == S_integer)
        {
          Value low = CL_cadr(type);
          if (integerp(low))
            {
              if (lt(make_value(this), low))
                return false;
            }
          Value high = CL_caddr(type);
          if (integerp(high))
            {
              if (gt(make_value(this), high))
                return false;
            }
          return true;
        }
    }

  return false;
}

Value Bignum::add(long n) const
{
  mpz_t result;
  mpz_init_set(result, _z);
  if (n >= 0)
    mpz_add_ui(result, result, (unsigned long) n);
  else
    mpz_sub_ui(result, result, (unsigned long) -n);
  if (mpz_fits_slong_p(result))
    {
      long n = mpz_get_si(result);
      if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
        {
          MPZ_CLEAR(result);
          return make_fixnum(n);
        }
    }
  return make_value(new Bignum(result, false));
}

Value Bignum::add(Bignum * b) const
{
  mpz_t result;
  mpz_init_set(result, _z);
  mpz_add(result, result, b->_z);
  Value value = normalize(result);
  MPZ_CLEAR(result);
  return value;
}

Value Bignum::subtract(long n) const
{
  mpz_t result;
  mpz_init_set(result, _z);
  if (n >= 0)
    mpz_sub_ui(result, result, (unsigned long) n);
  else
    mpz_add_ui(result, result, (unsigned long) -n);
  Value value = normalize(result);
  MPZ_CLEAR(result);
  return value;
}

Value Bignum::subtract(Bignum * b) const
{
  mpz_t result;
  mpz_init_set(result, _z);
  mpz_sub(result, result, b->_z);
  Value value = normalize(result);
  MPZ_CLEAR(result);
  return value;
}

Value Bignum::multiply(long n)
{
  mpz_t result;
  mpz_init_set(result, _z);
  mpz_mul_si(result, result, n);
  Value value = normalize(result);
  MPZ_CLEAR(result);
  return value;
}

Value Bignum::multiply(Bignum * b)
{
  mpz_t result;
  mpz_init_set(result, _z);
  mpz_mul(result, result, b->_z);
  Value value = normalize(result);
  MPZ_CLEAR(result);
  return value;
}

Value Bignum::negate() const
{
  mpz_t result;
  mpz_init(result);
  mpz_neg(result, _z);
  Value value = normalize(result);
  MPZ_CLEAR(result);
  return value;
}

bool Bignum::eql(Value value) const
{
  if (bignump(value))
    return mpz_cmp(_z, the_bignum(value)->_z) == 0;
  else
    return false;
}

bool Bignum::equal(Value value) const
{
  if (bignump(value))
    return mpz_cmp(_z, the_bignum(value)->_z) == 0;
  else
    return false;
}

unsigned long Bignum::hash()
{
  return mix(mpz_sgn(_z), mpz_get_ui(_z));
}

AbstractString * Bignum::write_to_string()
{
  Thread * const thread = current_thread();
  const long base = fixnum_value(current_thread()->symbol_value(S_print_base));
  char * buf = (char *) GC_malloc_atomic(mpz_sizeinbase(_z, base) + 2);
  if (!buf)
    out_of_memory();
  mpz_get_str(buf, base, _z);
  SimpleString * s = new_simple_string(buf);
  if (base > 10)
    s->nupcase();
  if (thread->symbol_value(S_print_radix) == NIL)
    return s;
  String * s2 = new String();
  switch (base)
    {
    case 2:
      s2->append("#b");
      s2->append(s);
      break;
    case 8:
      s2->append("#o");
      s2->append(s);
      break;
    case 10:
      s2->append(s);
      s2->append_char('.');
      break;
    case 16:
      s2->append("#x");
      s2->append(s);
      break;
    default:
      s2->append_char('#');
      s2->append_long(base);
      s2->append_char('r');
      s2->append(s);
      break;
    }
  return s2;
}
