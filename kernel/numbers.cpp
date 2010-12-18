// numbers.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include <float.h>

#include "lisp.hpp"
#include "primitives.hpp"
#include "Complex.hpp"
#include "Ratio.hpp"
#include "ReaderError.hpp"
#include "DivisionByZero.hpp"
#include "FloatingPointOverflow.hpp"
#include "FloatingPointUnderflow.hpp"

#include "../mpfr/mpfr.h"

inline long abs(long x)
{
  return (x >= 0) ? x : -x;
}

inline bool same_sign_p(long x, long y)
{
  if (x == y)
    return true;
  if (x > 0 && y > 0)
    return true;
  if (x < 0 && y < 0)
    return true;
  return false;
}

// ### integerp
Value CL_integerp(Value arg)
{
  return integerp(arg) ? T : NIL;
}

bool rationalp(Value value)
{
  if (fixnump(value))
    return true;
  if (typed_object_p(value))
    {
      switch (the_typed_object(value)->widetag())
        {
        case WIDETAG_BIGNUM:
        case WIDETAG_RATIO:
          return true;
        }
    }
  return false;
}

// ### rationalp
Value CL_rationalp(Value arg)
{
  return rationalp(arg) ? T : NIL;
}

bool realp(Value value)
{
  if (fixnump(value))
    return true;
  if (typed_object_p(value))
    {
      switch (the_typed_object(value)->widetag())
        {
        case WIDETAG_BIGNUM:
        case WIDETAG_RATIO:
        case WIDETAG_SINGLE_FLOAT:
        case WIDETAG_DOUBLE_FLOAT:
          return true;
        default:
          ;
          // Fall through...
        }
    }
  return false;
}

// ### realp
Value CL_realp(Value arg)
{
  return realp(arg) ? T : NIL;
}

bool numberp(Value value)
{
  if (fixnump(value))
    return true;
  if (typed_object_p(value))
    {
      switch (the_typed_object(value)->widetag())
        {
        case WIDETAG_BIGNUM:
        case WIDETAG_RATIO:
        case WIDETAG_SINGLE_FLOAT:
        case WIDETAG_DOUBLE_FLOAT:
        case WIDETAG_COMPLEX:
          return true;
        default:
          ;
          // Fall through...
        }
    }
  return false;
}

// ### numberp
Value CL_numberp(Value arg)
{
  return numberp(arg) ? T : NIL;
}

bool zerop(Value arg)
{
  if (arg == 0) // fixnum zero
    return true;
  TYPECODE typecode = typecode_of(arg);
  switch (typecode)
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
    case TYPECODE_RATIO:
      return false;
    case TYPECODE_SINGLE_FLOAT:
      return the_single_float(arg)->_f == 0;
    case TYPECODE_DOUBLE_FLOAT:
      return the_double_float(arg)->_d == 0;
    case TYPECODE_COMPLEX:
      {
        Complex * c = the_complex(arg);
        return (zerop(c->realpart()) && zerop(c->imagpart()));
      }
    default:
      signal_type_error(arg, S_number);
      // not reached
      return false;
    }
}

// ### zerop
Value CL_zerop(Value arg)
{
  return zerop(arg) ? T : NIL;
}

// ### evenp
Value CL_evenp(Value arg)
{
  if (fixnump(arg))
    return (xlong(arg) & 1) == 0 ? T : NIL;
  if (bignump(arg))
    return the_bignum(arg)->evenp() ? T : NIL;
  return signal_type_error(arg, S_integer);
}

// ### oddp
Value CL_oddp(Value arg)
{
  if (fixnump(arg))
    return (xlong(arg) & 1) ? T : NIL;
  if (bignump(arg))
    return the_bignum(arg)->oddp() ? T : NIL;
  return signal_type_error(arg, S_integer);
}

Value normalize(mpz_t z)
{
  if (mpz_fits_slong_p(z))
    {
      long n = mpz_get_si(z);
      if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
        return make_fixnum(n);
    }
  return make_value(new Bignum(z));
}

Value normalize(mpq_t q)
{
  if (mpz_cmp_si(mpq_denref(q), 1) == 0)
    {
      mpz_t z;
      mpz_init_set(z, mpq_numref(q));
      Value value = normalize(z);
      MPZ_CLEAR(z);
      return value;
    }
  else
    return make_value(new Ratio(q));
}

Value make_float(const char *s, unsigned long len)
{
  if (len == 0)
    return NULL_VALUE;

  String * string = new String();
  unsigned long i = 0;
  bool maybe_float = false;
  char c = s[i];
  if (c == '-' || c == '+')
    {
      string->append_char(c);
      ++i;
    }
  while (i < len)
    {
      c = s[i];
      if (c == '.' || (c >= '0' && c <= '9'))
        {
          if (c == '.')
            maybe_float = true;
          string->append_char(c);
          ++i;
        }
      else
        break;
    }
  Value type = current_thread()->symbol_value(S_read_default_float_format);
  if (i < len)
    {
      c = s[i];
      if (strchr("esfdlESFDL", c))
        {
          // Exponent marker.
          maybe_float = true;
          if (strchr("sfSF", c))
            type = S_single_float;
          else if (strchr("dlDL", c))
            type = S_double_float;
          string->append_char('E');
          ++i;
        }
    }
  if (!maybe_float)
      return NULL_VALUE;

  // Append rest of s.
  string->append(s + i);

  s = string->as_c_string();

  mpfr_t x;
  if (type == S_single_float || type == S_short_float)
    mpfr_init2(x, 24);
  else
    mpfr_init2(x, 53);
  if (mpfr_set_str(x, s, 10, GMP_RNDN) != 0)
    return NULL_VALUE;
  double d = mpfr_get_d(x, GMP_RNDN);
  mpfr_clear(x);
  if (type == S_single_float || type == S_short_float)
    return make_single_float((float)d);
  else
    return make_double_float(d);
}

Value make_number(AbstractString * string, int base, Stream * stream)
{
  long index = string->index_of('/');
  if (index >= 0)
    {
      SimpleString * s1 = string->substring(0, index);
      Value numerator = make_number(s1, base, stream);
      if (numerator == NULL_VALUE)
        return NULL_VALUE;
      SimpleString * s2 = string->substring(index + 1);
      for (INDEX i = s2->length(); i-- > 0;)
        {
          if (!digit_char_p(s2->fast_char_at(i), base))
            return NULL_VALUE;
        }
      Value denominator = make_number(s2, base, stream);
      if (integerp(numerator) && integerp(denominator))
        {
          if (stream != NULL && denominator == 0)
            return signal_lisp_error(new ReaderError(stream, "Division by zero."));
          return SYS_two_arg_slash(numerator, denominator);
        }
      return signal_lisp_error(new ReaderError()); // REVIEW
    }

  INDEX len = string->length();

  if (len == 0)
    return NULL_VALUE;

  // check for trailing '.'
  if (string->fast_char_at(len - 1) == '.')
    {
      if (--len == 0)
        return NULL_VALUE;
      base = 10;
      string = string->substring(0, len);
    }

  const char * s = string->as_c_string();
  bool maybe_integer = true;
  if (base == 10)
    {
      for (INDEX i = len; i-- > 0;)
        {
          unsigned char c = s[i];
          if (c < '0' || c > '9')
            {
              if (i > 0 || len == 1 || (c != '-' && c != '+'))
                {
                  maybe_integer = false;
                  break;
                }
            }
        }
    }
  else
    {
      // base != 10
      for (INDEX i = len; i-- > 0;)
        {
          BASE_CHAR c = s[i];
          if (!digit_char_p(c, base))
            {
              if (i > 0 || len == 1 || (c != '-' && c != '+'))
                {
                  maybe_integer = false;
                  break;
                }
            }
        }
    }

  if (!maybe_integer)
    return make_float(s, len);

  long n = strtol(s, NULL, base);
  if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
    return make_fixnum(n);

  // "The strtol() function returns the result of the conversion, unless the
  // value would underflow or overflow. If an underflow occurs, strtol()
  // returns LONG_MIN. If an overflow occurs, strtol() returns LONG_MAX."
  if (n > LONG_MIN && n < LONG_MAX)
    // No overflow.
    return make_value(new Bignum(n));

  // mpz_init_set_str() doesn't like a leading '+'
  if (*s == '+')
    ++s;

  mpz_t z;
  int error = mpz_init_set_str(z, s, base);
  if (error)
    {
      MPZ_CLEAR(z);
      return NULL_VALUE;
    }
  // conversion succeeded
  Value value = make_value(new Bignum(z));
  MPZ_CLEAR(z);
  return value;
}

bool digit_char_p(BASE_CHAR c, int radix)
{
  if (c >= '0')
    {
      int n = c - '0';
      if (radix <= 10)
        return n < radix;
      if (n < 10)
        return true;
      if (c >= 'A')
        {
          // A-Z
          n -= 7;
          if (n >= 10 && n < radix)
            return true;
          if (c >= 'a')
            {
              // a-z
              n -= 32;
              if (n >= 10 && n < radix)
                return true;
            }
        }
    }
  return false;
}

Value negate(Value arg)
{
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n == MOST_NEGATIVE_FIXNUM)
        return make_integer(-n);
      else
        return make_fixnum(-n);
    }
  if (bignump(arg))
    return the_bignum(arg)->negate();
  if (ratiop(arg))
    return the_ratio(arg)->negate();
  if (single_float_p(arg))
    return the_single_float(arg)->negate();
  if (double_float_p(arg))
    return the_double_float(arg)->negate();
  if (complexp(arg))
    return the_complex(arg)->negate();
  return signal_type_error(arg, S_number);
}

// ### two-arg-*
Value SYS_two_arg_star(Value v1, Value v2)
{
  if (fixnump(v1))
    {
      if (fixnump(v2))
        {
#ifdef __x86_64__
          long n1 = xlong(v1);
          long n2 = xlong(v2);
          if (n1 >= INT_MIN && n1 <= INT_MAX)
            {
              if (n2 >= INT_MIN && n2 <= INT_MAX)
                {
                  long product = n1 * n2;
                  if (product >= MOST_NEGATIVE_FIXNUM && product <= MOST_POSITIVE_FIXNUM)
                    return make_fixnum(product);
                  else
                    return make_value(new Bignum(product));
                }
            }
#else
          long long n1 = xlong(v1);
          long long n2 = xlong(v2);
          long long product = n1 * n2;
          if (product >= MOST_NEGATIVE_FIXNUM && product <= MOST_POSITIVE_FIXNUM)
            return make_fixnum((long)product);

          if (product >= LONG_MIN && product <= LONG_MAX)
            return make_value(new Bignum((long)product));
#endif
          mpz_t result;
          mpz_init_set_si(result, xlong(v1));
          mpz_mul_si(result, result, xlong(v2));
          Value v = normalize(result);
          MPZ_CLEAR(result);
          return v;
        }
      if (bignump(v2))
        {
          Bignum * b = the_bignum(v2);
          return b->multiply(xlong(v1));
        }
      if (ratiop(v2))
        {
          mpq_t q;
          mpq_init(q);
          mpq_set_si(q, xlong(v1), 1);
          mpq_mul(q, q, the_ratio(v2)->_q);
          Value value = normalize(q);
          MPQ_CLEAR(q);
          return value;
        }
      if (single_float_p(v2))
        return make_single_float(the_single_float(v2)->_f * xlong(v1));
      if (double_float_p(v2))
        return make_double_float(the_double_float(v2)->_d * xlong(v1));
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_complex(SYS_two_arg_star(v1, c->realpart()),
                              SYS_two_arg_star(v1, c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (bignump(v1))
    {
      if (fixnump(v2))
        {
          Bignum * b = the_bignum(v1);
          return b->multiply(xlong(v2));
        }
      if (bignump(v2))
        {
          Bignum * b1 = the_bignum(v1);
          Bignum * b2 = the_bignum(v2);
          return b1->multiply(b2);
        }
      if (ratiop(v2))
        return SYS_two_arg_star(v2, v1);
      if (single_float_p(v2))
        return SYS_two_arg_star(coerce_to_single_float(v1), v2);
      if (double_float_p(v2))
        return SYS_two_arg_star(coerce_to_double_float(v1), v2);
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_complex(SYS_two_arg_star(v1, c->realpart()),
                              SYS_two_arg_star(v1, c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (ratiop(v1))
    {
      if (fixnump(v2))
        {
          mpq_t q;
          mpq_init(q);
          mpq_set_si(q, xlong(v2), 1);
          mpq_mul(q, the_ratio(v1)->_q, q);
          Value value = normalize(q);
          MPQ_CLEAR(q);
          return value;
        }
      if (bignump(v2))
        {
          mpq_t q;
          mpq_init(q);
          mpq_set_z(q, the_bignum(v2)->_z);
          mpq_mul(q, the_ratio(v1)->_q, q);
          Value value = normalize(q);
          MPQ_CLEAR(q);
          return value;
        }
      if (ratiop(v2))
        {
          mpq_t q;
          mpq_init(q);
          mpq_mul(q, the_ratio(v1)->_q, the_ratio(v2)->_q);
          Value value = normalize(q);
          MPQ_CLEAR(q);
          return value;
        }
      if (single_float_p(v2))
        {
          v1 = coerce_to_single_float(v1);
          float f = the_single_float(v1)->_f * the_single_float(v2)->_f;
          return make_single_float(f);
        }
      if (double_float_p(v2))
        {
          v1 = coerce_to_double_float(v1);
          double d = the_double_float(v1)->_d * the_double_float(v2)->_d;
          return make_double_float(d);
        }
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_complex(SYS_two_arg_star(v1, c->realpart()),
                              SYS_two_arg_star(v1, c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (single_float_p(v1))
    {
      if (fixnump(v2))
        return make_single_float(the_single_float(v1)->_f * xlong(v2));
      if (bignump(v2))
        {
          float f1 = the_single_float(v1)->_f;
          float f2 = the_single_float(coerce_to_single_float(v2))->_f;
          return make_single_float(f1 * f2);
        }
      if (ratiop(v2))
        {
          mpf_t(f1);
          mpf_init_set_d(f1, the_single_float(v1)->_f);
          mpf_t(f2);
          mpf_init(f2);
          mpf_set_q(f2, the_ratio(v2)->_q);
          mpf_mul(f2, f2, f1);
          mpf_clear(f1);
          double d = mpf_get_d(f2);
          mpf_clear(f2);
          return make_single_float((float)d);
        }
      if (single_float_p(v2))
        return make_single_float(the_single_float(v1)->_f * the_single_float(v2)->_f);
      if (double_float_p(v2))
        return make_double_float(the_single_float(v1)->_f * the_double_float(v2)->_d);
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_complex(SYS_two_arg_star(v1, c->realpart()),
                              SYS_two_arg_star(v1, c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (double_float_p(v1))
    {
      if (fixnump(v2))
        return make_double_float(the_double_float(v1)->_d * xlong(v2));
      if (bignump(v2))
        {
          double d1 = the_double_float(v1)->_d;
          double d2 = the_double_float(coerce_to_double_float(v2))->_d;
          return make_double_float(d1 * d2);
        }
      if (ratiop(v2))
        {
          mpf_t(f1);
          mpf_init_set_d(f1, the_double_float(v1)->_d);
          mpf_t(f2);
          mpf_init(f2);
          mpf_set_q(f2, the_ratio(v2)->_q);
          mpf_mul(f2, f2, f1);
          mpf_clear(f1);
          double d = mpf_get_d(f2);
          mpf_clear(f2);
          return make_double_float(d);
        }
      if (single_float_p(v2))
        return make_double_float(the_double_float(v1)->_d * the_single_float(v2)->_f);
      if (double_float_p(v2))
        return make_double_float(the_double_float(v1)->_d * the_double_float(v2)->_d);
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_complex(SYS_two_arg_star(v1, c->realpart()),
                              SYS_two_arg_star(v1, c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (complexp(v1))
    {
      if (realp(v2))
        {
          Complex * c = the_complex(v1);
          return make_complex(SYS_two_arg_star(c->realpart(), v2),
                              SYS_two_arg_star(c->imagpart(), v2));
        }
      if (complexp(v2))
        {
          Value a = the_complex(v1)->realpart();
          Value b = the_complex(v1)->imagpart();
          Value c = the_complex(v2)->realpart();
          Value d = the_complex(v2)->imagpart();
          // xy = (ac - bd) + i(ad + bc)
          // real part = ac - bd
          // imag part = (a + b)(c + d) - ac - bd
          Value ac = SYS_two_arg_star(a, c);
          Value bd = SYS_two_arg_star(b, d);
          return make_complex(SYS_two_arg_minus(ac, bd),
                              SYS_two_arg_minus(SYS_two_arg_minus(SYS_two_arg_star(SYS_two_arg_plus(a, b), SYS_two_arg_plus(c, d)), ac), bd));
        }
      return signal_type_error(v2, S_number);
    }
  return signal_type_error(v1, S_number);
}

// ### two-arg-/
Value SYS_two_arg_slash(Value v1, Value v2)
{
TOP:
  long typecode1 = typecode_of(v1);
  long typecode2 = typecode_of(v2);
  if (v2 == 0)
    goto DIVIDE_BY_ZERO;
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpq_t q;
              mpq_init(q);
              long n1 = xlong(v1);
              long n2 = xlong(v2);
              if (n2 < 0)
                {
                  n1 = - n1;
                  n2 = - n2;
                }
              mpq_set_si(q, n1, n2);
              mpq_canonicalize(q);
              Value result = normalize(q);
              MPQ_CLEAR(q);
              return result;
            }
          case TYPECODE_BIGNUM:
            {
              mpq_t q;
              mpq_init(q);
              mpq_set_si(q, xlong(v1), 1);
              mpq_t q2;
              mpq_init(q2);
              mpq_set_z(q2, the_bignum(v2)->_z);
              mpq_div(q, q, q2);
              MPQ_CLEAR(q2);
              Value result = normalize(q);
              MPQ_CLEAR(q);
              return result;
            }
          case TYPECODE_RATIO:
            {
              mpq_t q;
              mpq_init(q);
              mpq_set_si(q, xlong(v1), 1);
              mpq_div(q, q, the_ratio(v2)->_q);
              Value result = normalize(q);
              MPQ_CLEAR(q);
              return result;
            }
          case TYPECODE_SINGLE_FLOAT:
            {
              float f = the_single_float(v2)->_f;
              if (f == 0)
                goto DIVIDE_BY_ZERO;
              return make_single_float(xlong(v1) / f);
            }
          case TYPECODE_DOUBLE_FLOAT:
            {
              double d = the_double_float(v2)->_d;
              if (d == 0)
                goto DIVIDE_BY_ZERO;
              return make_double_float(xlong(v1) / d);
            }
          case TYPECODE_COMPLEX:
            {
              Complex * c = the_complex(v2);
              Value realpart = c->realpart();
              Value imagpart = c->imagpart();
              Value denominator =
                SYS_two_arg_plus(SYS_two_arg_star(realpart, realpart), SYS_two_arg_star(imagpart, imagpart));
              return make_complex(SYS_two_arg_slash(SYS_two_arg_star(v1, realpart), denominator),
                                  SYS_two_arg_slash(SYS_two_arg_minus(FIXNUM_ZERO, SYS_two_arg_star(v1, imagpart)),
                                             denominator));
            }
          default:
            return signal_type_error(v2, S_number);
          }
      }
    case TYPECODE_BIGNUM:
      {
        if (fixnump(v2))
          {
            mpq_t q;
            mpq_init(q);
            mpq_set_z(q, the_bignum(v1)->_z);
            mpq_t q2;
            mpq_init(q2);
            mpq_set_si(q2, xlong(v2), 1);
            mpq_div(q, q, q2);
            MPQ_CLEAR(q2);
            Value result = normalize(q);
            MPQ_CLEAR(q);
            return result;
          }
        if (bignump(v2))
          {
            mpq_t q;
            mpq_init(q);
            mpq_set_z(q, the_bignum(v1)->_z);
            mpq_t q2;
            mpq_init(q2);
            mpq_set_z(q2, the_bignum(v2)->_z);
            mpq_div(q, q, q2);
            MPQ_CLEAR(q2);
            Value result = normalize(q);
            MPQ_CLEAR(q);
            return result;
          }
        if (ratiop(v2))
          {
            mpq_t q;
            mpq_init(q);
            mpq_set_z(q, the_bignum(v1)->_z);
            mpq_div(q, q, the_ratio(v2)->_q);
            Value value = normalize(q);
            MPQ_CLEAR(q);
            return value;
          }
        if (single_float_p(v2))
          {
            float f = the_single_float(v2)->_f;
            if (f == 0)
              goto DIVIDE_BY_ZERO;
            v1 = coerce_to_single_float(v1);
            return make_single_float(the_single_float(v1)->_f / f);
          }
        if (double_float_p(v2))
          {
            double d = the_double_float(v2)->_d;
            if (d == 0)
              goto DIVIDE_BY_ZERO;
            v1 = coerce_to_double_float(v1);
            return make_double_float(the_double_float(v1)->_d / d);
          }
        if (complexp(v2))
          {
            Complex * c = the_complex(v2);
            Value realpart = c->realpart();
            Value imagpart = c->imagpart();
            Value denominator =
              SYS_two_arg_plus(SYS_two_arg_star(realpart, realpart), SYS_two_arg_star(imagpart, imagpart));
            return make_complex(SYS_two_arg_slash(SYS_two_arg_star(v1, realpart), denominator),
                                SYS_two_arg_slash(SYS_two_arg_minus(FIXNUM_ZERO, SYS_two_arg_star(v1, imagpart)),
                                           denominator));
          }
        return signal_type_error(v2, S_number);
      }
    case TYPECODE_RATIO:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpq_t q;
              mpq_init(q);
              mpq_set_si(q, xlong(v2), 1);
              mpq_div(q, the_ratio(v1)->_q, q);
              Value value = normalize(q);
              MPQ_CLEAR(q);
              return value;
            }
          case TYPECODE_BIGNUM:
            {
              mpq_t q;
              mpq_init(q);
              mpq_set_z(q, the_bignum(v2)->_z);
              mpq_div(q, the_ratio(v1)->_q, q);
              Value value = normalize(q);
              MPQ_CLEAR(q);
              return value;
            }
          case TYPECODE_RATIO:
            {
              mpq_t q;
              mpq_init(q);
              mpq_div(q, the_ratio(v1)->_q, the_ratio(v2)->_q);
              Value value = normalize(q);
              MPQ_CLEAR(q);
              return value;
            }
          case TYPECODE_SINGLE_FLOAT:
            {
              v1 = coerce_to_single_float(v1);
              float f = the_single_float(v1)->_f / the_single_float(v2)->_f;
              return make_single_float(f);
            }
          case TYPECODE_DOUBLE_FLOAT:
            {
              v1 = coerce_to_double_float(v1);
              double d = the_double_float(v1)->_d / the_double_float(v2)->_d;
              return make_double_float(d);
            }
          case TYPECODE_COMPLEX:
            {
              Complex * c = the_complex(v2);
              Value v2_realpart = c->realpart();
              Value v2_imagpart = c->imagpart();
              // numerator
              Value realpart = SYS_two_arg_star(v1, v2_realpart);
              Value imagpart =
                SYS_two_arg_star(SYS_two_arg_minus(FIXNUM_ZERO, v1), v2_imagpart);
              // denominator
              Value d = SYS_two_arg_star(v2_realpart, v2_realpart);
              d = SYS_two_arg_plus(d, SYS_two_arg_star(v2_imagpart, v2_imagpart));
              return make_complex(SYS_two_arg_slash(realpart, d),
                                  SYS_two_arg_slash(imagpart, d));
            }
          default:
            return signal_type_error(v2, S_number);
          }
      }
    case TYPECODE_SINGLE_FLOAT:
      {
        if (v2 == 0)
          goto DIVIDE_BY_ZERO;
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return make_single_float(the_single_float(v1)->_f / xlong(v2));
          case TYPECODE_BIGNUM:
          case TYPECODE_RATIO:
            {
              v2 = coerce_to_single_float(v2);
              goto TOP;
            }
          case TYPECODE_SINGLE_FLOAT:
            {
              float f = the_single_float(v1)->_f / the_single_float(v2)->_f;
              return make_single_float(f);
            }
          case TYPECODE_DOUBLE_FLOAT:
            {
              double d = the_single_float(v1)->_f / the_double_float(v2)->_d;
              return make_double_float(d);
            }
          case TYPECODE_COMPLEX:
            {
              Complex * c = the_complex(v2);
              Value v2_realpart = c->realpart();
              Value v2_imagpart = c->imagpart();
              // numerator
              Value realpart = SYS_two_arg_star(v1, v2_realpart);
              Value imagpart =
                SYS_two_arg_star(SYS_two_arg_minus(FIXNUM_ZERO, v1), v2_imagpart);
              // denominator
              Value d = SYS_two_arg_star(v2_realpart, v2_realpart);
              d = SYS_two_arg_plus(d, SYS_two_arg_star(v2_imagpart, v2_imagpart));
              return make_complex(SYS_two_arg_slash(realpart, d),
                                  SYS_two_arg_slash(imagpart, d));
            }
          default:
            return signal_type_error(v2, S_number);
          }
      }
    case TYPECODE_DOUBLE_FLOAT:
      {
        if (v2 == 0)
          goto DIVIDE_BY_ZERO;
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return make_double_float(the_double_float(v1)->_d / xlong(v2));
          case TYPECODE_BIGNUM:
          case TYPECODE_RATIO:
            return SYS_two_arg_slash(v1, coerce_to_double_float(v2));
          case TYPECODE_SINGLE_FLOAT:
            {
              double d = the_double_float(v1)->_d / the_single_float(v2)->_f;
              return make_double_float(d);
            }
          case TYPECODE_DOUBLE_FLOAT:
            {
              double d = the_double_float(v1)->_d / the_double_float(v2)->_d;
              return make_double_float(d);
            }
          case TYPECODE_COMPLEX:
            {
              Complex * c = the_complex(v2);
              Value v2_realpart = c->realpart();
              Value v2_imagpart = c->imagpart();
              // numerator
              Value realpart = SYS_two_arg_star(v1, v2_realpart);
              Value imagpart =
                SYS_two_arg_star(SYS_two_arg_minus(FIXNUM_ZERO, v1), v2_imagpart);
              // denominator
              Value d = SYS_two_arg_star(v2_realpart, v2_realpart);
              d = SYS_two_arg_plus(d, SYS_two_arg_star(v2_imagpart, v2_imagpart));
              return make_complex(SYS_two_arg_slash(realpart, d),
                                  SYS_two_arg_slash(imagpart, d));
            }
          default:
            return signal_type_error(v2, S_number);
          }
      }
    case TYPECODE_COMPLEX:
      {
        if (complexp(v2))
          {
            Value a = the_complex(v1)->realpart();
            Value b = the_complex(v1)->imagpart();
            Value c = the_complex(v2)->realpart();
            Value d = the_complex(v2)->imagpart();
            Value ac = SYS_two_arg_star(a, c);
            Value bd = SYS_two_arg_star(b, d);
            Value bc = SYS_two_arg_star(b, c);
            Value ad = SYS_two_arg_star(a, d);
            Value denominator = SYS_two_arg_plus(SYS_two_arg_star(c, c), SYS_two_arg_star(d, d));
            return make_complex(SYS_two_arg_slash(SYS_two_arg_plus(ac, bd), denominator),
                                SYS_two_arg_slash(SYS_two_arg_minus(bc, ad), denominator));
          }
        else
          return make_complex(SYS_two_arg_slash(the_complex(v1)->realpart(), v2),
                              SYS_two_arg_slash(the_complex(v1)->imagpart(), v2));
        return signal_type_error(v2, S_number);
      }
      default:
        return signal_type_error(v1, S_number);
    }
 DIVIDE_BY_ZERO:
  return signal_lisp_error(new DivisionByZero(S_two_arg_slash, list2(v1, v2)));
}

// ### two-arg-+
Value SYS_two_arg_plus(Value v1, Value v2)
{
  long typecode1 = typecode_of(v1);
  long typecode2 = typecode_of(v2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        long n1 = xlong(v1);
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              long n = n1 + xlong(v2);
              if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
                return make_fixnum(n);
              return make_value(new Bignum((long)n));
            }
          case TYPECODE_BIGNUM:
            return the_bignum(v2)->add(n1);
          case TYPECODE_RATIO:
            return the_ratio(v2)->add(n1);
          case TYPECODE_SINGLE_FLOAT:
            return the_single_float(v2)->add(n1);
          case TYPECODE_DOUBLE_FLOAT:
            return the_double_float(v2)->add(n1);
          case TYPECODE_COMPLEX:
            return the_complex(v2)->add(n1);
          default:
            return signal_type_error(v2, S_number);
          }
      }
    case TYPECODE_BIGNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return the_bignum(v1)->add(xlong(v2));
          case TYPECODE_BIGNUM:
            return the_bignum(v1)->add(the_bignum(v2));
          case TYPECODE_RATIO:
            return the_ratio(v2)->add(the_bignum(v1));
          case TYPECODE_SINGLE_FLOAT:
            return the_single_float(coerce_to_single_float(v1))->add(the_single_float(v2));
          case TYPECODE_DOUBLE_FLOAT:
            return the_double_float(coerce_to_double_float(v1))->add(the_double_float(v2));
          case TYPECODE_COMPLEX:
            {
              Complex * c = the_complex(v2);
              return make_value(new Complex(SYS_two_arg_plus(v1, c->realpart()), c->imagpart()));
            }
          default:
            return signal_type_error(v2, S_number);
          }
      }
    case TYPECODE_RATIO:
      {
        if (fixnump(v2))
          return the_ratio(v1)->add(xlong(v2));
        if (bignump(v2))
          return the_ratio(v1)->add(the_bignum(v2));
        if (ratiop(v2))
          return the_ratio(v1)->add(the_ratio(v2));
        if (single_float_p(v2))
          return the_single_float(coerce_to_single_float(v1))->add(the_single_float(v2));
        if (double_float_p(v2))
          return the_double_float(coerce_to_double_float(v1))->add(the_double_float(v2));
        if (complexp(v2))
          {
            Complex * c = the_complex(v2);
            return make_value(new Complex(SYS_two_arg_plus(v1, c->realpart()), c->imagpart()));
          }
        return signal_type_error(v2, S_number);
      }
    case TYPECODE_SINGLE_FLOAT:
      {
        if (fixnump(v2))
          return the_single_float(v1)->add(xlong(v2));
        if (bignump(v2))
          return the_single_float(v1)->add(the_single_float(coerce_to_single_float(v2)));
        if (ratiop(v2))
          return the_single_float(v1)->add(the_single_float(coerce_to_single_float(v2)));
        if (single_float_p(v2))
          return the_single_float(v1)->add(the_single_float(v2));
        if (double_float_p(v2))
          return the_single_float(v1)->add(the_double_float(v2));
        if (complexp(v2))
          {
            Complex * c = the_complex(v2);
            return make_value(new Complex(SYS_two_arg_plus(v1, c->realpart()), c->imagpart()));
          }
        return signal_type_error(v2, S_number);
      }
    case TYPECODE_DOUBLE_FLOAT:
      {
        if (fixnump(v2))
          return the_double_float(v1)->add(xlong(v2));
        if (bignump(v2))
          return the_double_float(v1)->add(the_double_float(coerce_to_double_float(v2)));
        if (ratiop(v2))
          return the_double_float(v1)->add(the_double_float(coerce_to_double_float(v2)));
        if (single_float_p(v2))
          return the_double_float(v1)->add(the_single_float(v2));
        if (double_float_p(v2))
          return the_double_float(v1)->add(the_double_float(v2));
        if (complexp(v2))
          {
            Complex * c = the_complex(v2);
            return make_value(new Complex(SYS_two_arg_plus(v1, c->realpart()), c->imagpart()));
          }
        return signal_type_error(v2, S_number);
      }
    case TYPECODE_COMPLEX:
      {
        Complex * c = the_complex(v1);
        if (fixnump(v2))
          return c->add(xlong(v2));
        if (complexp(v2))
          return c->add(the_complex(v2));
        return make_value(new Complex(SYS_two_arg_plus(c->realpart(), v2), c->imagpart()));
      }
    default:
      return signal_type_error(v1, S_number);
    }
}

// ### two-arg--
Value SYS_two_arg_minus(Value v1, Value v2)
{
  if (fixnump(v1))
    {
      if (fixnump(v2))
        {
          {
            long n = xlong(v1) - xlong(v2);
            if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
              return make_fixnum(n);
            return make_value(new Bignum((long)n));
          }
        }
      if (bignump(v2))
        return SYS_two_arg_plus(the_bignum(v2)->negate(), v1);
      if (ratiop(v2))
        return SYS_two_arg_plus(the_ratio(v2)->negate(), v1);
      if (single_float_p(v2))
        return SYS_two_arg_plus(the_single_float(v2)->negate(), v1);
      if (double_float_p(v2))
        return SYS_two_arg_plus(the_double_float(v2)->negate(), v1);
      if (complexp(v2))
        return SYS_two_arg_plus(the_complex(v2)->negate(), v1);
      return signal_type_error(v2, S_number);
    }
  if (bignump(v1))
    {
      if (fixnump(v2))
        return the_bignum(v1)->subtract(xlong(v2));
      if (bignump(v2))
        return the_bignum(v1)->subtract(the_bignum(v2));
      if (ratiop(v2))
        return SYS_two_arg_plus(v1, the_ratio(v2)->negate());
      if (single_float_p(v2))
        return the_single_float(coerce_to_single_float(v1))->subtract(the_single_float(v2));
      if (double_float_p(v2))
        return the_double_float(coerce_to_double_float(v1))->subtract(the_double_float(v2));
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_value(new Complex(SYS_two_arg_minus(v1, c->realpart()), c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (ratiop(v1))
    {
      if (fixnump(v2))
        return the_ratio(v1)->subtract(xlong(v2));
      if (bignump(v2))
        return SYS_two_arg_plus(v1, the_bignum(v2)->negate());
      if (ratiop(v2))
        return the_ratio(v1)->subtract(the_ratio(v2));
      if (single_float_p(v2))
        return the_single_float(coerce_to_single_float(v1))->subtract(the_single_float(v2));
      if (double_float_p(v2))
        return the_double_float(coerce_to_double_float(v1))->subtract(the_double_float(v2));
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_value(new Complex(SYS_two_arg_minus(v1, c->realpart()), c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (single_float_p(v1))
    {
      if (fixnump(v2))
        return the_single_float(v1)->subtract(xlong(v2));
      if (bignump(v2))
        return the_single_float(v1)->subtract(the_single_float(coerce_to_single_float(v2)));
      if (ratiop(v2))
        {
          mpf_t f;
          mpf_init(f);
          mpf_set_q(f, the_ratio(v2)->_q);
          Value value = make_single_float(the_single_float(v1)->_f - (float) mpf_get_d(f));
          mpf_clear(f);
          return value;
        }
      if (single_float_p(v2))
        return the_single_float(v1)->subtract(the_single_float(v2));
      if (double_float_p(v2))
        return the_single_float(v1)->subtract(the_double_float(v2));
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_value(new Complex(SYS_two_arg_minus(v1, c->realpart()), c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (double_float_p(v1))
    {
      if (fixnump(v2))
        return the_double_float(v1)->subtract(xlong(v2));
      if (bignump(v2))
        return the_double_float(v1)->subtract(the_double_float(coerce_to_double_float(v2)));
      if (ratiop(v2))
        {
          mpf_t f;
          mpf_init(f);
          mpf_set_q(f, the_ratio(v2)->_q);
          Value value = make_double_float(the_double_float(v1)->_d - mpf_get_d(f));
          mpf_clear(f);
          return value;
        }
      if (single_float_p(v2))
        return the_double_float(v1)->subtract(the_single_float(v2));
      if (double_float_p(v2))
        return the_double_float(v1)->subtract(the_double_float(v2));
      if (complexp(v2))
        {
          Complex * c = the_complex(v2);
          return make_value(new Complex(SYS_two_arg_minus(v1, c->realpart()), c->imagpart()));
        }
      return signal_type_error(v2, S_number);
    }
  if (complexp(v1))
    {
      Complex * c = the_complex(v1);
      if (fixnump(v2))
        return c->subtract(xlong(v2));
      if (complexp(v2))
        return c->subtract(the_complex(v2));
      return make_value(new Complex(SYS_two_arg_minus(c->realpart(), v2), c->imagpart()));
    }
  return signal_type_error(v1, S_number);
}

// ### +
Value CL_add(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return FIXNUM_ZERO;
    case 1:
      if (numberp(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_number);
    case 2:
      return SYS_two_arg_plus(args[0], args[1]);
    default:
      {
        Value result = make_fixnum(0);
        for (unsigned int i = 0; i < numargs; i++)
          result = SYS_two_arg_plus(result, args[i]);
        return result;
      }
    }
}

// ### -
Value CL_subtract(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_minus, 0, 1, -1);
    case 1:
      {
        if (fixnump(args[0]))
          {
            long n = xlong(args[0]);
            if (n == MOST_NEGATIVE_FIXNUM)
              return make_integer(-n);
            else
              return make_fixnum(-n);
          }
        if (bignump(args[0]))
          return the_bignum(args[0])->negate();
        if (ratiop(args[0]))
          return the_ratio(args[0])->negate();
        if (single_float_p(args[0]))
          return the_single_float(args[0])->negate();
        if (double_float_p(args[0]))
          return the_double_float(args[0])->negate();
        if (complexp(args[0]))
          return the_complex(args[0])->negate();
        return signal_type_error(args[0], S_number);
      }
    case 2:
      return SYS_two_arg_minus(args[0], args[1]);
    default:
      {
        Value result = args[0];
        for (unsigned int i = 1; i < numargs; i++)
          result = SYS_two_arg_minus(result, args[i]);
        return result;
      }
    }
}

Value CL_multiply(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return make_fixnum(1);
    case 1:
      if (numberp(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_number);
    case 2:
      return SYS_two_arg_star(args[0], args[1]);
    default:
      {
        Value result = args[0];
        for (unsigned int i = 1; i < numargs; i++)
          result = SYS_two_arg_star(result, args[i]);
        return result;
      }
    }
}

Value CL_divide(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_slash, numargs, 1, MANY);
    case 1:
      return SYS_two_arg_slash(FIXNUM_ONE, args[0]);
    case 2:
      return SYS_two_arg_slash(args[0], args[1]);
    default:
      {
        Value result = args[0];
        for (unsigned int i = 1; i < numargs; i++)
          result = SYS_two_arg_slash(result, args[i]);
        return result;
      }
    }
}

// ### 1+
Value CL_one_plus(Value arg)
{
  return SYS_two_arg_plus(arg, FIXNUM_ONE);
}

// ### 1-
Value CL_one_minus(Value arg)
{
  return SYS_two_arg_minus(arg, FIXNUM_ONE);
}

bool equals(Value arg1, Value arg2)
{
 top:
  if (arg1 == arg2)
    return true;
  if (fixnump(arg1))
    {
      if (fixnump(arg2))
        return false;
      if (bignump(arg2))
        return false;
      if (ratiop(arg2))
        return false;
      if (single_float_p(arg2))
        {
          arg2 = the_single_float(arg2)->rational();
          goto top;
        }
      if (double_float_p(arg2))
        {
          arg2 = the_double_float(arg2)->rational();
          goto top;
        }
      if (complexp(arg2))
        return equals(arg2, arg1);
      signal_type_error(arg2, S_number);
      // not reached
      return false;
    }
  if (bignump(arg1))
    {
     bignump:
      if (fixnump(arg2))
        return false;
      if (bignump(arg2))
        return mpz_cmp(the_bignum(arg1)->_z, the_bignum(arg2)->_z) == 0;
      if (ratiop(arg2))
        return false;
      if (single_float_p(arg2))
        {
          arg2 = the_single_float(arg2)->rational();
          goto bignump;
        }
      if (double_float_p(arg2))
        {
          arg2 = the_double_float(arg2)->rational();
          goto bignump;
        }
      if (complexp(arg2))
        return equals(arg2, arg1);
      signal_type_error(arg2, S_number);
      // not reached
      return false;
    }
  if (ratiop(arg1))
    {
     ratiop:
      if (integerp(arg2))
        return false;
      if (ratiop(arg2))
        return the_ratio(arg1)->eql(the_ratio(arg2));
      if (single_float_p(arg2))
        {
          arg2 = the_single_float(arg2)->rational();
          goto ratiop;
        }
      if (double_float_p(arg2))
        {
          arg2 = the_double_float(arg2)->rational();
          goto ratiop;
        }
      if (complexp(arg2))
        return false;
      signal_type_error(arg2, S_number);
      // not reached
      return false;
    }
  if (single_float_p(arg1))
    {
      if (fixnump(arg2) || bignump(arg2) || ratiop(arg2))
        return equals(the_single_float(arg1)->rational(), arg2);
      if (single_float_p(arg2))
        return the_single_float(arg1)->_f == the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return the_single_float(arg1)->_f == the_double_float(arg2)->_d;
      if (complexp(arg2))
        return equals(arg2, arg1);
      signal_type_error(arg2, S_number);
      // not reached
      return false;
    }
  if (double_float_p(arg1))
    {
      if (fixnump(arg2) || bignump(arg2) || ratiop(arg2))
        return equals(the_double_float(arg1)->rational(), arg2);
      if (single_float_p(arg2))
        return the_double_float(arg1)->_d == the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return the_double_float(arg1)->_d == the_double_float(arg2)->_d;
      if (complexp(arg2))
        return equals(arg2, arg1);
      signal_type_error(arg2, S_number);
      // not reached
      return false;
    }
  if (complexp(arg1))
    {
      if (complexp(arg2))
        {
          Complex * c1 = the_complex(arg1);
          Complex * c2 = the_complex(arg2);
          return (equals(c1->realpart(), c2->realpart())
                  && equals (c1->imagpart(), c2->imagpart()));
        }
      if (numberp(arg2))
        {
          Complex * c1 = the_complex(arg1);
          return (equals(c1->imagpart(), FIXNUM_ZERO)
                  && equals(c1->realpart(), arg2));
        }
      signal_type_error(arg2, S_number);
      // not reached
      return false;
    }
  signal_type_error(arg1, S_number);
  // not reached
  return false;
}

bool lt(Value arg1, Value arg2)
{
  long typecode1 = typecode_of(arg1);
  long typecode2 = typecode_of(arg2);
 top:
  switch (typecode1)
    {
      case TYPECODE_FIXNUM:
        {
          long n1 = xlong(arg1);
          switch (typecode2)
            {
            case TYPECODE_FIXNUM:
              return n1 < xlong(arg2);
            case TYPECODE_BIGNUM:
              return the_bignum(arg2)->plusp();
            case TYPECODE_RATIO:
              {
                mpq_t q1;
                mpq_init(q1);
                mpq_set_si(q1, n1, 1);
                bool result = mpq_cmp(q1, the_ratio(arg2)->_q) < 0;
                MPQ_CLEAR(q1);
                return result;
              }
            case TYPECODE_SINGLE_FLOAT:
              arg2 = the_single_float(arg2)->rational();
              typecode2 = typecode_of(arg2);
              goto top;
            case TYPECODE_DOUBLE_FLOAT:
              arg2 = the_double_float(arg2)->rational();
              typecode2 = typecode_of(arg2);
              goto top;
            default:
              signal_type_error(arg2, S_real);
              // not reached
              return false;
            }
        }
    case TYPECODE_BIGNUM:
      {
       bignump:
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return the_bignum(arg1)->minusp();
          case TYPECODE_BIGNUM:
            return mpz_cmp(the_bignum(arg1)->_z, the_bignum(arg2)->_z) < 0;
          case TYPECODE_RATIO:
            {
              mpq_t q1;
              mpq_init(q1);
              mpq_set_z(q1, the_bignum(arg1)->_z);
              bool result = mpq_cmp(q1, the_ratio(arg2)->_q) < 0;
              MPQ_CLEAR(q1);
              return result;
            }
          case TYPECODE_SINGLE_FLOAT:
            {
              arg2 = the_single_float(arg2)->rational();
              typecode2 = typecode_of(arg2);
              goto bignump;
            }
          case TYPECODE_DOUBLE_FLOAT:
            {
              arg2 = the_double_float(arg2)->rational();
              typecode2 = typecode_of(arg2);
              goto bignump;
            }
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_RATIO:
      {
       ratiop:
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpq_t q2;
              mpq_init(q2);
              mpq_set_si(q2, xlong(arg2), 1);
              bool result = mpq_cmp(the_ratio(arg1)->_q, q2) < 0;
              MPQ_CLEAR(q2);
              return result;
            }
          case TYPECODE_BIGNUM:
            {
              mpq_t q2;
              mpq_init(q2);
              mpq_set_z(q2, the_bignum(arg2)->_z);
              bool result = mpq_cmp(the_ratio(arg1)->_q, q2) < 0;
              MPQ_CLEAR(q2);
              return result;
            }
          case TYPECODE_RATIO:
            return mpq_cmp(the_ratio(arg1)->_q, the_ratio(arg2)->_q) < 0;
          case TYPECODE_SINGLE_FLOAT:
            {
              arg2 = the_single_float(arg2)->rational();
              typecode2 = typecode_of(arg2);
              goto ratiop;
            }
          case TYPECODE_DOUBLE_FLOAT:
            {
              arg2 = the_double_float(arg2)->rational();
              typecode2 = typecode_of(arg2);
              goto ratiop;
            }
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_SINGLE_FLOAT:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            arg1 = the_single_float(arg1)->rational();
            typecode1 = typecode_of(arg1);
            goto top;
          case TYPECODE_BIGNUM:
          case TYPECODE_RATIO:
            {
              arg1 = the_single_float(arg1)->rational();
              typecode1 = typecode_of(arg1);
              goto top;
            }
          case TYPECODE_SINGLE_FLOAT:
            return the_single_float(arg1)->_f < the_single_float(arg2)->_f;
          case TYPECODE_DOUBLE_FLOAT:
            return the_single_float(arg1)->_f < the_double_float(arg2)->_d;
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_DOUBLE_FLOAT:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            arg1 = the_double_float(arg1)->rational();
            typecode1 = typecode_of(arg1);
            goto top;
          case TYPECODE_BIGNUM:
          case TYPECODE_RATIO:
            {
              arg1 = the_double_float(arg1)->rational();
              typecode1 = typecode_of(arg1);
              goto top;
            }
          case TYPECODE_SINGLE_FLOAT:
            return the_double_float(arg1)->_d < the_single_float(arg2)->_f;
          case TYPECODE_DOUBLE_FLOAT:
            return the_double_float(arg1)->_d < the_double_float(arg2)->_d;
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    default:
      signal_type_error(arg1, S_real);
      // not reached
      return false;
    }
}

bool le(Value arg1, Value arg2)
{
  long typecode1 = typecode_of(arg1);
  long typecode2 = typecode_of(arg2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return xlong(arg1) <= xlong(arg2);
          case TYPECODE_BIGNUM:
            return the_bignum(arg2)->plusp();
          case TYPECODE_RATIO:
            return ge(arg2, arg1);
          case TYPECODE_SINGLE_FLOAT:
            return xlong(arg1) <= the_single_float(arg2)->_f;
          case TYPECODE_DOUBLE_FLOAT:
            return xlong(arg1) <= the_double_float(arg2)->_d;
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_BIGNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return the_bignum(arg1)->minusp();
          case TYPECODE_BIGNUM:
            return compare(the_bignum(arg1), the_bignum(arg2)) <= 0;
          case TYPECODE_RATIO:
            return ge(arg2, arg1);
          case TYPECODE_SINGLE_FLOAT:
            return le(arg1, the_single_float(arg2)->rational());
          case TYPECODE_DOUBLE_FLOAT:
            return le(arg1, the_double_float(arg2)->rational());
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_RATIO:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpq_t q2;
              mpq_init(q2);
              mpq_set_si(q2, xlong(arg2), 1);
              bool result = mpq_cmp(the_ratio(arg1)->_q, q2) <= 0;
              MPQ_CLEAR(q2);
              return result;
            }
          case TYPECODE_BIGNUM:
            {
              mpq_t q2;
              mpq_init(q2);
              mpq_set_z(q2, the_bignum(arg2)->_z);
              bool result = mpq_cmp(the_ratio(arg1)->_q, q2) <= 0;
              MPQ_CLEAR(q2);
              return result;
            }
          case TYPECODE_RATIO:
            return mpq_cmp(the_ratio(arg1)->_q, the_ratio(arg2)->_q) <= 0;
          case TYPECODE_SINGLE_FLOAT:
            return le(arg1, the_single_float(arg2)->rational());
          case TYPECODE_DOUBLE_FLOAT:
            return le(arg1, the_double_float(arg2)->rational());
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_SINGLE_FLOAT:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return the_single_float(arg1)->_f <= xlong(arg2);
          case TYPECODE_BIGNUM:
            return le(the_single_float(arg1)->rational(), arg2);
          case TYPECODE_RATIO:
            return ge(arg2, arg1);
          case TYPECODE_SINGLE_FLOAT:
            return the_single_float(arg1)->_f <= the_single_float(arg2)->_f;
          case TYPECODE_DOUBLE_FLOAT:
            return the_single_float(arg1)->_f <= the_double_float(arg2)->_d;
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    case TYPECODE_DOUBLE_FLOAT:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            return the_double_float(arg1)->_d <= xlong(arg2);
          case TYPECODE_BIGNUM:
            return le(the_double_float(arg1)->rational(), arg2);
          case TYPECODE_RATIO:
            return ge(arg2, arg1);
          case TYPECODE_SINGLE_FLOAT:
            return the_double_float(arg1)->_d <= the_single_float(arg2)->_f;
          case TYPECODE_DOUBLE_FLOAT:
            return the_double_float(arg1)->_d <= the_double_float(arg2)->_d;
          default:
            signal_type_error(arg2, S_real);
            // not reached
            return false;
          }
      }
    default:
      signal_type_error(arg1, S_real);
      // not reached
      return false;
    }
}

bool gt(Value arg1, Value arg2)
{
 top:
  if (fixnump(arg1))
    {
     fixnump:
      if (fixnump(arg2))
        return xlong(arg1) > xlong(arg2);
      if (bignump(arg2))
        return the_bignum(arg2)->minusp();
      if (ratiop(arg2))
        return lt(arg2, arg1);
      if (single_float_p(arg2))
        {
          arg2 = the_single_float(arg2)->rational();
          goto fixnump;
        }
      if (double_float_p(arg2))
        {
          arg2 = the_double_float(arg2)->rational();
          goto fixnump;
        }
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (bignump(arg1))
    {
      if (fixnump(arg2))
        return the_bignum(arg1)->plusp();
      if (bignump(arg2))
        return compare(the_bignum(arg1), the_bignum(arg2)) > 0;
      if (ratiop(arg2))
        return lt(arg2, arg1);
      if (single_float_p(arg2))
        return gt(arg1, the_single_float(arg2)->rational());
      if (double_float_p(arg2))
        return gt(arg1, the_double_float(arg2)->rational());
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (ratiop(arg1))
    {
      if (fixnump(arg2))
        {
          mpq_t q2;
          mpq_init(q2);
          mpq_set_si(q2, xlong(arg2), 1);
          bool result = mpq_cmp(the_ratio(arg1)->_q, q2) > 0;
          MPQ_CLEAR(q2);
          return result;
        }
      if (bignump(arg2))
        {
          mpq_t q2;
          mpq_init(q2);
          mpq_set_z(q2, the_bignum(arg2)->_z);
          bool result = mpq_cmp(the_ratio(arg1)->_q, q2) > 0;
          MPQ_CLEAR(q2);
          return result;
        }
      if (ratiop(arg2))
        return mpq_cmp(the_ratio(arg1)->_q, the_ratio(arg2)->_q) > 0;
      if (single_float_p(arg2))
        return gt(arg1, the_single_float(arg2)->rational());
      if (double_float_p(arg2))
        return gt(arg1, the_double_float(arg2)->rational());
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (single_float_p(arg1))
    {
      if (fixnump(arg2))
        {
          arg1 = the_single_float(arg1)->rational();
          goto top;
        }
      if (bignump(arg2))
        return gt(the_single_float(arg1)->rational(), arg2);
      if (ratiop(arg2))
        return lt(arg2, arg1);
      if (single_float_p(arg2))
        return the_single_float(arg1)->_f > the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return the_single_float(arg1)->_f > the_double_float(arg2)->_d;
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (double_float_p(arg1))
    {
      if (fixnump(arg2))
        {
          arg1 = the_double_float(arg1)->rational();
          goto top;
        }
      if (bignump(arg2))
        return gt(the_double_float(arg1)->rational(), arg2);
      if (ratiop(arg2))
        return lt(arg2, arg1);
      if (single_float_p(arg2))
        return the_double_float(arg1)->_d > the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return the_double_float(arg1)->_d > the_double_float(arg2)->_d;
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  signal_type_error(arg1, S_real);
  // not reached
  return false;
}

bool ge(Value arg1, Value arg2)
{
  if (fixnump(arg1))
    {
      if (fixnump(arg2))
        return xlong(arg1) >= xlong(arg2);
      if (bignump(arg2))
        return the_bignum(arg2)->minusp();
      if (ratiop(arg2))
        return le(arg2, arg1);
      if (single_float_p(arg2))
        return xlong(arg1) >= the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return xlong(arg1) >= the_double_float(arg2)->_d;
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (bignump(arg1))
    {
      if (fixnump(arg2))
        return the_bignum(arg1)->plusp();
      if (bignump(arg2))
        return compare(the_bignum(arg1), the_bignum(arg2)) >= 0;
      if (ratiop(arg2))
        return le(arg2, arg1);
      if (single_float_p(arg2))
        return ge(arg1, the_single_float(arg2)->rational());
      if (double_float_p(arg2))
        return ge(arg1, the_double_float(arg2)->rational());
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (ratiop(arg1))
    {
      if (fixnump(arg2))
        {
          mpq_t q2;
          mpq_init(q2);
          mpq_set_si(q2, xlong(arg2), 1);
          bool result = mpq_cmp(the_ratio(arg1)->_q, q2) >= 0;
          MPQ_CLEAR(q2);
          return result;
        }
      if (bignump(arg2))
        {
          mpq_t q2;
          mpq_init(q2);
          mpq_set_z(q2, the_bignum(arg2)->_z);
          bool result = mpq_cmp(the_ratio(arg1)->_q, q2) >= 0;
          MPQ_CLEAR(q2);
          return result;
        }
      if (ratiop(arg2))
        return mpq_cmp(the_ratio(arg1)->_q, the_ratio(arg2)->_q) >= 0;
      if (single_float_p(arg2))
        return ge(arg1, the_single_float(arg2)->rational());
      if (double_float_p(arg2))
        return ge(arg1, the_double_float(arg2)->rational());
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (single_float_p(arg1))
    {
      if (fixnump(arg2))
        return the_single_float(arg1)->_f >= xlong(arg2);
      if (bignump(arg2))
        return ge(the_single_float(arg1)->rational(), arg2);
      if (ratiop(arg2))
        return le(arg2, arg1);
      if (single_float_p(arg2))
        return the_single_float(arg1)->_f >= the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return the_single_float(arg1)->_f >= the_double_float(arg2)->_d;
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  if (double_float_p(arg1))
    {
      if (fixnump(arg2))
        return the_double_float(arg1)->_d >= xlong(arg2);
      if (bignump(arg2))
        return ge(the_double_float(arg1)->rational(), arg2);
      if (ratiop(arg2))
        return le(arg2, arg1);
      if (single_float_p(arg2))
        return the_double_float(arg1)->_d >= the_single_float(arg2)->_f;
      if (double_float_p(arg2))
        return the_double_float(arg1)->_d >= the_double_float(arg2)->_d;
      signal_type_error(arg2, S_real);
      // not reached
      return false;
    }
  signal_type_error(arg1, S_real);
  // not reached
  return false;
}

// ### two-arg-=
Value SYS_equals_2(Value arg1, Value arg2)
{
  return equals(arg1, arg2) ? T : NIL;
}

// ### =
Value CL_equals(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_equals, numargs, 1, MANY);
    case 1:
      if (numberp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_number);
    case 2:
      return equals(args[0], args[1]) ? T : NIL;
    case 3:
      return (equals(args[0], args[1]) && equals(args[1], args[2])) ? T : NIL;
    default:
      {
        const Value value = args[0];
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (!equals(value, args[i]))
              return NIL;
          }
        return T;
      }
    }
}

// ### two-arg-/=
Value SYS_not_equals_2(Value arg1, Value arg2)
{
  return equals(arg1, arg2) ? NIL : T;
}

// ### /=
Value CL_not_equals(unsigned int numargs, Value args[])
{
  // "The value of /= is true if no two numbers are the same in value;
  // otherwise it is false."
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_equals, numargs, 1, MANY);
    case 1:
      if (numberp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_number);
    case 2:
      return equals(args[0], args[1]) ? NIL : T;
    case 3:
      if (equals(args[0], args[1]))
        return NIL;
      if (equals(args[0], args[2]))
        return NIL;
      if (equals(args[1], args[2]))
        return NIL;
      return T;
    default:
      {
        for (unsigned int i = 0; i < numargs; i++)
          {
            const Value value = args[i];
            for (unsigned int j = i + 1; j < numargs; j++)
              {
                if (equals(args[j],value))
                  return NIL;
              }
          }
        return T;
      }
    }
}

// ### two-arg-<
Value SYS_lt_2(Value arg1, Value arg2)
{
  return lt(arg1, arg2) ? T : NIL;
}

// ### <
Value CL_lt(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_lt, numargs, 1, MANY);
    case 1:
      return T;
    case 2:
      return lt(args[0], args[1]) ? T : NIL;
    case 3:
      return (lt(args[0], args[1]) && lt(args[1], args[2])) ? T : NIL;
    default:
      for (unsigned int i = 1; i < numargs; i++)
        {
          if (ge(args[i - 1], args[i]))
            return NIL;
        }
      return T;
    }
}

// ### two-arg-<=
Value SYS_le_2(Value arg1, Value arg2)
{
  return le(arg1, arg2) ? T : NIL;
}

// ### <=
Value CL_le(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_le, numargs, 1, MANY);
    case 1:
      return T;
    case 2:
      return le(args[0], args[1]) ? T : NIL;
    case 3:
      return (le(args[0], args[1]) && le(args[1], args[2])) ? T : NIL;
    default:
      for (unsigned int i = 1; i < numargs; i++)
        {
          if (gt(args[i - 1], args[i]))
            return NIL;
        }
      return T;
    }
}

// ### two-arg->
Value SYS_gt_2(Value arg1, Value arg2)
{
  return gt(arg1, arg2) ? T : NIL;
}

// ### >
Value CL_gt(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_gt, numargs, 1, MANY);
    case 1:
      return T;
    case 2:
      return gt(args[0], args[1]) ? T : NIL;
    case 3:
      return (gt(args[0], args[1]) && gt(args[1], args[2])) ? T : NIL;
    default:
      for (unsigned int i = 1; i < numargs; i++)
        {
          if (le(args[i - 1], args[i]))
            return NIL;
        }
      return T;
    }
}

// ### two-arg->=
Value SYS_ge_2(Value arg1, Value arg2)
{
  return ge(arg1, arg2) ? T : NIL;
}

// ### >=
Value CL_ge(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_ge, numargs, 1, MANY);
    case 1:
      return T;
    case 2:
      return ge(args[0], args[1]) ? T : NIL;
    case 3:
      return (ge(args[0], args[1]) && ge(args[1], args[2])) ? T : NIL;
    default:
      for (unsigned int i = 1; i < numargs; i++)
        {
          if (lt(args[i - 1], args[i]))
            return NIL;
        }
      return T;
    }
}

// ### two-arg-max
Value SYS_two_arg_max(Value arg1, Value arg2)
{
  return lt(arg1, arg2) ? arg2 : arg1;
}

// ### max
Value CL_max(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_max, numargs, 1, MANY);
    case 1:
      if (realp(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_real);
    case 2:
      if (lt(args[0], args[1]))
        return args[1];
      else
        return args[0];
    default:
      {
        if (!realp(args[0]))
          return signal_type_error(args[0], S_real);
        Value result = args[0];
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (lt(result, args[i]))
              result = args[i];
          }
        return result;
      }
    }
}

// ### two-arg-min
Value SYS_two_arg_min(Value arg1, Value arg2)
{
  return lt(arg1, arg2) ? arg1 : arg2;
}

// ### min
Value CL_min(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_min, numargs, 1, MANY);
    case 1:
      if (realp(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_real);
    case 2:
      if (lt(args[0], args[1]))
        return args[0];
      else
        return args[1];
    default:
      {
        if (!realp(args[0]))
          return signal_type_error(args[0], S_real);
        Value result = args[0];
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (lt(args[i], result))
              result = args[i];
          }
        return result;
      }
    }
}

// ### rational number => rational
Value CL_rational(Value arg)
{
  if (single_float_p(arg))
    return the_single_float(arg)->rational();
  if (double_float_p(arg))
    return the_double_float(arg)->rational();
  if (rationalp(arg))
    return arg;
  return signal_type_error(arg, S_real);
}

// ### rationalize number => rational
Value CL_rationalize(Value arg)
{
  // "What distinguishes the two functions is that RATIONAL typically has a
  // simple, inexpensive implementation, whereas RATIONALIZE goes to more
  // trouble to produce a result that is more pleasant to view and simpler to
  // compute with for some purposes." CLtL2, p.350

  // Examples from CLHS:
  //   (rational .1) =>  13421773/134217728 ;implementation-dependent
  //   (rationalize .1) =>  1/10

  // REVIEW
  if (single_float_p(arg))
    return the_single_float(arg)->rational();
  if (double_float_p(arg))
    return the_double_float(arg)->rational();
  if (rationalp(arg))
    return arg;
  return signal_type_error(arg, S_real);
}

// ### ash integer count => shifted-integer
Value CL_ash(Value arg1, Value arg2)
{
  if (arg2 == 0)
    {
      if (integerp(arg1))
        return arg1;
      else
        return signal_type_error(arg1, S_integer);
    }
  if (fixnump(arg1) && fixnump(arg2))
    {
      const long shift = xlong(arg2);
#ifdef __x86_64__
      if (shift < LOWTAG_BITS && shift > -61)
        {
          const long n = xlong(arg1);
          long result;
          if (shift > 0)
            result = n << shift;
          else
            result = n >> -shift;
          return make_integer(result);
        }
      if (shift <= -61)
        return xlong(arg1) >= 0 ? FIXNUM_ZERO : FIXNUM_MINUS_ONE;
#else
      if (shift < LOWTAG_BITS && shift > -29)
        {
          const long n = xlong(arg1);
          long result;
          if (shift > 0)
            result = n << shift;
          else
            result = n >> -shift;
          return make_integer(result);
        }
      if (shift <= -29)
        return xlong(arg1) >= 0 ? FIXNUM_ZERO : FIXNUM_MINUS_ONE;
#endif
    }
  mpz_t z;
  if (fixnump(arg1))
    mpz_init_set_si(z, xlong(arg1));
  else if (bignump(arg1))
    mpz_init_set(z, the_bignum(arg1)->_z);
  else
    return signal_type_error(arg1, S_integer);
  if (fixnump(arg2))
    {
      long shift = xlong(arg2);
      if (shift >= 0)
        mpz_mul_2exp(z, z, shift);
      else
        mpz_div_2exp(z, z, -shift);
      Value value = normalize(z);
      MPZ_CLEAR(z);
      return value;
    }
  else if (bignump(arg2))
    {
      Bignum * b = the_bignum(arg2);
      if (b->plusp())
        {
          MPZ_CLEAR(z);
          return signal_lisp_error("Can't represent result of left shift.");
        }
      else
        {
          Value value = mpz_sgn(z) >= 0 ? FIXNUM_ZERO : FIXNUM_MINUS_ONE;
          MPZ_CLEAR(z);
          return value;
        }
    }
  else
    {
      MPZ_CLEAR(z);
      return signal_type_error(arg2, S_integer);
    }
}

// ### float-digits float => float-digits
Value CL_float_digits(Value arg)
{
  if (single_float_p(arg))
    return make_fixnum(24);
  if (double_float_p(arg))
    return make_fixnum(53);
  return signal_type_error(arg, S_float);
}

// ### float-radix float => float-radix
Value CL_float_radix(Value arg)
{
  if (single_float_p(arg) || double_float_p(arg))
    return FIXNUM_TWO;
  return signal_type_error(arg, S_float);
}

// ### abs number => absolute-value
Value CL_abs(Value arg)
{
  if (fixnump(arg))
    {
      long n = xlong(arg);
      return n < 0 ? make_integer(-n) : arg;
    }
  if (bignump(arg))
    {
      Bignum * b = the_bignum(arg);
      return b->minusp() ? b->negate() : arg;
    }
  if (ratiop(arg))
    {
      Ratio * r = the_ratio(arg);
      return mpq_sgn(r->_q) < 0 ? r->negate() : arg;
    }
  if (single_float_p(arg))
    {
      float f = the_single_float(arg)->_f;
      return f < 0 ? make_single_float(-f) : arg;
    }
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      return d < 0 ? make_double_float(-d) : arg;
    }
  if (complexp(arg))
    {
      Complex * c = the_complex(arg);
      Value realpart = c->realpart();
      Value imagpart = c->imagpart();
      if (double_float_p(realpart))
        {
          double real = the_double_float(coerce_to_double_float(realpart))->_d;
          double imag = the_double_float(coerce_to_double_float(imagpart))->_d;
          return make_double_float(hypot(real, imag));
        }
      else
        {
          float real = the_single_float(coerce_to_single_float(realpart))->_f;
          float imag = the_single_float(coerce_to_single_float(imagpart))->_f;
          return make_single_float(hypotf(real, imag));
        }
    }
  return signal_type_error(arg, S_real);
}

bool plusp(Value arg)
{
  long typecode = typecode_of(arg);
  switch (typecode)
    {
    case TYPECODE_FIXNUM:
      return xlong(arg) > 0;
    case TYPECODE_BIGNUM:
      return the_bignum(arg)->plusp();
    case TYPECODE_RATIO:
      return mpq_sgn(the_ratio(arg)->_q) > 0;
    case TYPECODE_SINGLE_FLOAT:
      return the_single_float(arg)->_f > 0;
    case TYPECODE_DOUBLE_FLOAT:
      return the_double_float(arg)->_d > 0;
    }
  return signal_type_error(arg, S_real);
}

// ### plusp
Value CL_plusp(Value arg)
{
  return plusp(arg) ? T : NIL;
}

bool minusp(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
      return xlong(arg) < 0;
    case TYPECODE_BIGNUM:
      return the_bignum(arg)->minusp();
    case TYPECODE_RATIO:
      return mpq_sgn(the_ratio(arg)->_q) < 0;
    case TYPECODE_SINGLE_FLOAT:
      return the_single_float(arg)->_f < 0;
    case TYPECODE_DOUBLE_FLOAT:
      return the_double_float(arg)->_d < 0;
    default:
      signal_type_error(arg, S_real);
      // not reached
      return false;
    }
}

// ### minusp
Value CL_minusp(Value arg)
{
  return minusp(arg) ? T : NIL;
}

// ### signum
Value CL_signum(Value arg)
{
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n == 0)
        return arg;
      else if (n < 0)
        return FIXNUM_MINUS_ONE;
      else
        return FIXNUM_ONE;
    }
  if (bignump(arg))
    return make_fixnum(mpz_sgn(the_bignum(arg)->_z));
  if (ratiop(arg))
    return make_fixnum(mpq_sgn(the_ratio(arg)->_q));
  if (single_float_p(arg))
    {
      float f = the_single_float(arg)->_f;
      if (f == 0)
        return arg;
      else if (f < 0)
        return make_single_float(-1);
      else
        return make_single_float(1);
    }
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d == 0)
        return arg;
      else if (d < 0)
        return make_double_float(-1);
      else
        return make_double_float(1);
    }
  if (complexp(arg))
    {
      if (zerop(arg))
        return arg;
      else
        return SYS_two_arg_slash(arg, CL_abs(arg));
    }
  return NIL;
}

// ### floatp
Value CL_floatp(Value arg)
{
  return (single_float_p(arg) || double_float_p(arg)) ? T : NIL;
}

// ### integer-decode-float float => significand, exponent, integer-sign
Value CL_integer_decode_float(Value arg)
{
  Thread * thread = current_thread();
  Value significand;
  int exponent;
  long sign = 1;
  if (single_float_p(arg))
    {
      float f = the_single_float(arg)->_f;
      if (f == 0)
        {
          significand = FIXNUM_ZERO;
          exponent = -150;
        }
      else
        {
          if (f < 0)
            {
              f = -f;
              sign = -1;
            }
          float fraction = frexpf(f, &exponent);
          exponent -= FLT_MANT_DIG;
          mpq_t q;
          mpq_init(q);
          mpq_set_d(q, ldexp(fraction, FLT_MANT_DIG));
          significand = normalize(q);
          MPQ_CLEAR(q);
        }
    }
  else if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d == 0)
        {
          significand = FIXNUM_ZERO;
          exponent = -1075;
        }
      else
        {
          if (d < 0)
            {
              d = -d;
              sign = -1;
            }
          double fraction = frexp(d, &exponent);
          exponent -= DBL_MANT_DIG;
          mpq_t q;
          mpq_init(q);
          mpq_set_d(q, ldexp(fraction, DBL_MANT_DIG));
          significand = normalize(q);
          MPQ_CLEAR(q);
      }
    }
  else
    return signal_type_error(arg, S_float);

  return thread->set_values(significand,
                            make_fixnum(exponent),
                            make_fixnum(sign));
}

// ### decode-float float => significand, exponent, integer-sign
Value CL_decode_float(Value arg)
{
  Thread * thread = current_thread();
  int exponent;
  long sign = 1;
  if (single_float_p(arg))
    {
      float f = the_single_float(arg)->_f;
      if (f < 0)
        {
          f = -f;
          sign = -1;
        }
      float fraction = frexpf(f, &exponent);
      return thread->set_values(make_single_float(fraction),
                                make_fixnum(exponent),
                                make_single_float(sign));
    }
  else if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d < 0)
        {
          d = -d;
          sign = -1;
        }
      double fraction = frexp(d, &exponent);
      return thread->set_values(make_double_float(fraction),
                                make_fixnum(exponent),
                                make_double_float(sign));
    }
  else
    return signal_type_error(arg, S_float);
}

// ### float-sign
Value CL_float_sign(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      {
        // "If an implementation has distinct representations for negative zero
        // and positive zero, then (FLOAT-SIGN -0.0) => -1.0."
        Value arg = args[0];
        if (single_float_p(arg))
          return make_single_float(the_single_float(arg)->sign());
        if (double_float_p(arg))
          return make_double_float(the_double_float(arg)->sign());
        return signal_type_error(arg, S_float);
      }
    case 2:
      {
        Value arg1 = args[0];
        Value arg2 = args[1];
        bool minusp = false;
        if (single_float_p(arg1))
          {
            if (the_single_float(arg1)->_f < 0)
              minusp = true;
          }
        else if (double_float_p(arg1))
          {
            if (the_double_float(arg1)->_d < 0)
              minusp = true;
          }
        else
          return signal_type_error(arg1, S_float);
        if (minusp)
          {
            if (single_float_p(arg2))
              {
                float f = the_single_float(arg2)->_f;
                if (f < 0)
                  return arg2;
                else
                  return make_single_float(-f);
              }
            if (double_float_p(arg2))
              {
                double d = the_double_float(arg2)->_d;
                if (d < 0)
                  return arg2;
                else
                  return make_double_float(-d);
              }
            signal_type_error(arg2, S_float);
          }
        else
          {
            if (single_float_p(arg2))
              {
                float f = the_single_float(arg2)->_f;
                if (f < 0)
                  return make_single_float(-f);
                else
                  return arg2;
              }
            if (double_float_p(arg2))
              {
                double d = the_double_float(arg2)->_d;
                if (d < 0)
                  return make_double_float(-d);
                else
                  return arg2;
              }
            signal_type_error(arg2, S_float);
          }
      }
    default:
      return wrong_number_of_arguments(S_float_sign, numargs, 1, 2);
    }
}

Value coerce_to_single_float(Value arg)
{
  if (fixnump(arg))
    return make_value(new SingleFloat(xlong(arg)));
  if (bignump(arg))
    {
      mpf_t f;
      mpf_init(f);
      mpf_set_z(f, the_bignum(arg)->_z);
      Value value = make_single_float((float)mpf_get_d(f));
      mpf_clear(f);
      return value;
    }
  if (ratiop(arg))
    {
      mpf_t f;
      mpf_init(f);
      mpf_set_q(f, the_ratio(arg)->_q);
      Value value = make_single_float((float)mpf_get_d(f));
      mpf_clear(f);
      return value;
    }
  if (single_float_p(arg))
    return arg;
  if (double_float_p(arg))
    return make_single_float((float)the_double_float(arg)->_d);
  return signal_type_error(arg, S_real);
}

Value coerce_to_double_float(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
      return make_double_float(xlong(arg));
    case TYPECODE_BIGNUM:
      {
        mpf_t f;
        mpf_init(f);
        mpf_set_z(f, the_bignum(arg)->_z);
        Value value = make_double_float(mpf_get_d(f));
        mpf_clear(f);
        return value;
      }
    case TYPECODE_RATIO:
      {
        mpf_t f;
        mpf_init(f);
        mpf_set_q(f, the_ratio(arg)->_q);
        Value value = make_double_float(mpf_get_d(f));
        mpf_clear(f);
        return value;
      }
    case TYPECODE_SINGLE_FLOAT:
      return make_double_float(the_single_float(arg)->_f);
    case TYPECODE_DOUBLE_FLOAT:
      return arg;
    default:
      return signal_type_error(arg, S_real);
    }
}

// ### float number &optional prototype => float
Value CL_float(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      if (single_float_p(args[0]) || double_float_p(args[0]))
        return args[0];
      else
        return coerce_to_single_float(args[0]);
    case 2:
      if (single_float_p(args[1]))
        return coerce_to_single_float(args[0]);
      if (double_float_p(args[1]))
        return coerce_to_double_float(args[0]);
      return signal_type_error(args[1], S_float);
    default:
      return wrong_number_of_arguments(S_float, numargs, 1, 2);
    }
}

// ### isqrt natural => natural-root
Value CL_isqrt(Value arg)
{
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n > 0)
        {
          mpz_t z;
          mpz_init_set_si(z, n);
          mpz_sqrt(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      else if (n == 0)
        return FIXNUM_ZERO;
      // else fall through...
    }
  else if (bignump(arg))
    {
      mpz_t z;
      mpz_init_set(z, the_bignum(arg)->_z);
      if (mpz_sgn(z) > 0)
        {
          mpz_sqrt(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      else
        MPZ_CLEAR(z);
      // and fall through...
    }
  return signal_type_error(arg, S_unsigned_byte);
}

// ### truncate-1
Value SYS_truncate_1(Value number)
{
  Thread * const thread = current_thread();
  if (fixnump(number) || bignump(number))
    return thread->set_values(number, FIXNUM_ZERO);
  if (ratiop(number))
    {
      mpz_t quotient, remainder;
      mpz_init(quotient);
      mpz_init(remainder);
      mpz_tdiv_qr(quotient, remainder,
                  mpq_numref(the_ratio(number)->_q),
                  mpq_denref(the_ratio(number)->_q));
      Value value1 = normalize(quotient);
      MPZ_CLEAR(quotient);
      Value value2 = normalize(remainder);
      MPZ_CLEAR(remainder);
      value2 = SYS_two_arg_slash(value2, the_ratio(number)->denominator());
      return thread->set_values(value1, value2);
    }
  if (single_float_p(number))
    {
      Value quotient = SYS_truncate_1(the_single_float(number)->rational());
      Value remainder = coerce_to_single_float(thread->nth_value(1));
      return thread->set_values(quotient, remainder);
    }
  if (double_float_p(number))
    {
      Value quotient = SYS_truncate_1(the_double_float(number)->rational());
      Value remainder = coerce_to_double_float(thread->nth_value(1));
      return thread->set_values(quotient, remainder);
    }
  return signal_type_error(number, S_real);
}

// ### truncate-2
Value SYS_truncate_2(Value number, Value divisor)
{
  Thread * const thread = current_thread();
  if (fixnump(number))
    {
      if (fixnump(divisor))
        {
          if (divisor == 0)
            goto DIVIDE_BY_ZERO;
          long n = xlong(number);
          long d = xlong(divisor);
          long n_abs = abs(n);
          long d_abs = abs(d);
          long q = n_abs / d_abs;
          long r = n_abs % d_abs;
          if (n < 0)
            r = -r;
          if (!same_sign_p(n, d))
            q = -q;
          Value quotient = make_integer(q);
          Value remainder = make_integer(r);
          return thread->set_values(quotient, remainder);
        }
      if (ratiop(divisor))
        {
          Ratio * r2 = the_ratio(divisor);
          Value quotient = SYS_truncate_2(SYS_two_arg_star(number, r2->denominator()), r2->numerator());
          current_thread()->set_values_length(-1);
          Value remainder = SYS_two_arg_minus(number, SYS_two_arg_star(quotient, divisor));
          return thread->set_values(quotient, remainder);
        }
    }
  if (bignump(number))
    {
      if (fixnump(divisor))
        {
          if (divisor == 0)
            goto DIVIDE_BY_ZERO;
          mpz_t z2;
          mpz_init_set_si(z2, xlong(divisor));
          mpz_t quotient, remainder;
          mpz_init(quotient);
          mpz_init(remainder);
          mpz_tdiv_qr(quotient, remainder, the_bignum(number)->_z, z2);
          MPZ_CLEAR(z2);
          Value value1 = normalize(quotient);
          MPZ_CLEAR(quotient);
          Value value2 = normalize(remainder);
          MPZ_CLEAR(remainder);
          return thread->set_values(value1, value2);
        }
    }
  if (realp(number))
    {
      if (realp(divisor))
        {
          Value quotient = SYS_two_arg_slash(number, divisor);
          Value value1 = SYS_truncate_1(quotient);
          Value value2 = thread->nth_value(1);
          if (!zerop(value2))
            value2 = SYS_two_arg_star(value2, divisor);
          return thread->set_values(value1, value2);
        }
      return signal_type_error(divisor, S_real);
    }
  return signal_type_error(number, S_real);
DIVIDE_BY_ZERO:
  return signal_lisp_error(new DivisionByZero(S_truncate_2, list2(number, divisor)));
}

// ### truncate
Value CL_truncate(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return SYS_truncate_1(args[0]);
    case 2:
      return SYS_truncate_2(args[0], args[1]);
    default:
      return wrong_number_of_arguments(S_truncate, numargs, 1, 2);
    }
}

// ### rem number divisor => remainder
Value CL_rem(Value number, Value divisor)
{
  // "REM performs the operation TRUNCATE on number and divisor and returns the
  // remainder of the TRUNCATE operation."
  if (fixnump(number) && fixnump(divisor))
    {
      if (divisor == 0)
        return signal_lisp_error(new DivisionByZero(S_rem, list2(number, divisor)));
      long n = xlong(number);
      long d = xlong(divisor);
      long n_abs = abs(n);
      long d_abs = abs(d);
      long r = n_abs % d_abs;
      if (n < 0)
        r = -r;
      return make_fixnum(r);
    }
  Thread * thread = current_thread();
  SYS_truncate_2(number, divisor);
  Value value = thread->nth_value(1);
  thread->clear_values();
  return value;
}

// ### mod number divisor => modulus
Value CL_mod(Value number, Value divisor)
{
  // "MOD performs the operation FLOOR on number and divisor and returns the
  // remainder of the FLOOR operation."
  if (fixnump(number) && fixnump(divisor))
    {
      if (divisor == 0)
        return signal_lisp_error(new DivisionByZero(S_mod, list2(number, divisor)));
      long n = xlong(number);
      long d = xlong(divisor);
      long n_abs = abs(n);
      long d_abs = abs(d);
      long r = n_abs % d_abs;
      if (!same_sign_p(n, d))
        if (r != 0)
          r -= d_abs;
      if (n < 0)
        r = -r;
      return make_fixnum(r);
    }
  Thread * thread = current_thread();
  SYS_floor_2(number, divisor);
  Value rem = thread->nth_value(1);
  thread->clear_values();
  return rem;
}

// ### floor-1
Value SYS_floor_1(Value arg)
{
  Thread * const thread = current_thread();
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
      return thread->set_values(arg, FIXNUM_ZERO);
    case TYPECODE_RATIO:
      {
        mpz_t quotient, remainder;
        mpz_init(quotient);
        mpz_init(remainder);
        mpz_fdiv_qr(quotient, remainder,
                    mpq_numref(the_ratio(arg)->_q),
                    mpq_denref(the_ratio(arg)->_q));
        Value value1 = normalize(quotient);
        MPZ_CLEAR(quotient);
        Value value2 = normalize(remainder);
        MPZ_CLEAR(remainder);
        value2 = SYS_two_arg_slash(value2, the_ratio(arg)->denominator());
        return thread->set_values(value1, value2);
      }
    case TYPECODE_SINGLE_FLOAT:
      {
        Value quotient = SYS_floor_1(the_single_float(arg)->rational());
        Value remainder = coerce_to_single_float(thread->nth_value(1));
        return thread->set_values(quotient, remainder);
      }
    case TYPECODE_DOUBLE_FLOAT:
      {
        Value quotient = SYS_floor_1(the_double_float(arg)->rational());
        Value remainder = coerce_to_double_float(thread->nth_value(1));
        return thread->set_values(quotient, remainder);
      }
    default:
      return signal_type_error(arg, S_real);
    }
}

// ### floor-2
Value SYS_floor_2(Value number, Value divisor)
{
  Thread * const thread = current_thread();
  if (fixnump(number))
    {
      if (fixnump(divisor))
        {
          if (divisor == 0)
            goto DIVIDE_BY_ZERO;
          long x = xlong(number);
          long y = xlong(divisor);
          long x_abs = abs(x);
          long y_abs = abs(y);
          long q = x_abs / y_abs;
          long r = x_abs % y_abs;
          if (!same_sign_p(x, y))
            {
              if (r != 0)
                {
                  ++q;
                  r -= y_abs;
                }
              q = -q;
            }
          if (x < 0)
            r = -r;
          Value quotient = make_integer(q);
          Value remainder = make_integer(r);
          return thread->set_values(quotient, remainder);
        }
      if (ratiop(divisor))
        {
          Ratio * r2 = the_ratio(divisor);
          Value quotient = SYS_floor_2(SYS_two_arg_star(number, r2->denominator()), r2->numerator());
          current_thread()->set_values_length(-1);
          Value remainder = SYS_two_arg_minus(number, SYS_two_arg_star(quotient, divisor));
          return thread->set_values(quotient, remainder);
        }
    }
  if (bignump(number))
    {
      if (fixnump(divisor))
        {
          if (divisor == 0)
            goto DIVIDE_BY_ZERO;
          mpz_t z2;
          mpz_init_set_si(z2, xlong(divisor));
          mpz_t quotient, remainder;
          mpz_init(quotient);
          mpz_init(remainder);
          mpz_fdiv_qr(quotient, remainder, the_bignum(number)->_z, z2);
          MPZ_CLEAR(z2);
          Value value1 = normalize(quotient);
          MPZ_CLEAR(quotient);
          Value value2 = normalize(remainder);
          MPZ_CLEAR(remainder);
          return thread->set_values(value1, value2);
        }
      if (bignump(divisor))
        {
          mpz_t quotient, remainder;
          mpz_init(quotient);
          mpz_init(remainder);
          mpz_fdiv_qr(quotient, remainder, the_bignum(number)->_z, the_bignum(divisor)->_z);
          Value value1 = normalize(quotient);
          MPZ_CLEAR(quotient);
          Value value2 = normalize(remainder);
          MPZ_CLEAR(remainder);
          return thread->set_values(value1, value2);
        }
    }
  if (realp(number))
    {
      if (realp(divisor))
        {
          Value quotient = SYS_two_arg_slash(number, divisor);
          Value value1 = SYS_floor_1(quotient);
          Value value2 = thread->nth_value(1);
          if (!zerop(value2))
            value2 = SYS_two_arg_star(value2, divisor);
          return thread->set_values(value1, value2);
        }
      return signal_type_error(divisor, S_real);
    }
  return signal_type_error(number, S_real);
DIVIDE_BY_ZERO:
  return signal_lisp_error(new DivisionByZero(S_floor_2, list2(number, divisor)));
}

// ### floor
Value CL_floor(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return SYS_floor_1(args[0]);
    case 2:
      return SYS_floor_2(args[0], args[1]);
    default:
      return wrong_number_of_arguments(S_floor, numargs, 1, 2);
    }
}

// ### ceiling-1
Value SYS_ceiling_1(Value number)
{
  Thread * const thread = current_thread();
  if (fixnump(number) || bignump(number))
    return thread->set_values(number, FIXNUM_ZERO);
  if (ratiop(number))
    {
      mpz_t quotient, remainder;
      mpz_init(quotient);
      mpz_init(remainder);
      mpz_cdiv_qr(quotient, remainder,
                  mpq_numref(the_ratio(number)->_q),
                  mpq_denref(the_ratio(number)->_q));
      Value value1 = normalize(quotient);
      MPZ_CLEAR(quotient);
      Value value2 = normalize(remainder);
      MPZ_CLEAR(remainder);
      value2 = SYS_two_arg_slash(value2, the_ratio(number)->denominator());
      return thread->set_values(value1, value2);
    }
  if (single_float_p(number))
    {
      Value quotient = SYS_ceiling_1(the_single_float(number)->rational());
      Value remainder = coerce_to_single_float(thread->nth_value(1));
      return thread->set_values(quotient, remainder);
    }
  if (double_float_p(number))
    {
      Value quotient = SYS_ceiling_1(the_double_float(number)->rational());
      Value remainder = coerce_to_double_float(thread->nth_value(1));
      return thread->set_values(quotient, remainder);
    }
  return signal_type_error(number, S_real);
}

// ### ceiling-2
Value SYS_ceiling_2(Value number, Value divisor)
{
  Thread * const thread = current_thread();
  if (fixnump(number))
    {
      if (fixnump(divisor))
        {
          if (divisor == 0)
            goto DIVIDE_BY_ZERO;
          long n = xlong(number);
          long d = xlong(divisor);
          long n_abs = abs(n);
          long d_abs = abs(d);
          long q = n_abs / d_abs;
          long r = n_abs % d_abs;
          if (same_sign_p(n, d))
            if (r != 0)
              {
                ++q;
                r -= d_abs;
              }
          if (n < 0)
            r = -r;
          if (!same_sign_p(n, d))
            q = -q;
          Value quotient = make_integer(q);
          Value remainder = make_integer(r);
          return thread->set_values(quotient, remainder);
        }
      if (ratiop(divisor))
        {
          Ratio * r2 = the_ratio(divisor);
          Value quotient = SYS_ceiling_2(SYS_two_arg_star(number, r2->denominator()), r2->numerator());
          current_thread()->set_values_length(-1);
          Value remainder = SYS_two_arg_minus(number, SYS_two_arg_star(quotient, divisor));
          return thread->set_values(quotient, remainder);
        }
    }
  if (bignump(number))
    {
      if (fixnump(divisor))
        {
          if (divisor == 0)
            goto DIVIDE_BY_ZERO;
          mpz_t z2;
          mpz_init_set_si(z2, xlong(divisor));
          mpz_t quotient, remainder;
          mpz_init(quotient);
          mpz_init(remainder);
          mpz_cdiv_qr(quotient, remainder, the_bignum(number)->_z, z2);
          MPZ_CLEAR(z2);
          Value value1 = normalize(quotient);
          MPZ_CLEAR(quotient);
          Value value2 = normalize(remainder);
          MPZ_CLEAR(remainder);
          return thread->set_values(value1, value2);
        }
      if (bignump(divisor))
        {
          mpz_t quotient, remainder;
          mpz_init(quotient);
          mpz_init(remainder);
          mpz_cdiv_qr(quotient, remainder, the_bignum(number)->_z, the_bignum(divisor)->_z);
          Value value1 = normalize(quotient);
          MPZ_CLEAR(quotient);
          Value value2 = normalize(remainder);
          MPZ_CLEAR(remainder);
          return thread->set_values(value1, value2);
        }
    }
  if (realp(number))
    {
      if (realp(divisor))
        {
          Value quotient = SYS_two_arg_slash(number, divisor);
          Value value1 = SYS_ceiling_1(quotient);
          Value value2 = thread->nth_value(1);
          if (!zerop(value2))
            value2 = SYS_two_arg_star(value2, divisor);
          return thread->set_values(value1, value2);
        }
      return signal_type_error(divisor, S_real);
    }
  return signal_type_error(number, S_real);
DIVIDE_BY_ZERO:
  return signal_lisp_error(new DivisionByZero(S_ceiling_2, list2(number, divisor)));
}

// ### ceiling
Value CL_ceiling(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return SYS_ceiling_1(args[0]);
    case 2:
      return SYS_ceiling_2(args[0], args[1]);
   default:
     return wrong_number_of_arguments(S_ceiling, numargs, 1, 2);
    }
}

Value log1(Value arg)
{
  if (realp(arg))
    {
      if (CL_minusp(arg) == NIL)
        {
          // result is real
          if (fixnump(arg))
            return make_single_float(logf(xlong(arg)));
          if (bignump(arg) || ratiop(arg))
            {
              arg = coerce_to_double_float(arg);
              return make_single_float(log(the_double_float(arg)->_d));
            }
          if (single_float_p(arg))
            return make_single_float(logf(the_single_float(arg)->_f));
          if (double_float_p(arg))
            return make_double_float(log(the_double_float(arg)->_d));
        }
      else if (single_float_p(arg))
        {
          Value abs = CL_abs(arg);
          Value phase = make_single_float(M_PI);
          return make_complex(log1(abs), phase);
        }
      else if (double_float_p(arg))
        {
          Value abs = CL_abs(arg);
          Value phase = make_double_float(M_PI);
          return make_complex(log1(abs), phase);
        }
      else
        return log1(coerce_to_single_float(arg));
    }
  else if (complexp(arg))
    {
      Complex * c = the_complex(arg);
      Value realpart = c->realpart();
      if (double_float_p(realpart))
        {
          double real = the_double_float(realpart)->_d;
          double imag = the_double_float(coerce_to_double_float(c->imagpart()))->_d;
          double phase = atan2(imag, real);
          double abs = the_double_float(coerce_to_double_float(CL_abs(arg)))->_d;
          return make_complex(make_value(new DoubleFloat(log(abs))),
                              make_value(new DoubleFloat(phase)));
        }
      else
        {
          float real = the_single_float(coerce_to_single_float(realpart))->_f;
          float imag = the_single_float(coerce_to_single_float(c->imagpart()))->_f;
          float phase = atan2f(imag, real);
          float abs = the_single_float(coerce_to_single_float(CL_abs(arg)))->_f;
          return make_complex(make_value(new SingleFloat(log(abs))),
                              make_value(new SingleFloat(phase)));
        }
    }
  return signal_type_error(arg, S_number);
}

// ### log
Value CL_log(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return log1(args[0]);
    case 2:
      return SYS_two_arg_slash(log1(args[0]), log1(args[1]));
    default:
      return wrong_number_of_arguments(S_log, numargs, 1, 2);
    }
}

// ### sin radians => number
Value CL_sin(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
    case TYPECODE_RATIO:
    case TYPECODE_SINGLE_FLOAT:
      return make_single_float(sinf(the_single_float(coerce_to_single_float(arg))->_f));
    case TYPECODE_DOUBLE_FLOAT:
      return make_double_float(sin(the_double_float(arg)->_d));
    case TYPECODE_COMPLEX:
      {
        Value n = SYS_two_arg_star(arg, make_complex(FIXNUM_ZERO, FIXNUM_ONE));
        Value result = CL_exp(n);
        result = SYS_two_arg_minus(result, CL_exp(SYS_two_arg_star(n, FIXNUM_MINUS_ONE)));
        return SYS_two_arg_slash(result, make_complex(FIXNUM_ZERO, FIXNUM_TWO));
      }
    default:
      return signal_type_error(arg, S_number);
    }
}

// ### cos radians => number
Value CL_cos(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
    case TYPECODE_RATIO:
    case TYPECODE_SINGLE_FLOAT:
      return make_single_float(cosf(the_single_float(coerce_to_single_float(arg))->_f));
    case TYPECODE_DOUBLE_FLOAT:
      return make_double_float(cos(the_double_float(arg)->_d));
    case TYPECODE_COMPLEX:
      {
        Value n = SYS_two_arg_star(arg, make_complex(FIXNUM_ZERO, FIXNUM_ONE));
        Value result = CL_exp(n);
        result = SYS_two_arg_plus(result, CL_exp(SYS_two_arg_star(n, FIXNUM_MINUS_ONE)));
        return SYS_two_arg_slash(result, FIXNUM_TWO);
      }
    default:
      return signal_type_error(arg, S_number);
    }
}

// ### tan radians => number
Value CL_tan(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
    case TYPECODE_RATIO:
    case TYPECODE_SINGLE_FLOAT:
      return make_single_float(tanf(the_single_float(coerce_to_single_float(arg))->_f));
    case TYPECODE_DOUBLE_FLOAT:
      return make_double_float(tan(the_double_float(arg)->_d));
    case TYPECODE_COMPLEX:
      return SYS_two_arg_slash(CL_sin(arg), CL_cos(arg));
    default:
      return signal_type_error(arg, S_number);
    }
}

// ### real-asin
Value SYS_real_asin(Value arg)
{
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d >= -1 && d <= 1)
        return make_double_float(asin(d));
      else
        return signal_type_error(arg, list3(S_real, make_single_float(-1.0), make_single_float(1.0)));
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      if (f >= -1 && f <= 1)
        return make_single_float(asinf(f));
      else
        return signal_type_error(arg, list3(S_real, make_single_float(-1.0), make_single_float(1.0)));
    }
}

// ### real-acos
Value SYS_real_acos(Value arg)
{
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d >= -1 && d <= 1)
        return make_double_float(acos(d));
      else
        return signal_type_error(arg, list3(S_real, make_single_float(-1.0), make_single_float(1.0)));
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      if (f >= -1 && f <= 1)
        return make_single_float(acosf(f));
      else
        return signal_type_error(arg, list3(S_real, make_single_float(-1.0), make_single_float(1.0)));
    }
}

// ### real-sinh
Value SYS_real_sinh(Value arg)
{
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      return make_double_float(sinh(d));
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      return make_single_float(sinhf(f));
    }
}

// ### real-cosh
Value SYS_real_cosh(Value arg)
{
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      return make_double_float(cosh(d));
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      return make_single_float(coshf(f));
    }
}

// ### real-tanh
Value SYS_real_tanh(Value arg)
{
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      return make_double_float(tanh(d));
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      return make_single_float(tanhf(f));
    }
}

// ### real-asinh
Value SYS_real_asinh(Value arg)
{
  mpfr_t x;
  if (double_float_p(arg))
    {
      mpfr_init2(x, 53); // 53-bit precision
      mpfr_set_d(x, the_double_float(arg)->_d, GMP_RNDN);
      mpfr_asinh(x, x, GMP_RNDN);
      double d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      return make_double_float(d);
    }
  else
    {
      mpfr_init2(x, 24); // 24-bit precision
      mpfr_set_d(x, the_single_float(coerce_to_single_float(arg))->_f, GMP_RNDN);
      mpfr_asinh(x, x, GMP_RNDN);
      double d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      return make_single_float(d);
    }
}

// ### real-acosh
Value SYS_real_acosh(Value arg)
{
  mpfr_t x;
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d < 1)
        return signal_type_error(arg, list2(S_float, make_single_float(1.0)));
      mpfr_init2(x, 53); // 53-bit precision
      mpfr_set_d(x, the_double_float(arg)->_d, GMP_RNDN);
      mpfr_acosh(x, x, GMP_RNDN);
      d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      return make_double_float(d);
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      if (f < 1)
        return signal_type_error(arg, list2(S_float, make_single_float(1.0)));
      mpfr_init2(x, 24); // 24-bit precision
      mpfr_set_d(x, f, GMP_RNDN);
      mpfr_acosh(x, x, GMP_RNDN);
      double d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      return make_single_float(d);
    }
}

// ### real-atanh
Value SYS_real_atanh(Value arg)
{
  mpfr_t x;
  if (double_float_p(arg))
    {
      double d = the_double_float(arg)->_d;
      if (d > 1 || d < -1)
        return signal_type_error(arg, list3(S_float, make_single_float(-1.0), make_single_float(1.0)));
      mpfr_init2(x, 53); // 53-bit precision
      mpfr_set_d(x, the_double_float(arg)->_d, GMP_RNDN);
      mpfr_atanh(x, x, GMP_RNDN);
      d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      return make_double_float(d);
    }
  else
    {
      float f = the_single_float(coerce_to_single_float(arg))->_f;
      if (f > 1 || f < -1)
        return signal_type_error(arg, list3(S_float, make_single_float(-1.0), make_single_float(1.0)));
      mpfr_init2(x, 24); // 24-bit precision
      mpfr_set_d(x, f, GMP_RNDN);
      mpfr_atanh(x, x, GMP_RNDN);
      double d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      return make_single_float(d);
    }
}

// ### atan-1
Value SYS_atan_1(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
    case TYPECODE_RATIO:
    case TYPECODE_SINGLE_FLOAT:
      return make_single_float(atanf(the_single_float(coerce_to_single_float(arg))->_f));
    case TYPECODE_DOUBLE_FLOAT:
      return make_double_float(atan(the_double_float(arg)->_d));
    case TYPECODE_COMPLEX:
      {
        Complex * c = the_complex(arg);
        Value imagpart = c->imagpart();
        if (zerop(imagpart))
          return make_complex(SYS_atan_1(c->realpart()), imagpart);
        Value result = SYS_two_arg_star(arg, arg);
        result = SYS_two_arg_plus(result, FIXNUM_ONE);
        result = SYS_two_arg_slash(FIXNUM_ONE, result);
        result = CL_sqrt(result);
        Value n = make_complex(FIXNUM_ZERO, FIXNUM_ONE);
        n = SYS_two_arg_star(n, arg);
        n = SYS_two_arg_plus(n, FIXNUM_ONE);
        result = SYS_two_arg_star(n, result);
        result = log1(result);
        result = SYS_two_arg_star(result, make_complex(FIXNUM_ZERO, FIXNUM_MINUS_ONE));
        return result;
      }
    default:
      return signal_type_error(arg, S_number);
    }
}

// ### atan
Value CL_atan(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return SYS_atan_1(args[0]);
    case 2:
      {
        Value arg1 = args[0];
        Value arg2 = args[1];
        if (double_float_p(arg1) || double_float_p(arg2))
          {
            double d1 = the_double_float(coerce_to_double_float(arg1))->_d;
            double d2 = the_double_float(coerce_to_double_float(arg2))->_d;
            return make_double_float(atan2(d1, d2));
          }
        else
          {
            float f1 = the_single_float(coerce_to_single_float(arg1))->_f;
            float f2 = the_single_float(coerce_to_single_float(arg2))->_f;
            return make_single_float(atan2f(f1, f2));
          }
      }
    default:
      return wrong_number_of_arguments(S_atan, numargs, 1, 2);
    }
}

// ### cis radians => number
Value CL_cis(Value arg)
{
  if (realp(arg))
    return make_complex(CL_cos(arg), CL_sin(arg));
  return signal_type_error(arg, S_real);
}

// ### exp
Value CL_exp(Value arg)
{
  if (single_float_p(arg))
    {
      mpfr_t x;
      mpfr_init2(x, 24); // 24-bit precision
      mpfr_set_d(x, the_single_float(arg)->_f, GMP_RNDN);
      mpfr_exp(x, x, GMP_RNDN);
      double d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      if (d < FLT_MIN)
        return signal_lisp_error(new FloatingPointUnderflow());
      return make_single_float(d);
    }
  if (double_float_p(arg))
    {
      mpfr_t x;
      mpfr_init2(x, 53); // 53-bit precision
      mpfr_set_d(x, the_double_float(arg)->_d, GMP_RNDN);
      mpfr_exp(x, x, GMP_RNDN);
      double d = mpfr_get_d(x, GMP_RNDN);
      mpfr_clear(x);
      if (d == 0)
        return signal_lisp_error(new FloatingPointUnderflow());
      return make_double_float(d);
    }
  if (realp(arg))
    return CL_exp(coerce_to_single_float(arg));
  if (complexp(arg))
    {
      Complex * c = the_complex(arg);
      return SYS_two_arg_star(CL_exp(c->realpart()), CL_cis(c->imagpart()));
    }
  return signal_type_error(arg, S_number);
}

// ### two-arg-logand
Value SYS_two_arg_logand(Value v1, Value v2)
{
  if (fixnump(v1))
    {
      if (fixnump(v2))
        return (v1 & v2);
      if (bignump(v2))
        {
          long n1 = xlong(v1);
          if (n1 == 0)
            return FIXNUM_ZERO;
          if (n1 > 0)
            return make_fixnum(n1 & mpz_get_si(the_bignum(v2)->_z));
          mpz_t result;
          mpz_init_set_si(result, n1);
          mpz_and(result, result, the_bignum(v2)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      return signal_type_error(v2, S_integer);
    }
  if (bignump(v1))
    {
      if (fixnump(v2))
        {
          long n2 = xlong(v2);
          if (n2 == 0)
            return FIXNUM_ZERO;
          if (n2 > 0)
            return make_fixnum(mpz_get_si(the_bignum(v1)->_z) & n2);
          mpz_t result;
          mpz_init_set_si(result, n2);
          mpz_and(result, result, the_bignum(v1)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      if (bignump(v2))
        {
          mpz_t result;
          mpz_init_set(result, the_bignum(v1)->_z);
          mpz_and(result, result, the_bignum(v2)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      return signal_type_error(v2, S_integer);
    }
  return signal_type_error(v1, S_integer);
}

// ### logand
Value CL_logand(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return FIXNUM_MINUS_ONE;
    case 1:
      if (fixnump(args[0]) || bignump(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_integer);
    case 2:
      return SYS_two_arg_logand(args[0], args[1]);
    default:
      {
        Value result = FIXNUM_MINUS_ONE;
        for (unsigned int i = 0; i < numargs; i++)
          result = SYS_two_arg_logand(result, args[i]);
        return result;
      }
    }
}

// ### logtest integer-1 integer-2 => generalized-boolean
Value CL_logtest(Value v1, Value v2)
{
  // (logtest x y) == (not (zerop (logand x y)))
  return zerop(SYS_two_arg_logand(v1, v2)) ? NIL : T;
}

// ### logandc1 integer-1 integer-2 => result-integer
// and complement of integer-1 with integer-2
Value CL_logandc1(Value v1, Value v2)
{
  long typecode1 = typecode_of(v1);
  long typecode2 = typecode_of(v2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              long n1 = xlong(v1);
              long n2 = xlong(v2);
              return make_fixnum(~n1 & n2);
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t result;
              mpz_init_set_si(result, ~xlong(v1));
              mpz_and(result, result, the_bignum(v2)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    case TYPECODE_BIGNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v1)->_z);
              mpz_t result;
              mpz_init_set_si(result, xlong(v2));
              mpz_and(result, z, result);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v1)->_z);
              mpz_t result;
              mpz_init(result);
              mpz_and(result, z, the_bignum(v2)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    default:
      return signal_type_error(v1, S_integer);
    }
}

// ### logandc2 integer-1 integer-2 => result-integer
// and integer-1 with complement of integer-2
Value CL_logandc2(Value v1, Value v2)
{
  long typecode1 = typecode_of(v1);
  long typecode2 = typecode_of(v2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              long n1 = xlong(v1);
              long n2 = xlong(v2);
              return make_fixnum(n1 & ~n2);
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v2)->_z);
              mpz_t result;
              mpz_init_set_si(result, xlong(v1));
              mpz_and(result, z, result);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    case TYPECODE_BIGNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpz_t result;
              mpz_init_set_si(result, ~xlong(v2));
              mpz_and(result, result, the_bignum(v1)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v2)->_z);
              mpz_t result;
              mpz_init(result);
              mpz_and(result, z, the_bignum(v1)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    default:
      return signal_type_error(v1, S_integer);
    }
}

// ### logorc1 integer-1 integer-2 => result-integer
// or complement of integer-1 with integer-2
Value CL_logorc1(Value v1, Value v2)
{
  long typecode1 = typecode_of(v1);
  long typecode2 = typecode_of(v2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              long n1 = xlong(v1);
              long n2 = xlong(v2);
              return make_fixnum(~n1 | n2);
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t result;
              mpz_init_set_si(result, ~xlong(v1));
              mpz_ior(result, result, the_bignum(v2)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    case TYPECODE_BIGNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v1)->_z);
              mpz_t result;
              mpz_init_set_si(result, xlong(v2));
              mpz_ior(result, z, result);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v1)->_z);
              mpz_t result;
              mpz_init(result);
              mpz_ior(result, z, the_bignum(v2)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    default:
      return signal_type_error(v1, S_integer);
    }
}

// ### logorc2 integer-1 integer-2 => result-integer
// or integer-1 with complement of integer-2
Value CL_logorc2(Value v1, Value v2)
{
  long typecode1 = typecode_of(v1);
  long typecode2 = typecode_of(v2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              long n1 = xlong(v1);
              long n2 = xlong(v2);
              return make_fixnum(n1 | ~n2);
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v2)->_z);
              mpz_t result;
              mpz_init_set_si(result, xlong(v1));
              mpz_ior(result, z, result);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    case TYPECODE_BIGNUM:
      {
        switch (typecode2)
          {
          case TYPECODE_FIXNUM:
            {
              mpz_t result;
              mpz_init_set_si(result, ~xlong(v2));
              mpz_ior(result, result, the_bignum(v1)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          case TYPECODE_BIGNUM:
            {
              mpz_t z;
              mpz_init(z);
              mpz_com(z, the_bignum(v2)->_z);
              mpz_t result;
              mpz_init(result);
              mpz_ior(result, z, the_bignum(v1)->_z);
              Value value = normalize(result);
              MPZ_CLEAR(result);
              return value;
            }
          default:
            return signal_type_error(v2, S_integer);
          }
      }
    default:
      return signal_type_error(v1, S_integer);
    }
}

// ### two-arg-logior
Value SYS_two_arg_logior(Value v1, Value v2)
{
  if (fixnump(v1))
    {
      if (fixnump(v2))
        return (v1 | v2);
      if (bignump(v2))
        {
          long n1 = xlong(v1);
          if (n1 == 0)
            return v2;
          mpz_t result;
          mpz_init_set_si(result, n1);
          mpz_ior(result, result, the_bignum(v2)->_z);
          Value value;
          if (n1 >= 0)
            value = make_value(new Bignum(result));
          else
            value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      return signal_type_error(v2, S_integer);
    }
  if (bignump(v1))
    {
      if (fixnump(v2))
        {
          long n2 = xlong(v2);
          if (n2 == 0)
            return v1;
          mpz_t result;
          mpz_init_set_si(result, n2);
          mpz_ior(result, result, the_bignum(v1)->_z);
          Value value;
          if (n2 >= 0)
            value = make_value(new Bignum(result));
          else
            value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      if (bignump(v2))
        {
          mpz_t result;
          mpz_init_set(result, the_bignum(v1)->_z);
          mpz_ior(result, result, the_bignum(v2)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      return signal_type_error(v2, S_integer);
    }
  return signal_type_error(v1, S_integer);
}

// ### logior
Value CL_logior(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return FIXNUM_ZERO;
    case 1:
      if (fixnump(args[0]) || bignump(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_integer);
    case 2:
      return SYS_two_arg_logior(args[0], args[1]);
    default:
      {
        Value result = FIXNUM_ZERO;
        for (unsigned int i = 0; i < numargs; i++)
          result = SYS_two_arg_logior(result, args[i]);
        return result;
      }
    }
}

// ### two-arg-logxor
Value SYS_two_arg_logxor(Value v1, Value v2)
{
  if (fixnump(v1))
    {
      if (fixnump(v2))
        {
          long n1 = xlong(v1);
          long n2 = xlong(v2);
          return make_fixnum(n1 ^ n2);
        }
      else if (bignump(v2))
        {
          mpz_t result;
          mpz_init_set_si(result, xlong(v1));
          mpz_xor(result, result, the_bignum(v2)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      else
        return signal_type_error(v2, S_integer);
    }
  else if (bignump(v1))
    {
      if (fixnump(v2))
        {
          mpz_t result;
          mpz_init_set_si(result, xlong(v2));
          mpz_xor(result, result, the_bignum(v1)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      else if (bignump(v2))
        {
          mpz_t result;
          mpz_init_set(result, the_bignum(v1)->_z);
          mpz_xor(result, result, the_bignum(v2)->_z);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      else
        return signal_type_error(v2, S_integer);
    }
  else
    return signal_type_error(v1, S_integer);
}

// ### logxor
Value CL_logxor(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return FIXNUM_ZERO;
    case 1:
      if (integerp(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_integer);
    case 2:
      return SYS_two_arg_logxor(args[0], args[1]);
    default:
      {
        Value result = FIXNUM_ZERO;
        for (unsigned int i = 0; i < numargs; i++)
          result = SYS_two_arg_logxor(result, args[i]);
        return result;
      }
    }
}

static Value logeqv(Value v1, Value v2)
{
  if (fixnump(v1))
    {
      if (fixnump(v2))
        {
          long n1 = xlong(v1);
          long n2 = xlong(v2);
          return make_integer(~(n1 ^ n2));
        }
      else if (bignump(v2))
        {
          mpz_t result;
          mpz_init_set_si(result, xlong(v1));
          mpz_xor(result, result, the_bignum(v2)->_z);
          mpz_com(result, result);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      else
        return signal_type_error(v2, S_integer);
    }
  else if (bignump(v1))
    {
      if (fixnump(v2))
        {
          mpz_t result;
          mpz_init_set_si(result, xlong(v2));
          mpz_xor(result, result, the_bignum(v1)->_z);
          mpz_com(result, result);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      else if (bignump(v2))
        {
          mpz_t result;
          mpz_init_set(result, the_bignum(v1)->_z);
          mpz_xor(result, result, the_bignum(v2)->_z);
          mpz_com(result, result);
          Value value = normalize(result);
          MPZ_CLEAR(result);
          return value;
        }
      else
        return signal_type_error(v2, S_integer);
    }
  else
    return signal_type_error(v1, S_integer);
}

// ### logeqv
Value CL_logeqv(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return FIXNUM_MINUS_ONE;
    case 1:
      if (integerp(args[0]))
        return args[0];
      else
        return signal_type_error(args[0], S_integer);
    case 2:
      return logeqv(args[0], args[1]);
    default:
      {
        Value result = logeqv(args[0], args[1]);
        for (unsigned int i = 2; i < numargs; i++)
          result = logeqv(result, args[i]);
        return result;
      }
    }
}

// ### integer-length
Value CL_integer_length(Value arg)
{
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n < 0)
        n = ~n;
      long count = 0;
      while (n > 0)
        {
          n = n >> 1;
          ++count;
        }
      return make_fixnum(count);
    }
  if (bignump(arg))
    {
      mpz_t z;
      mpz_init_set(z, the_bignum(arg)->_z);
      if (mpz_sgn(z) < 0)
        mpz_com(z, z);
      Value value = make_integer(mpz_sizeinbase(z, 2));
      MPZ_CLEAR(z);
      return value;
    }
  return signal_type_error(arg, S_integer);
}

// ### expt base-number power-number => result
Value CL_expt(Value base, Value power)
{
  if (zerop(power))
    {
      if (fixnump(power))
        {
          if (single_float_p(base))
            return make_single_float(1);
          if (double_float_p(base))
            return make_double_float(1);
          if (complexp(base))
            {
              Complex * c = the_complex(base);
              if (single_float_p(c->realpart()))
                return make_complex(make_single_float(1), make_single_float(0));
              if (double_float_p(c->realpart()))
                return make_complex(make_double_float(1), make_double_float(0));
            }
          return FIXNUM_ONE;
        }
      else if (double_float_p(power) || (double_float_p(base)))
        return make_double_float(1);
      else
        return make_single_float(1);
    }
  if (power == FIXNUM_ONE)
    return base;
  if (power == FIXNUM_MINUS_ONE)
    return SYS_two_arg_slash(FIXNUM_ONE, base);
  if (fixnump(power))
    {
      long exp = xlong(power);
      if (fixnump(base))
        {
          long n = xlong(base);
          if (exp == 0)
            return FIXNUM_ONE;
          if (n == 0)
            return FIXNUM_ZERO;
          if (exp > 0)
            {
              if (n > 0)
                {
                  mpz_t z;
                  mpz_init(z);
                  mpz_ui_pow_ui(z, n, exp);
                  Value value = normalize(z);
                  MPZ_CLEAR(z);
                  return value;
                }
              if (n < 0)
                {
                  mpz_t z;
                  mpz_init(z);
                  mpz_ui_pow_ui(z, -n, exp);
                  if (exp % 2)
                    mpz_neg(z, z);
                  Value value = normalize(z);
                  MPZ_CLEAR(z);
                  return value;
                }
            }
          else
            {
              assert(exp < 0);
              if (n > 0)
                {
                  mpz_t z;
                  mpz_init(z);
                  mpz_ui_pow_ui(z, n, -exp);
                  mpq_t q;
                  mpq_init(q);
                  mpq_set_z(q, z);
                  MPZ_CLEAR(z);
                  mpq_inv(q, q);
                  Value value = normalize(q);
                  MPQ_CLEAR(q);
                  return value;
                }
              else
                {
                  // n < 0
                  mpz_t z;
                  mpz_init(z);
                  mpz_ui_pow_ui(z, -n, -exp);
                  if (exp % 2)
                    mpz_neg(z, z);
                  mpq_t q;
                  mpq_init(q);
                  mpq_set_z(q, z);
                  MPZ_CLEAR(z);
                  mpq_inv(q, q);
                  Value value = normalize(q);
                  MPQ_CLEAR(q);
                  return value;
                }
            }
        }
      if (bignump(base))
        {
          if (exp == 0)
            return FIXNUM_ONE;
          if (exp > 0)
            {
              mpz_t z;
              mpz_init_set(z, the_bignum(base)->_z);
              if (mpz_sgn(z) > 0)
                mpz_pow_ui(z, z, exp);
              else
                {
                  // mpz_sgn(z) < 0
                  mpz_neg(z, z);
                  mpz_pow_ui(z, z, exp);
                  if (exp % 2)
                    mpz_neg(z, z);
                }
              Value value = normalize(z);
              MPZ_CLEAR(z);
              return value;
            }
          else
            {
              // exp < 0
              assert(exp < 0);
              mpz_t z;
              mpz_init_set(z, the_bignum(base)->_z);
              if (mpz_sgn(z) > 0)
                {
                  mpz_pow_ui(z, z, -exp);
                  mpq_t q;
                  mpq_init(q);
                  mpq_set_z(q, z);
                  MPZ_CLEAR(z);
                  mpq_inv(q, q);
                  Value value = normalize(q);
                  MPQ_CLEAR(q);
                  return value;
                }
              else
                {
                  // mpz_sgn(z) < 0
                  mpz_neg(z, z);
                  mpz_pow_ui(z, z, -exp);
                  if (exp % 2)
                    mpz_neg(z, z);

                  mpq_t q;
                  mpq_init(q);
                  mpq_set_z(q, z);
                  MPZ_CLEAR(z);
                  mpq_inv(q, q);
                  Value value = normalize(q);
                  MPQ_CLEAR(q);
                  return value;
                }
            }
        }
      if (ratiop(base))
        {
          Ratio * r = the_ratio(base);
          Value numerator = r->numerator();
          Value denominator = r->denominator();
          return SYS_two_arg_slash(CL_expt(numerator, power), CL_expt(denominator, power));
        }
      if (single_float_p(base))
        {
          mpfr_t x, y;
          mpfr_init2(x, 24); // 24-bit precision
          mpfr_init2(y, 24);
          mpfr_set_d(x, the_single_float(base)->_f, GMP_RNDN);
          mpfr_set_d(y, exp, GMP_RNDN);
          mpfr_pow(x, x, y, GMP_RNDN);
          double d = mpfr_get_d(x, GMP_RNDN);
          mpfr_clear(x);
          if (d < FLT_MIN)
            return signal_lisp_error(new FloatingPointUnderflow());
          return make_single_float(d);
        }
      else if (double_float_p(base))
        {
          mpfr_t x, y;
          mpfr_init2(x, 53); // 53-bit precision
          mpfr_init2(y, 53);
          mpfr_set_d(x, the_double_float(base)->_d, GMP_RNDN);
          mpfr_set_d(y, exp, GMP_RNDN);
          mpfr_pow(x, x, y, GMP_RNDN);
          double d = mpfr_get_d(x, GMP_RNDN);
          mpfr_clear(x);
          if (d == 0)
            return signal_lisp_error(new FloatingPointUnderflow());
          return make_double_float(d);
        }
      if (complexp(base))
        {
          Value result = FIXNUM_ONE;
          if (exp > 0)
            {
              for (long i = exp; i-- > 0;)
                result = SYS_two_arg_star(result, base);
            }
          else if (exp < 0)
            {
              for (long i = -exp; i-- > 0;)
                result = SYS_two_arg_slash(result, base);
            }
          return result;
        }
    }
  if (complexp(base) || complexp(power))
    return CL_exp(SYS_two_arg_star(power, log1(base)));
  // power is not a fixnum
  if (double_float_p(base))
    return make_value(new DoubleFloat(pow(the_double_float(base)->_d,
                                          the_single_float(coerce_to_single_float(power))->_f)));
  else
    return make_value(new SingleFloat(powf(the_single_float(coerce_to_single_float(base))->_f,
                                           the_single_float(coerce_to_single_float(power))->_f)));
}

// ### sqrt number => root
Value CL_sqrt(Value arg)
{
  switch (typecode_of(arg))
    {
    case TYPECODE_FIXNUM:
    case TYPECODE_BIGNUM:
    case TYPECODE_RATIO:
    case TYPECODE_SINGLE_FLOAT:
      {
        float f = the_single_float(coerce_to_single_float(arg))->_f;
        bool neg = f < 0;
        if (neg)
          f = -f;
        Value value = make_single_float(sqrtf(f));
        if (neg)
          return make_value(new Complex(make_single_float(0), value));
        else
          return value;
      }
    case TYPECODE_DOUBLE_FLOAT:
      {
        double d = the_double_float(arg)->_d;
        bool neg = d < 0;
        if (neg)
          d = -d;
        Value value = make_double_float(sqrt(d));
        if (neg)
          return make_value(new Complex(make_double_float(0), value));
        else
          return value;
      }
    case TYPECODE_COMPLEX:
      {
        Complex * c = the_complex(arg);
        Value imagpart = c->imagpart();
        if (zerop(imagpart))
          {
            Value realpart = c->realpart();
            if (minusp(realpart))
              return make_complex(imagpart, CL_sqrt(negate(realpart)));
            else
              return make_complex(CL_sqrt(realpart), imagpart);
          }
        return CL_exp(SYS_two_arg_slash(log1(arg), FIXNUM_TWO));
      }
    default:
      return signal_type_error(arg, S_number);
    }
}

// ### two-arg-gcd
Value SYS_gcd_2(Value arg1, Value arg2)
{
  mpz_t z1, z2;
  long typecode1 = typecode_of(arg1);
  long typecode2 = typecode_of(arg2);
  switch (typecode1)
    {
    case TYPECODE_FIXNUM:
      mpz_init_set_si(z1, xlong(arg1));
      break;
    case TYPECODE_BIGNUM:
      mpz_init_set(z1, the_bignum(arg1)->_z);
      break;
    default:
      return signal_type_error(arg1, S_integer);
    }
  switch (typecode2)
    {
    case TYPECODE_FIXNUM:
      mpz_init_set_si(z2, xlong(arg2));
      break;
    case TYPECODE_BIGNUM:
      mpz_init_set(z2, the_bignum(arg2)->_z);
      break;
    default:
      return signal_type_error(arg2, S_integer);
    }
  mpz_gcd(z1, z1, z2);
  Value result = normalize(z1);
  MPZ_CLEAR(z1);
  MPZ_CLEAR(z2);
  return result;
}
