// random.cpp
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

#include <time.h>

#include "lisp.hpp"
#include "RandomState.hpp"

RandomState::RandomState(RandomState * rs) : TypedObject(WIDETAG_RANDOM_STATE)
{
  gmp_randinit_default(_state);
  if (rs)
    gmp_randseed(_state, rs->_state->_mp_seed);
  else
    gmp_randseed_ui(_state, time(NULL));
}

RandomState::~RandomState()
{
  gmp_randclear(_state);
}

Value RandomState::type_of() const
{
  return S_random_state;
}

Value RandomState::class_of() const
{
  return C_random_state;
}

bool RandomState::typep(Value type) const
{
  return (type == S_random_state || type == S_atom || type == T
          || type == C_random_state || type == C_t);
}

Value RandomState::random(Value arg)
{
    if (fixnump(arg))
      {
        long n = xlong(arg);
        if (n > 0)
          {
            mpz_t limit;
            mpz_init_set_si(limit, n);
            mpz_t result;
            mpz_init(result);
            mpz_urandomm(result, _state, limit);
            return normalize(result);
          }
      }
    else if (bignump(arg))
      {
        Bignum * b = the_bignum(arg);
        if (b->plusp())
          {
            mpz_t result;
            mpz_init(result);
            mpz_urandomm(result, _state, b->_z);
            return normalize(result);
          }
      }
    else if (single_float_p(arg))
      {
        float f = the_single_float(arg)->_f;
        if (f > 0)
          {
            mpz_t fixnum_limit;
            mpz_init_set_si(fixnum_limit, MOST_POSITIVE_FIXNUM);
            mpz_t fixnum_result;
            mpz_init(fixnum_result);
            mpz_urandomm(fixnum_result, _state, fixnum_limit);
            double double_result = mpz_get_si(fixnum_result);
            double_result /= MOST_POSITIVE_FIXNUM;
            return make_value(new SingleFloat(double_result * f));
          }
      }
    else if (double_float_p(arg))
      {
        double d = the_double_float(arg)->_d;
        if (d > 0)
          {
            mpz_t fixnum_limit;
            mpz_init_set_si(fixnum_limit, MOST_POSITIVE_FIXNUM);
            mpz_t fixnum_result;
            mpz_init(fixnum_result);
            mpz_urandomm(fixnum_result, _state, fixnum_limit);
            double double_result = mpz_get_si(fixnum_result);
            double_result /= MOST_POSITIVE_FIXNUM;
            return make_value(new DoubleFloat(double_result * d));
          }
      }
    return signal_type_error(arg,
                             list3(S_or,
                                   list2(S_integer, list1(FIXNUM_ZERO)),
                                   list2(S_float, list1(FIXNUM_ZERO))));
}

AbstractString * RandomState::write_to_string()
{
  String * string = new String("#<");
  string->append(::prin1_to_string(S_random_state));
  string->append_char(' ');
  string->append(::prin1_to_string(normalize(_state->_mp_seed)));
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  string->append(buf);
  return string;
}

// ### make-random-state &optional state => new-state
Value CL_make_random_state(unsigned int numargs, Value args[])
{
  // "If STATE is a random state object, the NEW-STATE is a copy[5] of
  // that object. If state is NIL, the NEW-STATE is a copy[5] of the
  // current random state. If state is T, the NEW-STATE is a fresh random
  // state object that has been randomly initialized by some means."
  if (numargs > 1)
    return wrong_number_of_arguments(S_make_random_state, numargs, 0, 1);
  RandomState * rs;
  if (numargs == 0 || args[0] == NIL)
    rs = check_random_state(current_thread()->symbol_value(S__random_state_));
  else if (random_state_p(args[0]))
    rs = the_random_state(args[0]);
  else if (args[0] == T)
    rs = NULL;
  else
    return signal_type_error(args[0], S_random_state);
  return make_value(new RandomState(rs));
}

// ### random limit &optional random-state => random-number
Value CL_random(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return check_random_state(current_thread()->symbol_value(S__random_state_))->random(args[0]);
    case 2:
      return check_random_state(args[1])->random(args[0]);
    default:
      return wrong_number_of_arguments(S_random, numargs, 1, 2);
    }
}

// ### random-state-p object => generalized-boolean
Value CL_random_state_p(Value arg)
{
  return random_state_p(arg) ? T : NIL;
}
