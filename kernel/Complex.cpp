// Complex.cpp
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
#include "Complex.hpp"

bool Complex::typep(Value type) const
{
  if (type == S_complex || type == S_number || type == S_atom || type == T)
    return true;

  if (type == C_complex || type == C_number || type == C_t)
    return true;

  return false;
}

Value Complex::add(long n) const
{
  return make_value(new Complex(SYS_two_arg_plus(_realpart, make_fixnum(n)), _imagpart));
}

Value Complex::add(Complex * c) const
{
  return make_complex(SYS_two_arg_plus(_realpart, c->_realpart),
                      SYS_two_arg_plus(_imagpart, c->_imagpart));
}

Value Complex::subtract(long n) const
{
  return make_value(new Complex(SYS_subtract_2(_realpart, make_fixnum(n)), _imagpart));
}

Value Complex::subtract(Complex * c) const
{
  return make_complex(SYS_subtract_2(_realpart, c->_realpart),
                      SYS_subtract_2(_imagpart, c->_imagpart));
}

Value Complex::negate() const
{
  return make_value(new Complex(::negate(_realpart), ::negate(_imagpart)));
}

bool Complex::eql(Value value) const
{
  if (complexp(value))
    {
      Complex * c = the_complex(value);
      if (::eql(_realpart, c->_realpart) && ::eql(_imagpart, c->_imagpart))
        return true;
    }
  return false;
}

bool Complex::equal(Value value) const
{
  if (complexp(value))
    {
      Complex * c = the_complex(value);
      if (::eql(_realpart, c->_realpart) && ::eql(_imagpart, c->_imagpart))
        return true;
    }
  return false;
}

bool Complex::equalp(Value value) const
{
  if (complexp(value))
    {
      Complex * c = the_complex(value);
      if (::eql(_realpart, c->_realpart) && ::eql(_imagpart, c->_imagpart))
        return true;
    }
  return false;
}

AbstractString * Complex::write_to_string()
{
  String * string = new String("#C(");
  string->append(::prin1_to_string(_realpart));
  string->append_char(' ');
  string->append(::prin1_to_string(_imagpart));
  string->append_char(')');
  return string;
}

Value make_complex(Value realpart, Value imagpart)
{
  if (!realp(realpart))
    return signal_type_error(realpart, S_real);
  if (!realp(imagpart))
    return signal_type_error(imagpart, S_real);
  // "If realpart is a rational and imagpart is the rational number zero,
  // the result of complex is realpart, a rational. Otherwise, the result
  // is a complex."
  if (rationalp(realpart) && imagpart == FIXNUM_ZERO)
    return realpart;
  // "If either realpart or imagpart is a float, the non-float is converted
  // to a float before the complex is created."
  if (double_float_p(realpart))
    {
      if (!double_float_p(imagpart))
        imagpart = coerce_to_double_float(imagpart);
    }
  else if (double_float_p(imagpart))
    {
      if (!double_float_p(realpart))
        realpart = coerce_to_double_float(realpart);
    }
  else if (single_float_p(realpart))
    {
      if (!single_float_p(imagpart))
        imagpart = coerce_to_single_float(imagpart);
    }
  else if (single_float_p(imagpart))
    {
      if (!single_float_p(realpart))
        realpart = coerce_to_single_float(realpart);
    }
  return make_value(new Complex(realpart, imagpart));
}

// ### complex realpart &optional imagpart => complex
Value CL_complex(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      if (single_float_p(args[0]))
        return make_value(new Complex(args[0], make_single_float(0)));
      if (double_float_p(args[0]))
        return make_value(new Complex(args[0], make_double_float(0)));
      if (rationalp(args[0]))
        return args[0];
      return signal_type_error(args[0], S_real);
    case 2:
      return make_complex(args[0], args[1]);
    default:
      return wrong_number_of_arguments(S_complex, numargs, 1, 2);
    }
}

// ### complexp object => generalized-boolean
Value CL_complexp(Value arg)
{
  return complexp(arg) ? T : NIL;
}

// ### realpart number => real
Value CL_realpart(Value arg)
{
  if (complexp(arg))
    return the_complex(arg)->realpart();
  if (realp(arg))
    return arg;
  return signal_type_error(arg, S_number);
}

// ### imagpart number => real
Value CL_imagpart(Value arg)
{
  if (complexp(arg))
    return the_complex(arg)->imagpart();
  if (single_float_p(arg))
    {
      if (the_single_float(arg)->bits() < 0)
        return make_value(new SingleFloat(-0.0));
      else
        return make_value(new SingleFloat(0.0));
    }
  if (double_float_p(arg))
    {
      if (the_double_float(arg)->high_bits() < 0)
        return make_value(new DoubleFloat(-0.0));
      else
        return make_value(new DoubleFloat(0.0));
    }
  if (rationalp(arg))
    return FIXNUM_ZERO;
  return signal_type_error(arg, S_number);
}
