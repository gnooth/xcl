// Primitive.cpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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
#include "Primitive.hpp"
#include "SimpleArray_UB8_1.hpp"
#include "WrongNumberOfArgumentsError.hpp"

Value Primitive::execute()
{
  if (_arity == 0)
    {
      Value (*code) () = (Value (*) ()) _code;
      return (*code)();
    }
  if (_arity < 0)
    {
      if (_minargs <= 0 && 0 <= _maxargs)
        {
          Value args[0];
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(0, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 0, _minargs, _maxargs);
}

Value Primitive::execute(Value arg)
{
  if (_arity == 1)
    {
      Value (*code) (Value) = (Value (*) (Value)) _code;
      return (*code)(arg);
    }
  if (_arity < 0)
    {
      if (_minargs <= 1 && 1 <= _maxargs)
        {
          Value args[1];
          args[0] = arg;
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(1, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 1, _minargs, _maxargs);
}

Value Primitive::execute(Value arg1, Value arg2)
{
  if (_arity == 2)
    {
      Value (*code) (Value, Value) = (Value (*) (Value, Value)) _code;
      return (*code)(arg1, arg2);
    }
  if (_arity < 0)
    {
      if (_minargs <= 2 && 2 <= _maxargs)
        {
          Value args[2];
          args[0] = arg1;
          args[1] = arg2;
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(2, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 2, _minargs, _maxargs);
}

Value Primitive::execute(Value arg1, Value arg2, Value arg3)
{
  if (_arity == 3)
    {
      Value (*code) (Value, Value, Value) = (Value (*) (Value, Value, Value)) _code;
      return (*code)(arg1, arg2, arg3);
    }
  if (_arity < 0)
    {
      if (_minargs <= 3 && 3 <= _maxargs)
        {
          Value args[3];
          args[0] = arg1;
          args[1] = arg2;
          args[2] = arg3;
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(3, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 3, _minargs, _maxargs);
}

Value Primitive::execute(Value arg1, Value arg2, Value arg3, Value arg4)
{
  if (_arity == 4)
    {
      Value (*code) (Value, Value, Value, Value) = (Value (*) (Value, Value, Value, Value)) _code;
      return (*code)(arg1, arg2, arg3, arg4);
    }
  if (_arity < 0)
    {
      if (_minargs <= 4 && 4 <= _maxargs)
        {
          Value args[4];
          args[0] = arg1;
          args[1] = arg2;
          args[2] = arg3;
          args[3] = arg4;
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(4, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 4, _minargs, _maxargs);
}

Value Primitive::execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  if (_arity == 5)
    {
      Value (*code) (Value, Value, Value, Value, Value) = (Value (*) (Value, Value, Value, Value, Value)) _code;
      return (*code)(arg1, arg2, arg3, arg4, arg5);
    }
  if (_arity < 0)
    {
      if (_minargs <= 5 && 5 <= _maxargs)
        {
          Value args[5];
          args[0] = arg1;
          args[1] = arg2;
          args[2] = arg3;
          args[3] = arg4;
          args[4] = arg5;
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(5, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 5, _minargs, _maxargs);
}

Value Primitive::execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  if (_arity == 6)
    {
      Value (*code) (Value, Value, Value, Value, Value, Value) = (Value (*) (Value, Value, Value, Value, Value, Value)) _code;
      return (*code)(arg1, arg2, arg3, arg4, arg5, arg6);
    }
  if (_arity < 0)
    {
      if (_minargs <= 6 && 6 <= _maxargs)
        {
          Value args[6];
          args[0] = arg1;
          args[1] = arg2;
          args[2] = arg3;
          args[3] = arg4;
          args[4] = arg5;
          args[5] = arg6;
          Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
          return (*code)(6, args);
        }
    }
  return wrong_number_of_arguments(operator_name(), 6, _minargs, _maxargs);
}

Value Primitive::execute(unsigned int numargs, Value args[])
{
  if (_minargs <= numargs && numargs <= _maxargs)
    {
      Value (*code) (unsigned int, Value *) = (Value (*) (unsigned int, Value *)) _code;
      return (*code)(numargs, args);
    }
  return wrong_number_of_arguments(operator_name(), numargs, _minargs, _maxargs);
}

// ### make-primitive name code minargs maxargs => primitive
Value SYS_make_primitive(Value arg1, Value arg2, Value arg3, Value arg4)
{
//   Symbol * sym = check_symbol(arg1);
  SimpleArray_UB8_1 * vector = check_simple_array_ub8_1(arg2);
  long minargs = fixnum_value(arg3);
  long maxargs = fixnum_value(arg4);
  return make_value(new Primitive(arg1, vector->data(), minargs, maxargs, false));
}

// ### kernel-function-p
Value SYS_kernel_function_p(Value arg)
{
  if (symbolp(arg))
    return the_symbol(arg)->is_kernel_function() ? T : NIL;
  if (typed_object_p(arg))
    {
      Value name = the_typed_object(arg)->operator_name();
      if (symbolp(name))
        return the_symbol(name)->is_kernel_function() ? T : NIL;
    }
  return NIL;
}

// ### function-arity function-designator
Value SYS_function_arity(Value arg)
{
  return make_fixnum(coerce_to_function(arg)->arity());
}

// ### verify-call function-designator numargs => boolean
Value SYS_verify_call(Value arg1, Value arg2)
{
  TypedObject * function = coerce_to_function(arg1);
  INDEX numargs = check_index(arg2);
  return (numargs >= function->minargs() && numargs <= function->maxargs()) ? T : NIL;
}

// ### function-name function-designator
Value SYS_function_name(Value arg)
{
  if (symbolp(arg))
    return arg;
  if (functionp(arg))
    return the_typed_object(arg)->operator_name();
  return signal_type_error(arg, S_function_designator);
}
