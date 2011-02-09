// conditions.cpp
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
#include "primitives.hpp"
#include "ArithmeticError.hpp"
#include "CellError.hpp"
#include "CompilerError.hpp"
#include "CompilerUnsupportedFeatureError.hpp"
#include "ControlError.hpp"
#include "DivisionByZero.hpp"
#include "EndOfFile.hpp"
#include "FileError.hpp"
#include "FloatingPointInexact.hpp"
#include "FloatingPointInvalidOperation.hpp"
#include "FloatingPointOverflow.hpp"
#include "FloatingPointUnderflow.hpp"
#include "PackageError.hpp"
#include "ParseError.hpp"
#include "PrintNotReadable.hpp"
#include "ProgramError.hpp"
#include "ReaderError.hpp"
#include "SeriousCondition.hpp"
#include "SimpleCondition.hpp"
#include "SimpleError.hpp"
#include "SimpleTypeError.hpp"
#include "SimpleWarning.hpp"
#include "StorageCondition.hpp"
#include "StyleWarning.hpp"
#include "TypeError.hpp"
#include "UnboundSlot.hpp"
#include "UnboundVariable.hpp"
#include "UndefinedFunction.hpp"
#include "Warning.hpp"
#include "WrongNumberOfArgumentsError.hpp"

// ### %make-condition type initargs
Value SYS_make_condition_internal(Value type, Value initargs)
{
  if (classp(type))
    type = the_class(type)->name();
  if (symbolp(type))
    {
      Condition * condition = NULL;
      if (type ==S_arithmetic_error)
        condition = new ArithmeticError();
      else if (type == S_cell_error)
        condition = new CellError();
      else if (type == S_compiler_error)
        condition = new CompilerError();
      else if (type == S_compiler_unsupported_feature_error)
        condition = new CompilerUnsupportedFeatureError();
      else if (type == S_condition)
        condition = new Condition();
      else if (type == S_control_error)
        condition = new ControlError();
      else if (type == S_division_by_zero)
        condition = new DivisionByZero();
      else if (type == S_end_of_file)
        condition = new EndOfFile();
      else if (type == S_error)
        condition = new Error();
      else if (type == S_file_error)
        condition = new FileError();
      else if (type == S_floating_point_inexact)
        condition = new FloatingPointInexact();
      else if (type == S_floating_point_invalid_operation)
        condition = new FloatingPointInvalidOperation();
      else if (type == S_floating_point_overflow)
        condition = new FloatingPointOverflow();
      else if (type == S_floating_point_underflow)
        condition = new FloatingPointUnderflow();
      else if (type == S_package_error)
        condition = new PackageError();
      else if (type == S_parse_error)
        condition = new ParseError();
      else if (type == S_print_not_readable)
        condition = new PrintNotReadable();
      else if (type == S_program_error)
        condition = new ProgramError();
      else if (type == S_reader_error)
        condition = new ReaderError();
      else if (type == S_serious_condition)
        condition = new SeriousCondition();
      else if (type == S_simple_condition)
        condition = new SimpleCondition();
      else if (type == S_simple_error)
        condition = new SimpleError();
      else if (type == S_simple_type_error)
        condition = new SimpleTypeError();
      else if (type == S_simple_warning)
        condition = new SimpleWarning();
      else if (type == S_storage_condition)
        condition = new StorageCondition();
      else if (type == S_stream_error)
        condition = new StreamError();
      else if (type == S_style_warning)
        condition = new StyleWarning();
      else if (type == S_type_error)
        condition = new TypeError();
      else if (type == S_unbound_variable)
        condition = new UnboundVariable();
      else if (type == S_unbound_slot)
        condition = new UnboundSlot();
      else if (type == S_undefined_function)
        condition = new UndefinedFunction();
      else if (type == S_warning)
        condition = new Warning();
      if (condition)
        {
          if (initargs != NIL)
            condition->initialize(initargs);
          return make_value(condition);
        }
    }
  return NIL;
}

// ### make-condition type &rest initargs => condition
// redefined in clos.lisp
Value CL_make_condition(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_make_condition, 0, 1, MANY);
  Value type = args[0];
  Value initargs = NIL;
  if (numargs > 1)
    {
      for (unsigned long i = numargs; i-- > 1;)
        initargs = make_cons(args[i], initargs);
    }
  return SYS_make_condition_internal(type, initargs);
}

// ### error datum &rest arguments => |
Value CL_error(unsigned int numargs, Value args[])
{
  if (numargs >= 1)
    {
      Value datum = args[0];
      if (conditionp(datum))
        return signal_lisp_error(the_condition(datum));
      if (stringp(datum))
        {
          SimpleError * error;
          if (numargs == 1)
            error = new SimpleError(the_string(datum));
          else
            {
              error = new SimpleError();
              Value format_arguments = NIL;
              for (unsigned long i = numargs; i-- > 1;)
                format_arguments = make_cons(args[i], format_arguments);
              Value initargs = NIL;
              initargs = make_cons(format_arguments, initargs);
              initargs = make_cons(K_format_arguments, initargs);
              initargs = make_cons(datum, initargs);
              initargs = make_cons(K_format_control, initargs);
              error->initialize(initargs);
            }
          return signal_lisp_error(error);
        }
      Value initargs = NIL;
      if (numargs >= 2)
        {
          for (long i = numargs; i-- > 1;)
            initargs = make_cons(args[i], initargs);
        }
      Value condition = SYS_make_condition_internal(datum, initargs);
      if (condition != NIL)
        return signal_lisp_error(the_condition(condition));
    }
  return signal_lisp_error("Error!"); // FIXME
}

// ### simple-condition-format-control condition => format-control
Value CL_simple_condition_format_control(Value arg)
{
  return check_standard_object(arg)->slot_value(S_format_control);
}

// ### simple-condition-format-arguments condition => format-arguments
Value CL_simple_condition_format_arguments(Value arg)
{
  return check_standard_object(arg)->slot_value(S_format_arguments);
}

Value signal_lisp_error(Condition * condition)
{
  current_thread()->execute(the_symbol(S_error)->function(), make_value(condition));
  // not reached
  return 0;
}

Value signal_lisp_error(const char * s)
{
  return signal_lisp_error(new Error(s));
}

Value signal_lisp_error(AbstractString * s)
{
  return signal_lisp_error(new Error(s));
}

Value out_of_memory()
{
  return signal_lisp_error("Out of memory."); // FIXME storage-condition
}

Value wrong_number_of_arguments(Value op, long numargs, long min, long max)
{
  return signal_lisp_error(new WrongNumberOfArgumentsError(op, numargs, min, max));
}

Value signal_type_error(Value datum, Value expected_type)
{
  return signal_lisp_error(new TypeError(datum, expected_type));
}

Value signal_undefined_function(Value name)
{
  return signal_lisp_error(new UndefinedFunction(name));
}

// ### %type-error
Value SYS_type_error_internal(Value datum, Value expected_type)
{
  return signal_lisp_error(new TypeError(datum, expected_type));
}

// ### error-not-bit
Value SYS_error_not_bit(Value datum)
{
  return signal_lisp_error(new TypeError(datum, list3(S_integer, FIXNUM_ZERO, FIXNUM_ONE)));
}

// ### error-not-cons
Value SYS_error_not_cons(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_cons));
}

// ### error-not-fixnum
Value SYS_error_not_fixnum(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_fixnum));
}

// ### error-not-function
Value SYS_error_not_function(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_function));
}

// ### error-not-list
Value SYS_error_not_list(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_list));
}

// ### error-not-vector
Value SYS_error_not_vector(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_vector));
}

// ### error-not-simple-bit-vector
Value SYS_error_not_simple_bit_vector(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_simple_bit_vector));
}

// ### error-not-simple-string
Value SYS_error_not_simple_string(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_simple_string));
}

// ### error-not-simple-vector
Value SYS_error_not_simple_vector(Value datum)
{
  return signal_lisp_error(new TypeError(datum, S_simple_vector));
}

Value bad_index(Value index, Value expected_type)
{
  return signal_type_error(index, expected_type);
}

Value bad_index(INDEX index, long min, long max)
{
  return signal_type_error(make_unsigned_integer(index),
                           list3(S_integer, make_integer(min),
                                 list1(make_integer(max))));
}
