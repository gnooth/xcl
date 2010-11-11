// CompiledFunction.cpp
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

#include "lisp.hpp"
#include "CompiledFunction.hpp"
// #include "Primitive.hpp"
#include "SimpleArray_UB8_1.hpp"

// ### compiled-function-constants
Value SYS_compiled_function_constants(Value arg)
{
//   return check_compiled_function(arg)->constants();
  if (compiled_function_p(arg))
    return the_compiled_function(arg)->constants();
  else
    return NIL;
}

// ### make-compiled-function name code minargs maxargs constants => primitive
Value SYS_make_compiled_function(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  SimpleArray_UB8_1 * vector;
  if (consp(arg2))
    {
      INDEX len = length(arg2);
      vector = new_simple_array_ub8_1(len);
      for (INDEX i = 0; i < len; i++)
        {
          vector->aset(i, car(arg2));
          arg2 = xcdr(arg2);
        }
    }
  else
    vector = check_simple_array_ub8_1(arg2);
  long minargs = fixnum_value(arg3);
  long maxargs = fixnum_value(arg4);
  return make_value(new CompiledFunction(arg1, vector->data(), vector->length(), minargs, maxargs, arg5));
}
