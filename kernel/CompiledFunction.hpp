// CompiledFunction.hpp
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

#ifndef __COMPILED_FUNCTION_HPP
#define __COMPILED_FUNCTION_HPP

// #include "lisp.hpp"
#include "Primitive.hpp"
// #include "SimpleArray_UB8_1.hpp"

class CompiledFunction : public Primitive
{
private:
  Value _constants;

public:
  CompiledFunction(Value name, void * code, unsigned int minargs, unsigned int maxargs, Value constants)
//     : Primitive(name, code, minargs, maxargs, false), _constants(constants)
    : Primitive(WIDETAG_COMPILED_FUNCTION, name), _constants(constants)
  {
    _code = code;
    _minargs = minargs;
    _maxargs = maxargs;
    if (minargs == maxargs)
      _arity = minargs;
    else
      _arity = -1;
  }

  Value constants()
  {
    return _constants;
  }
};

// REVIEW
// not to be confused with CL_compiled_function_p()
inline bool compiled_function_p(Value value)
{
//   if (typed_object_p(value))
//     {
//       switch (the_typed_object(value)->widetag())
//         {
//         case WIDETAG_FUNCTION:
//         case WIDETAG_PRIMITIVE:
//         case WIDETAG_CLOSURE:
//         case WIDETAG_AUTOLOAD:
//         case WIDETAG_MACRO:
//         case WIDETAG_STANDARD_GENERIC_FUNCTION:
//         case WIDETAG_COMPILED_CLOSURE:
//           return true;
//         default:
//           ;
//           // Fall through...
//         }
//     }
//   return false;
  return (typed_object_p(value) && the_typed_object(value)->widetag() == WIDETAG_COMPILED_FUNCTION);
}

inline CompiledFunction * the_compiled_function(Value value)
{
  assert(compiled_function_p(value));
  return reinterpret_cast<CompiledFunction *>(value - LOWTAG_TYPED_OBJECT);
}

inline CompiledFunction * check_compiled_function(Value value)
{
  if (compiled_function_p(value))
    return the_compiled_function(value);
  signal_type_error(value, S_compiled_function); // REVIEW
  // Not reached.
  return NULL;
}

#endif // CompiledFunction.hpp
