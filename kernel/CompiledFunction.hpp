// CompiledFunction.hpp
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

#ifndef __COMPILED_FUNCTION_HPP
#define __COMPILED_FUNCTION_HPP

#include "Primitive.hpp"

class CompiledFunction : public Primitive
{
private:
  Value _constants;

public:
  CompiledFunction(Value name, void * code, unsigned long code_size,
                   unsigned int minargs, unsigned int maxargs, Value constants)
    : Primitive(WIDETAG_COMPILED_FUNCTION, name), _constants(constants)
  {
    _code = code;
    _code_size = make_unsigned_integer(code_size);
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
  // not reached
  return NULL;
}

#endif // CompiledFunction.hpp
