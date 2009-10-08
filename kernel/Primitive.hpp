// Primitive.hpp
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

#ifndef __PRIMITIVE_HPP
#define __PRIMITIVE_HPP

class Primitive : public Function
{
protected:
  void * _code;
  Value _code_size;

  Primitive(long widetag, Value name)
    : Function(widetag, name), _code_size(NIL)
  {
  }

public:
  Primitive(Value name, void * code, unsigned int minargs, unsigned int maxargs)
    : Function(WIDETAG_PRIMITIVE, name), _code_size(NIL)
  {
    _code = code;
    _minargs = minargs;
    _maxargs = maxargs;
    if (minargs == maxargs)
      _arity = minargs;
    else
      _arity = -1;
    if (symbolp(name))
      the_symbol(name)->set_kernel_function(true);
  }

  Primitive(Value name, void * code, unsigned int minargs, unsigned int maxargs, bool kernel_function_p)
    : Function(WIDETAG_PRIMITIVE, name), _code_size(NIL)
  {
    _code = code;
    _minargs = minargs;
    _maxargs = maxargs;
    if (minargs == maxargs)
      _arity = minargs;
    else
      _arity = -1;
    if (kernel_function_p && symbolp(name))
      the_symbol(name)->set_kernel_function(true);
  }

  void * code()
  {
    return _code;
  }

  Value code_size()
  {
    return _code_size;
  }

  void set_code_size(Value size)
  {
    _code_size = size;
  }

  virtual Value execute();
  virtual Value execute(Value arg);
  virtual Value execute(Value arg1, Value arg2);
  virtual Value execute(Value arg1, Value arg2, Value arg3);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6);
  virtual Value execute(unsigned int numargs, Value args[]);
};

#endif // Primitive.hpp
