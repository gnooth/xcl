// StackFrame.hpp
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

#ifndef __STACK_FRAME_HPP
#define __STACK_FRAME_HPP

class StackFrame : public gc
{
private:
  StackFrame * _next;
  TypedObject * _function;
  Value _arg1;
  Value _arg2;
  Value _arg3;
  Value _arg4;
  Value _arg5;
  Value _arg6;
  Value * _args;
  unsigned int _numargs;

public:
  StackFrame()
  {
    memset(this, 0, sizeof(*this));
  }

  StackFrame * next() const
  {
    return _next;
  }

  void set_next(StackFrame * next)
  {
    _next = next;
  }

  TypedObject * function() const
  {
    return _function;
  }

  void clear()
  {
    memset(this, 0, sizeof(*this));
  }

  void initialize_0(TypedObject * function)
  {
    _function = function;
    _args = NULL;
    _numargs = 0;
  }

  void initialize_1(TypedObject * function, Value arg)
  {
    _function = function;
    _arg1 = arg;
    _numargs = 1;
  }

  void initialize_2(TypedObject * function, Value arg1, Value arg2)
  {
    _function = function;
    _arg1 = arg1;
    _arg2 = arg2;
    _args = NULL;
    _numargs = 2;
  }

  void initialize_3(TypedObject * function, Value arg1, Value arg2, Value arg3)
  {
    _function = function;
    _arg1 = arg1;
    _arg2 = arg2;
    _arg3 = arg3;
    _args = NULL;
    _numargs = 3;
  }

  void initialize_4(TypedObject * function, Value arg1, Value arg2, Value arg3, Value arg4)
  {
    _function = function;
    _arg1 = arg1;
    _arg2 = arg2;
    _arg3 = arg3;
    _arg4 = arg4;
    _args = NULL;
    _numargs = 4;
  }

  void initialize_5(TypedObject * function, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
  {
    _function = function;
    _arg1 = arg1;
    _arg2 = arg2;
    _arg3 = arg3;
    _arg4 = arg4;
    _arg5 = arg5;
    _args = NULL;
    _numargs = 5;
  }

  void initialize_6(TypedObject * function, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
  {
    _function = function;
    _arg1 = arg1;
    _arg2 = arg2;
    _arg3 = arg3;
    _arg4 = arg4;
    _arg5 = arg5;
    _arg6 = arg6;
    _args = NULL;
    _numargs = 6;
  }

  void initialize(TypedObject * function, unsigned int numargs, Value * args, Value * temp)
  {
    _function = function;
    if (numargs > 6)
      {
        _args = temp;
        for (unsigned int i = numargs; i-- > 0;)
          _args[i] = args[i];
      }
    else
      {
        switch (numargs)
          {
          case 6:
            _arg6 = args[5];
          case 5:
            _arg5 = args[4];
          case 4:
            _arg4 = args[3];
          case 3:
            _arg3 = args[2];
          case 2:
            _arg2 = args[1];
          case 1:
            _arg1 = args[0];
          }
        _args = NULL;
      }
    _numargs = numargs;
  }

  Value to_list();
};

#endif // StackFrame.hpp
