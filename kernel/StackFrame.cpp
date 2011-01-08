// StackFrame.cpp
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
#include "StackFrame.hpp"

Value StackFrame::to_list()
{
  Value args = NIL;
  if (_args != NULL)
    {
      for (unsigned int i = _numargs; i-- > 0;)
        args = make_cons(_args[i], args);
    }
  else if (_numargs > 0)
    {
      switch (_numargs)
        {
        case 6:
          args = make_cons(_arg6, args);
        case 5:
          args = make_cons(_arg5, args);
        case 4:
          args = make_cons(_arg4, args);
        case 3:
          args = make_cons(_arg3, args);
        case 2:
          args = make_cons(_arg2, args);
        case 1:
          args = make_cons(_arg1, args);
        }
    }
  if (function())
    {
      Value name = function()->operator_name();
      if (name != NULL_VALUE && name != NIL)
        {
          Value frame = make_cons(name, args);
          return make_cons(make_unsigned_integer((unsigned long)_sp), frame);
        }
    }
  Value frame = make_cons(make_value(function()), args);
  return make_cons(make_unsigned_integer((unsigned long)_sp), frame);
}
