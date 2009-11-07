// WrongNumberOfArgumentsError.cpp
//
// Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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
#include "ProgramError.hpp"
#include "WrongNumberOfArgumentsError.hpp"

WrongNumberOfArgumentsError::WrongNumberOfArgumentsError()
  : _op(NULL_VALUE), _numargs(-1), _min(-1), _max(-1)
{
}

WrongNumberOfArgumentsError::WrongNumberOfArgumentsError(Value op)
  : _op(op),  _numargs(-1), _min(-1), _max(-1)
{
}

WrongNumberOfArgumentsError::WrongNumberOfArgumentsError(Value op, long numargs, long min, long max)
  : _op(op), _numargs(numargs), _min(min), _max(max)
{
}

AbstractString * WrongNumberOfArgumentsError::write_to_string()
{
  if (current_thread()->symbol_value(S_print_escape) != NIL)
    return unreadable_string();

  String * s = new String("Wrong number of arguments");
  if (_op != NULL_VALUE)
    {
      s->append(" for ");
      Value name = typed_object_p(_op) ? the_typed_object(_op)->operator_name() : NULL_VALUE;
      if (name != NULL_VALUE && name != NIL)
        s->append(::princ_to_string(name));
      else
        s->append(::princ_to_string(_op));
    }
  if (_numargs >= 0)
    {
      s->append(" (expected ");
      if (_min >= 0 && _max >= 0)
        {
          if (_min == _max)
            {
              s->append_long(_max);
            }
          else
            {
              s->append_long(_min);
              s->append_char('-');
              s->append_long(_max);
            }
        }
      else
        {
          s->append("at least ");
          s->append_long(_min);
        }
      s->append(", but received only ");
      s->append_long(_numargs);
      s->append_char(')');
    }
  s->append(".");
  return s;
}
