// position.cpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

// ### position-eql item sequence => position
Value SYS_position_eql(Value arg1, Value arg2)
{
  if (listp(arg2))
    return SYS_list_position_eql(arg1, arg2);
  else
    return SYS_vector_position_eql(arg1, arg2);
}

// ### list-position-eql item list => position
Value SYS_list_position_eql(Value arg1, Value arg2)
{
  INDEX pos = 0;
  while (true)
    {
      if (arg2 == NIL)
        return NIL;
      if (eql(arg1, car(arg2)))
        return make_fixnum(pos);
      arg2 = xcdr(arg2);
      ++pos;
    }
}

// ### vector-position-eql item vector => position
Value SYS_vector_position_eql(Value arg1, Value arg2)
{
  if (vectorp(arg2))
    {
      AbstractVector * v = the_vector(arg2);
      INDEX len = v->length();
      for (INDEX pos = 0; pos < len; pos++)
        {
          if (eql(arg1, v->aref(pos)))
            return make_fixnum(pos);
        }
      return NIL;
    }
  return signal_type_error(arg2, sequencep(arg2) ? S_vector : S_sequence);
}
