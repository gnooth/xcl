// subsetp.cpp
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

// ### subsetp-eql list-1 list-2 => generalized-boolean
Value SYS_subsetp_eql(Value arg1, Value arg2)
{
  while (arg1 != NIL)
    {
      Value item1 = car(arg1);
      Value list2 = arg2;
      bool found = false;
      while (list2 != NIL)
        {
          Value item2 = car(list2);
          if (eql(item1, item2))
            {
              found = true;
              break;
            }
          list2 = xcdr(list2);
        }
      if (!found)
        return NIL;
      arg1 = xcdr(arg1);
    }
  return T;
}
