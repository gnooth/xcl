// debug.cpp
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

// ### show-control-frames
Value SYS_print_control_frames()
{
  current_thread()->print_control_frames();
  return NIL;
}

// ### crash
Value SYS_crash()
{
  int *x = 0;
  *x = 42;
  return NIL;
}

unsigned long debug_level;

// ## debug-level
Value SYS_debug_level(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return make_unsigned_fixnum(debug_level);
    case 1:
      debug_level = fixnum_value(args[0]);
      return args[0];
    default:
      return wrong_number_of_arguments(S_debug_level, numargs, 0, 1);
    }
}
