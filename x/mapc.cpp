// mapc.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"

// ### mapc2 function list => result-list
Value SYS_mapc2(Value arg1, Value arg2)
{
  TypedObject * function = coerce_to_function(arg1);
  Thread * thread = current_thread();
  Value list = arg2;
  while (list != NIL)
    {
      thread->execute(function, car(list));
      list = xcdr(list);
    }
  thread->clear_values();
  return arg2;
}

// ### fast-mapc2 function list => result-list
Value SYS_fast_mapc2(Value arg1, Value arg2)
{
  TypedObject * function = coerce_to_function(arg1);
  Value list = arg2;
  while (list != NIL)
    {
      function->execute(car(list));
      list = xcdr(list);
    }
  current_thread()->clear_values();
  return arg2;
}

// ### mapc function &rest lists+ => list-1
Value CL_mapc(unsigned int numargs, Value args[])
{
  if (numargs < 2)
    return wrong_number_of_arguments(S_mapc, numargs, 2, MANY);
  TypedObject * function = coerce_to_function(args[0]);
  INDEX common_length = (INDEX) -1;
  for (unsigned int i = 1; i < numargs; i++)
    {
      if (!listp(args[i]))
        signal_type_error(args[i], S_list);
      INDEX len = length(args[i]);
      if (common_length > len)
        common_length = len;
    }
  Value result = args[1];
  unsigned int num_funcall_args = numargs - 1;
  Value * funcall_args = new (GC) Value[num_funcall_args];
  Thread * thread = current_thread();
  for (INDEX i = 0; i < common_length; i++)
    {
      for (INDEX j = 0; j < num_funcall_args; j++)
        funcall_args[j] = xcar(args[j + 1]);
      funcall(function, num_funcall_args, funcall_args, thread);
      for (unsigned int j = 1; j < numargs; j++)
        args[j] = xcdr(args[j]);
    }
  thread->clear_values();
  return result;
}
