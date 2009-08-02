// mapcar.cpp
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

// ### mapcar2 function list => result-list
Value SYS_mapcar2(Value arg1, Value arg2)
{
  TypedObject * function = coerce_to_function(arg1);
  Thread * thread = current_thread();
  Value list = check_list(arg2);
  if (list == NIL)
    return NIL;
  Value value = thread->execute(function, xcar(list));
  list = xcdr(list);
  Value result = make_cons(value);
  Value splice = result;
  while (list != NIL)
    {
      value = thread->execute(function, car(list));
      list = xcdr(list);
      Value temp = make_cons(value);
      setcdr(splice, temp);
      splice = temp;
    }
  thread->clear_values();
  return result;
}

// ### fast-mapcar2 function list => result-list
Value SYS_fast_mapcar2(Value arg1, Value arg2)
{
  TypedObject * function = coerce_to_function(arg1);
  Value list = check_list(arg2);
  if (list == NIL)
    return NIL;
  Value value = function->execute(xcar(list));
  list = xcdr(list);
  Value result = make_cons(value);
  Value splice = result;
  while (list != NIL)
    {
      value = function->execute(car(list));
      list = xcdr(list);
      Value temp = make_cons(value);
      setcdr(splice, temp);
      splice = temp;
    }
  current_thread()->clear_values();
  return result;
}

// ### mapcar function &rest lists+ => result-list
Value CL_mapcar(unsigned int numargs, Value args[])
{
  if (numargs < 2)
    return wrong_number_of_arguments(S_mapcar, numargs, 2, MANY);
  if (numargs == 2)
    return SYS_mapcar2(args[0], args[1]);
  TypedObject * function = coerce_to_function(args[0]);
  Thread * thread = current_thread();
  // numargs > 2
  INDEX common_length = (INDEX) -1;
  for (unsigned int i = 1; i < numargs; i++)
    {
      if (!listp(args[i]))
        signal_type_error(args[i], S_list);
      INDEX len = length(args[i]);
      if (common_length > len)
        common_length = len;
    }
  Value * results = new (GC) Value[common_length];
  unsigned int num_funcall_args = numargs - 1;
  Value * funcall_args = new (GC) Value[num_funcall_args];
  for (INDEX i = 0; i < common_length; i++)
    {
      for (unsigned int j = 0; j < num_funcall_args; j++)
        funcall_args[j] = car(args[j + 1]);
      results[i] = funcall(function, num_funcall_args, funcall_args, thread);
      for (unsigned int j = 1; j < numargs; j++)
        args[j] = cdr(args[j]);
    }
  thread->clear_values();
  Value result = NIL;
  for (INDEX i = common_length; i-- > 0;)
    result = make_cons(results[i], result);
  return result;
}
