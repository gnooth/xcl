// progv.cpp
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
#include "runtime.h"

// ### progv symbols values form* => result*
Value CL_progv(Value args, Environment * env, Thread * thread)
{
  const long numargs = length(args);
  if (numargs < 2)
    return wrong_number_of_arguments(S_progv, numargs, 2, MANY);
  Value symbols = check_list(eval(xcar(args), env, thread));
  Value values = check_list(eval(xcadr(args), env, thread));
  void * last_special_binding = thread->last_special_binding();
  // set up the new bindings
  RT_progv_bind_vars(thread, symbols, values);
  // implicit PROGN
  Value result = NIL;
  Value body = cdr(cdr(args));
  while (body != NIL)
    {
      result = eval(car(body), env, thread);
      body = xcdr(body);
    }
  thread->set_last_special_binding(last_special_binding);
  return result;
}
