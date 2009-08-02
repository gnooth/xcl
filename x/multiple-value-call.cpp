// multiple-value-call.cpp
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

// ### multiple-value-call
Value CL_multiple_value_call(Value args, Environment * env, Thread * thread)
{
  const unsigned long numargs = length(args);
  if (numargs == 0)
    return wrong_number_of_arguments(S_multiple_value_call, numargs, 1, MANY);
  Function * function;
  Value value = eval(car(args), env, thread);
  args = xcdr(args);
  if (symbolp(value))
    {
      Symbol * sym = the_symbol(value);
      if (sym->is_special_operator() || sym->is_macro()
          || (function = (Function *) sym->function()) == NULL)
        {
          String * string = new String("The symbol ");
          string->append(sym->prin1_to_string());
          string->append(" does not designate a function.");
          return signal_lisp_error(new Error(string));
        }
    }
  else if (functionp(value))
    function = the_function(value);
  else
    {
      String * string = new String("The value ");
      string->append(::prin1_to_string(value));
      string->append(" does not designate a function.");
      return signal_lisp_error(new Error(string));
    }
  Value list = NIL;
  while (args != NIL)
    {
      Value result = eval(car(args), env, thread);
      if (thread->values_length() >= 0)
        {
          Value * values = thread->values();
          const long limit = thread->values_length();
          for (long i = 0; i < limit; i++)
            list = make_cons(values[i], list);
        }
      else
        list = make_cons(result, list);
      args = xcdr(args);
    }
  unsigned long len = length(list);
  Value * funcall_args = new (GC) Value[len + 1];
  funcall_args[0] = make_value(function);
  if (list != NIL)
    {
      for (long i = len; i > 0; i--)
        {
          funcall_args[i] = xcar(list);
          list = xcdr(list);
        }
    }
  return CL_funcall(len + 1, funcall_args);
}
