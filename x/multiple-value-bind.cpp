// multiple-value-bind.cpp
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
#include "Environment.hpp"

// ### multiple-value-bind (var*) values-form declaration* form* => result*
// Should be a macro.
Value CL_multiple_value_bind(Value args, Environment * env, Thread * thread)
{
  Value vars = car(args);
  args = xcdr(args);
  Value values_form = car(args);
  Value body = xcdr(args);
  Value primary_value = eval(values_form, env, thread);

//   long values_length = thread->values_length();
//   Value * values;
//   if (values_length < 0)
//     {
//       // eval() did not return multiple values.
//       values = new (GC) Value[1];
//       values[0] = primary_value;
//       values_length = 1;
//     }
//   else
//     values = thread->values();
  Value * values = thread->get_values(primary_value, ::length(vars));

  // Process declarations.
  Value specials = NIL;
  while (body != NIL)
    {
      Value obj = car(body);
      if (consp(obj) && xcar(obj) == S_declare)
        {
          Value decls = xcdr(obj);
          while (decls != NIL)
            {
              Value decl = car(decls);
              if (consp(decl) && xcar(decl) == S_special)
                {
                  Value vars = xcdr(decl);
                  while (vars != NIL)
                    {
                      specials = make_cons(car(vars), specials);
                      vars = xcdr(vars);
                    }
                }
              decls = xcdr(decls);
            }
          body = xcdr(body);
        }
      else
        break;
    }

  void * last_special_binding = thread->last_special_binding();
  Environment * ext = new Environment(env);
  int i = 0;
  Value var = car(vars);
  while (var != NIL)
    {
      Symbol * symbol = check_symbol(var);
//       Value val = i < values_length ? values[i] : NIL;
      Value val = values[i];
      if (specials != NIL && memq(var, specials))
        {
          thread->bind_special(var, val);
          ext->declare_special(var);
        }
      else if (symbol->is_special_variable())
        thread->bind_special(var, val);
      else
        ext->bind(var, val);

      vars = xcdr(vars);
      var = car(vars);
      ++i;
    }

  // Make sure free special declarations are visible in the body.
  // "The scope of free declarations specifically does not include
  // initialization forms for bindings established by the form
  // containing the declarations." (3.3.4)
  while (specials != NIL)
    {
      ext->declare_special(xcar(specials));
      specials = xcdr(specials);
    }

  thread->set_values_length(-1);

  Value result = NIL;
  while (body != NIL)
    {
      result = eval(car(body), ext, thread);
      body = xcdr(body);
    }
  thread->set_last_special_binding(last_special_binding);
  return result;
}
