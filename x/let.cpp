// let.cpp
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

// ### let
Value CL_let(Value args, Environment * env, Thread * thread)
{
  Value varlist = check_list(car(args));
  void * last_special_binding = thread->last_special_binding();
  Value body = xcdr(args);
  assert(listp(body));
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
  assert(listp(body));
  Environment * ext = new Environment(env);
  const long len = length(varlist);
  Value * vals = new (GC) Value[len];
  for (long i = 0; i < len; i++)
    {
      Value spec = car(varlist);
      if (consp(spec))
        {
          if (length(spec) > 2)
            return signal_lisp_error(new Error("Invalid LET binding specification."));
          vals[i] = eval(car(xcdr(spec)), env, thread);
        }
      else
        vals[i] = NIL;
      varlist = xcdr(varlist);
    }
  varlist = car(args);
  long i = 0;
  while (varlist != NIL)
    {
      Value spec = car(varlist);
      Value name = consp(spec) ? xcar(spec) : spec;
      Symbol * symbol = check_symbol(name);
      Value value = vals[i];
      if (specials != NIL && memq(name, specials))
        {
          thread->bind_special(name, value);
          ext->declare_special(name);
        }
      else if (symbol->is_special_variable())
        thread->bind_special(name, value);
      else
        ext->bind(name, value);
      varlist = xcdr(varlist);
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
  Value result = NIL;
  while (body != NIL)
    {
      result = eval(car(body), ext, thread);
      body = xcdr(body);
    }
  thread->set_last_special_binding(last_special_binding);
  return result;
}

// ### let*
Value CL_let_star(Value args, Environment * env, Thread * thread)
{
  Value varlist = check_list(car(args));
  void * last_special_binding = thread->last_special_binding();
  Value body = xcdr(args);
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
  Environment * ext = new Environment(env);
  while (varlist != NIL)
    {
      Value obj = car(varlist);
      Value name, value;
      Symbol * symbol;
      if (consp(obj))
        {
          if (length(obj) > 2)
            return signal_lisp_error(new Error("Invalid LET* binding specification."));
          name = xcar(obj);
          symbol = check_symbol(name);
          value = eval(car(xcdr(obj)), ext, thread);
        }
      else
        {
          name = obj;
          symbol = check_symbol(name);
          value = NIL;
        }
      if (specials != NIL && memq(name, specials))
        {
          thread->bind_special(name, value);
          ext->declare_special(name);
        }
      else if (symbol->is_special_variable())
        thread->bind_special(name, value);
      else
        ext->bind(name, value);
      varlist = xcdr(varlist);
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
  Value result = NIL;
  while (body != NIL)
    {
      result = eval(car(body), ext, thread);
      body = xcdr(body);
    }
  thread->set_last_special_binding(last_special_binding);
  return result;
}
