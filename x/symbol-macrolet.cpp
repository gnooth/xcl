// symbol-macrolet.cpp
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
#include "SymbolMacro.hpp"
#include "ProgramError.hpp"

Value CL_symbol_macrolet(Value args, Environment * env, Thread * thread)
{
  Value varlist = check_list(car(args));
  Value result = NIL;
  void * last_special_binding = thread->last_special_binding();
  Environment * ext = new Environment(env);

  if (varlist != NIL)
    {
      for (unsigned long i = length(varlist); i-- > 0;)
        {
          Value obj = car(varlist);
          varlist = xcdr(varlist);
          if (consp(obj) && length(obj) == 2)
            {
              Value name = xcar(obj);
              Symbol * sym = check_symbol(name);
              if (sym->is_special_variable() || env->is_declared_special(name))
                {
                  String * s = new String("Attempt to bind the special variable ");
                  s->append(::prin1_to_string(name));
                  s->append(" with SYMBOL-MACROLET.");
                  return signal_lisp_error(new ProgramError(s));
                }
              ext->bind(name, make_value(new SymbolMacro(xcar(xcdr(obj)))));
            }
          else
            {
              String * s = new String("Malformed symbol-expansion pair in SYMBOL-MACROLET: ");
              s->append(::prin1_to_string(obj));
              return signal_lisp_error(new ProgramError(s));
            }
        }
    }

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
                      Value name = car(vars);

                      // "If declaration contains a special declaration that
                      // names one of the symbols being bound by SYMBOL-MACROLET,
                      // an error of type PROGRAM-ERROR is signaled."
                      Value list = xcar(args);
                      while (list != NIL)
                        {
                          if (xcar(xcar(list)) == name)
                            {
                              String * s = new String(::prin1_to_string(name));
                              s->append(" is a symbol-macro and thus can't be declared special.");
                              return signal_lisp_error(new ProgramError(s));
                            }
                          list = xcdr(list);
                        }

                      specials = make_cons(name, specials);
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
  while (specials != NIL)
    {
      ext->declare_special(xcar(specials));
      specials = xcdr(specials);
    }

  while (body != NIL)
    {
      result = eval(car(body), ext, thread);
      body = xcdr(body);
    }
  thread->set_last_special_binding(last_special_binding);
  return result;
}
