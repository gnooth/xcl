// eval.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

#if defined(WIN32)
#include <malloc.h>
#elif defined(__NetBSD__) || defined(__FreeBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

#include "lisp.hpp"
#include "Environment.hpp"
#include "Closure.hpp"
#include "Macro.hpp"
#include "SymbolMacro.hpp"
#include "SpecialOperator.hpp"
#include "UnboundVariable.hpp"
#include "UndefinedFunction.hpp"
#include "WrongNumberOfArgumentsError.hpp"

Value macroexpand_1(Value form, Environment * env, Thread * thread);
Value macroexpand(Value form, Environment * env, Thread * thread);
static Value eval_call(Function * function, Value args, Environment * env, Thread * thread);

Value macroexpand_1(Value form, Environment * env, Thread * thread)
{
  assert(env != NULL);
  if (consp(form))
    {
      length(form); // Force an error if form is not a proper list.
      Value first = xcar(form);
      if (symbolp(first))
        {
//           Symbol * sym = the_symbol(first);
//           if (sym->is_macro())
//             {
//               Function * function = (Function *) sym->function();
//               //               Value hook =
//               //                 coerceToFunction(Symbol.MACROEXPAND_HOOK.symbolValue(thread));
//               //               return thread.setValues(hook.execute(expander, form, env),
//               //                                       T);
//               return thread->set_values(function->execute(form, NIL), // FIXME should be env instead of NIL
//                                         T);
//             }

//           Function * function = sym->macro_function();
          Function * function = (Function *) env->lookup_function(first);

          if (function && function->widetag() == WIDETAG_AUTOLOAD)
            {
              reinterpret_cast<Autoload *>(function)->load();
              function = reinterpret_cast<Function *>(the_symbol(first)->function());
            }

          if (function && function->widetag() == WIDETAG_SPECIAL_OPERATOR)
            {
              Symbol * sym = the_symbol(first);
              Value value = sym->get(S_macro);
              if (autoloadp(value))
                {
                  the_autoload(value)->load();
                  value = sym->get(S_macro);
                }
              if (macrop(value))
                function = the_macro(value);
            }

          if (function && function->widetag() == WIDETAG_MACRO)
            {
//               printf("expanding macro %s\n", sym->prin1_to_string()->as_c_string());
//               if (env)
//                 printf("enviroment = %s\n", env->prin1_to_string()->as_c_string());
//               else
//                 printf("enviroment = NULL\n");

              // FIXME env should never be NULL
              Value env_val = (env != NULL) ? make_value(env) : NULL_VALUE;

//               return thread->set_values(function->execute(form, env_val),
//                                         T);

//               return thread->set_values(thread->execute(function, form, env_val),
//                                         T);

//               if (function->widetag() != WIDETAG_MACRO)
//                 {
//                   printf("macroexpand_1 form = %s\n",
//                          ::prin1_to_string(form)->as_c_string());
//                   fflush(stdout);
//                 }
//               assert(function->widetag() == WIDETAG_MACRO);

              Function * expansion_function =
                reinterpret_cast<Macro *>(function)->expansion_function();
              assert(expansion_function != NULL);
//               assert(expansion_function->arity() == 2);

              Value hook = thread->symbol_value(S_macroexpand_hook);

//               return thread->set_values(thread->execute(expansion_function,
//                                                         form, env_val),
//                                         T);
              return thread->set_values(thread->execute(coerce_to_function(hook),
                                                        make_value(expansion_function),
                                                        form, env_val),
                                        T);
            }
        }
    }
//   else if (form instanceof Symbol)
//     {
//       Symbol symbol = (Symbol) form;
//       LispObject obj = null;
//       if (symbol.isSpecialVariable())
//         obj = thread.lookupSpecial(symbol);
//       else
//         obj = env.lookup(symbol);
//       if (obj == null)
//         obj = symbol.getSymbolValue();
//       if (obj instanceof SymbolMacro)
//         return thread.setValues(((SymbolMacro)obj).getExpansion(), T);
//     }
  else if (symbolp(form))
    {
      Symbol * sym = the_symbol(form);
      Value value;
      if (sym->is_special_variable() || env->is_declared_special(form))
        value = thread->lookup_special(form);
      else
        value = env->lookup(form);
      if (symbol_macro_p(value))
        return thread->set_values(the_symbol_macro(value)->expansion(), T);
    }
  // Not a macro.
  return thread->set_values(form, NIL);
}

Value CL_macroexpand_1(unsigned int numargs, Value args[])
{
  Environment * env;
  switch (numargs)
    {
    case 1:
      env = new Environment();
      break;
    case 2:
      env = (args[1] != NIL) ? check_environment(args[1]) : new Environment();
      break;
    default:
      return wrong_number_of_arguments(S_macroexpand_1, numargs, 1, 2);
    }
  return macroexpand_1(args[0], env, current_thread());
}

Value macroexpand(Value form, Environment * env, Thread * thread)
{
  assert(env != NULL);
  Value expanded = NIL;
  while (true)
    {
      form = macroexpand_1(form, env, thread);
      assert(thread->values_length() == 2);
      Value * values = thread->values();
      if (values[1] == NIL)
        {
          values[1] = expanded;
          return form;
        }
      expanded = T;
    }
}

// ### macroexpand form &optional env => expansion, expanded-p
Value CL_macroexpand(unsigned int numargs, Value args[])
{
  Environment * env;
  switch (numargs)
    {
    case 1:
      env = new Environment();
      break;
    case 2:
      env = (args[1] != NIL) ? check_environment(args[1]) : new Environment();
      break;
    default:
      return wrong_number_of_arguments(S_macroexpand, numargs, 1, 2);
    }
  return macroexpand(args[0], env, current_thread());
}

// ### eval
Value CL_eval(Value form)
{
  return eval(form, new Environment(), current_thread());
}

Value eval(Value form, Environment * env, Thread * thread)
{
 top:
  thread->clear_values();
  if (symbolp(form))
    {
      Symbol * sym = the_symbol(form);
      Value value;
      if (sym->is_constant())
        value = sym->value();
      else if (sym->is_special_variable() || env->is_declared_special(form))
        value = thread->lookup_special(form);
      else
        {
          value = env->lookup(form);
          if (value == NULL_VALUE)
            value = thread->lookup_special(form);
        }
      if (value == NULL_VALUE)
        {
          value = sym->value();
          if (value == NULL_VALUE)
            return signal_lisp_error(new UnboundVariable(form));
        }
      if (symbol_macro_p(value))
        {
          form = the_symbol_macro(value)->expansion();
          goto top;
        }
      return value;
    }
  if (consp(form))
    {
      Value op = xcar(form);
      if (symbolp(op))
        {
          Symbol * sym = the_symbol(op);
          if (sym->is_special_operator())
            {
              TypedObject * function = sym->function();
              SpecialOperatorFunction pf = ((SpecialOperator *)function)->pf();
              return (*pf)(xcdr(form), env, thread);
            }
          if (sym->is_autoload())
            {
              Autoload * autoload = (Autoload *) sym->function();
              autoload->load();
              goto top;
            }
          Function * function = (Function *) env->lookup_function(op);
          if (function)
            {
              if (function->widetag() == WIDETAG_MACRO)
                {
                  form = macroexpand(form, env, thread);
                  goto top;
                }
              else
                return eval_call(function, xcdr(form), env, thread);
            }
          else
            return signal_undefined_function(op);
        }
      if (consp(op) && xcar(op) == S_lambda)
        return eval_call(new Closure(op, env), xcdr(form), env, thread);
      String * message = new String("Illegal function object: ");
      message->append(::prin1_to_string(op));
      return signal_lisp_error(new ProgramError(message));
    }
  return form;
}

Value eval_call(Function * function, Value args, Environment * env, Thread * thread)
{
  // args must be a list
  check_list(args);
  // args must be a proper list
  INDEX numargs = length(args);
  const int arity = function->arity();
  if (arity >= 0 && arity <= 6)
    {
      // fixed arity
      if (args == NIL)
        return thread->execute(function);
      Value arg1 = eval(car(args), env, thread);
      args = xcdr(args);
      if (args == NIL)
        return thread->execute(function, arg1);
      Value arg2 = eval(car(args), env, thread);
      args = xcdr(args);
      if (args == NIL)
        return thread->execute(function, arg1, arg2);
      Value arg3 = eval(car(args), env, thread);
      args = xcdr(args);
      if (args == NIL)
        return thread->execute(function, arg1, arg2, arg3);
      Value arg4 = eval(car(args), env, thread);
      args = xcdr(args);
      if (args == NIL)
        return thread->execute(function, arg1, arg2, arg3, arg4);
      Value arg5 = eval(car(args), env, thread);
      args = xcdr(args);
      if (args == NIL)
        return thread->execute(function, arg1, arg2, arg3, arg4, arg5);
      Value arg6 = eval(car(args), env, thread);
      args = xcdr(args);
      if (args == NIL)
        return thread->execute(function, arg1, arg2, arg3, arg4, arg5, arg6);
      return wrong_number_of_arguments(make_value(function), length(args) + 6,
                                       function->minargs(), function->maxargs());
    }
  else
    {
      // arity < 0 || arity > 6
      Value * vals = (Value *) alloca(numargs * sizeof(Value));
      for (INDEX i = 0; i < numargs; i++)
        {
          vals[i] = eval(xcar(args), env, thread);
          args = xcdr(args);
        }
      Value result = thread->execute(function, numargs, vals);
      return result;
    }
}
