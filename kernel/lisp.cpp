// lisp.cpp
//
// Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

#include <stdlib.h>     // exit()

#define MAIN

#include "lisp.hpp"
#include "Frame.hpp"
#include "Environment.hpp"
#include "runtime.h"
#include "primitives.hpp"
#include "Closure.hpp"
#include "ControlError.hpp"
#include "Macro.hpp"
#include "Package.hpp"
#include "Primitive.hpp"
#include "SymbolMacro.hpp"
#include "TypeError.hpp"
#include "UndefinedFunction.hpp"
#include "ProgramError.hpp"
#include "UnboundVariable.hpp"
#include "WrongNumberOfArgumentsError.hpp"

Value T;
Value NIL;

Frame * primordial_frame;

// ### backtrace
Value EXT_backtrace()
{
  Thread * const thread = current_thread();
  Value error_output = thread->symbol_value(S_error_output);
  if (!streamp(error_output))
    {
      // restore sanity
      error_output = make_value(ERROR_OUTPUT);
      thread->set_symbol_value(S_error_output, error_output);
    }
  Stream * err = the_stream(error_output);
//   Value backtrace = thread->backtrace_as_list(0);
  Value backtrace = thread->symbol_value(S_saved_backtrace);
  while (backtrace != NIL)
    {
      err->write_string(prin1_to_string(car(backtrace)));
      err->terpri();
      err->finish_output();
      backtrace = xcdr(backtrace);
    }
  return thread->set_values();
}

// ### interactive-eval
Value SYS_interactive_eval(Value form)
{
  Thread * const thread = current_thread();
  thread->set_symbol_value(S_minus, form);
  Value result =
    thread->execute(reinterpret_cast<Function *>(the_symbol(S_eval)->function()),
                    form);
  thread->set_symbol_value(S_star_star_star,
                           thread->symbol_value(S_star_star));
  thread->set_symbol_value(S_star_star,
                           thread->symbol_value(S_star));
  thread->set_symbol_value(S_star, result);
  thread->set_symbol_value(S_plus_plus_plus,
                           thread->symbol_value(S_plus_plus));
  thread->set_symbol_value(S_plus_plus,
                           thread->symbol_value(S_plus));
  thread->set_symbol_value(S_plus,
                           thread->symbol_value(S_minus));
  thread->set_symbol_value(S_slash_slash_slash,
                           thread->symbol_value(S_slash_slash));
  thread->set_symbol_value(S_slash_slash,
                           thread->symbol_value(S_slash));
  if (thread->values_length() >= 0)
    {
      Value slash = NIL;
      for (long i = thread->values_length(); i-- > 0;)
        slash = make_cons(thread->nth_value(i), slash);
      thread->set_symbol_value(S_slash, slash);
    }
  else
    thread->set_symbol_value(S_slash, make_cons(result));
  return result;
}

// ### reset
Value EXT_reset()
{
  RT_unwind_to(primordial_frame, current_thread());
  longjmp(*primordial_frame->jmp(), 1);
}

// ### quote
Value CL_quote(Value args, Environment * env, Thread * thread)
{
  return car(args);
}

// ### quoted-form-p
Value SYS_quoted_form_p(Value form)
{
  if (consp(form) && (xcar(form) == S_quote))
    {
      Value cdr = xcdr(form);
      if (consp(cdr) && xcdr(cdr) == NIL)
        return T;
    }
  return NIL;
}

// ### %make-list
Value SYS_make_list_internal(Value arg1, Value arg2)
{
  long size = fixnum_value(arg1);
  if (size < 0)
    return signal_type_error(arg1, list3(S_integer, make_fixnum(0),
                                         make_fixnum(MOST_POSITIVE_FIXNUM)));
  Value result = NIL;
  for (long i = size; i-- > 0;)
    result = make_cons(arg2, result);
  return result;
}

// ### gc
Value EXT_gc()
{
  GC_gcollect();
  return NIL;
}

// ### heap-size
Value SYS_heap_size()
{
  return make_number(GC_get_heap_size());
}

// ### heap-free
Value SYS_heap_free()
{
  return make_number(GC_get_free_bytes());
}

// ### heap-used
Value SYS_heap_used()
{
  return make_number(GC_get_heap_size() - GC_get_free_bytes());
}

// ### gc-total-bytes
Value SYS_gc_total_bytes()
{
  unsigned long n = GC_get_total_bytes();
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  return make_value(new Bignum(n));
}

// ### gc-total-cons-cells
Value SYS_gc_total_cons_cells()
{
  unsigned long n = Cons::get_count();
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  return make_value(new Bignum(n));
}

// ### room &optional x => implementation-dependent
Value CL_room(unsigned int numargs, Value args[])
{
  long heap_size = GC_get_heap_size();
  long free_bytes = GC_get_free_bytes();
  char buf[1024];
  SNPRINTF(buf, sizeof(buf),
           "\nHeap used:  %10lu bytes\nHeap free:  %10lu bytes\nTotal heap: %10lu bytes\n\n%lu bytes allocated since last GC\n",
           heap_size - free_bytes,
           free_bytes,
           heap_size,
           (long) GC_get_bytes_since_gc());
  check_stream(current_thread()->symbol_value(S_standard_output))->write_string(buf);
  // If we change this to return multiple values (or no values), remember to
  // update known-functions.lisp!
  return NIL;
}

// ### %inspected-parts arg => description, named-p, elements
Value SYS_xinspected_parts(Value arg)
{
  if (typed_object_p(arg))
    return the_typed_object(arg)->parts();
  else
    return current_thread()->set_values(NIL, NIL, NIL);
}

// ### quit
Value EXT_quit()
{
  exit(0);
  // not reached
  return NIL;
}

// ### exit
Value EXT_exit()
{
  exit(0);
  // not reached
  return NIL;
}

// ### getenv
Value EXT_getenv(Value arg)
{
  char * s = getenv(check_string(arg)->as_c_string());
  if (s)
    return make_simple_string(s);
  else
    return NIL;
}

// ### type-of
Value CL_type_of(Value value)
{
  long tag = lowtag_of(value);
  switch (tag)
    {
    case LOWTAG_EVEN_FIXNUM:
    case LOWTAG_ODD_FIXNUM:
      {
        long n = xlong(value);
        if (n >= 0)
          {
            if (n == 0 || n == 1)
              return S_bit;
            else
              return list3(S_integer, FIXNUM_ZERO, make_fixnum(MOST_POSITIVE_FIXNUM));
          }
        else
          return S_fixnum;
      }
    case LOWTAG_LIST:
      return (value == NIL) ? S_null : S_cons;
    case LOWTAG_TYPED_OBJECT:
      return the_typed_object(value)->type_of();
    case LOWTAG_CHARACTER:
      if (standard_char_p(value))
        return S_standard_char;
      else
        return S_character;
    case LOWTAG_SYMBOL:
      if (value == T)
        return S_boolean;
      if (the_symbol(value)->package() == make_value(PACKAGE_KEYWORD))
        return S_keyword;
      return S_symbol;
    default:
      printf("unknown lowtag %ld\n", tag);
      return T;
    }
}

// ### identity
Value CL_identity(Value arg)
{
  return arg;
}

// ### cons
Value CL_cons(Value arg1, Value arg2)
{
  assert(arg1 != NULL_VALUE);
  assert(arg2 != NULL_VALUE);
  return make_cons(arg1, arg2);
}

// ### consp
Value CL_consp(Value arg)
{
  return consp(arg) ? T : NIL;
}

// ### fixnump
Value EXT_fixnump(Value arg)
{
  return fixnump(arg) ? T : NIL;
}

// ### fixnum-typep object low high => boolean
// bounds are inclusive
Value SYS_fixnum_typep(Value object, Value low, Value high)
{
  // LOW and HIGH must be fixnums!
  return (fixnump(object) && object >= low && object <= high) ? T : NIL;
}

// ### bignump
Value EXT_bignump(Value arg)
{
  return bignump(arg) ? T : NIL;
}

// ### sequencep
Value EXT_sequencep(Value arg)
{
  if (listp(arg) || vectorp(arg))
    return T;
  else
    return NIL;
}

// ### listp
Value CL_listp(Value arg)
{
  return listp(arg) ? T : NIL;
}

// ### stringp
Value CL_stringp(Value arg)
{
  return stringp(arg) ? T : NIL;
}

// ### not
Value CL_not(Value arg)
{
  return null(arg) ? T : NIL;
}

// ### null
Value CL_null(Value arg)
{
  return null(arg) ? T : NIL;
}

// ### values &rest objects => multiple values
Value CL_values(unsigned int numargs, Value args[])
{
  if (numargs <= MULTIPLE_VALUES_LIMIT)
    return current_thread()->set_values(numargs, args);
  else
    return signal_lisp_error(new Error("Too many values."));
}

inline INDEX inline_length(Value arg)
{
  if (arg == NIL)
    return 0;
  if (consp(arg))
    {
      INDEX length = 0;
      while (consp(arg))
        {
          ++length;
          arg = xcdr(arg);
        }
      if (arg == NIL)
        return length;
      // not CONSP, not NIL...
      return signal_type_error(arg, S_list);
    }
  if (vectorp(arg))
    return the_vector(arg)->length();
  signal_type_error(arg, S_sequence);
  // not reached
  return 0;
}

INDEX length(Value arg)
{
  return inline_length(arg);
}

// ### length
Value CL_length(Value arg)
{
  assert(arg != NULL_VALUE);
  return make_fixnum(inline_length(arg));
}

// ### vector-length
Value SYS_vector_length(Value arg)
{
  return make_fixnum(check_vector(arg)->length());
}

// ### %vector-length
Value SYS_vector_length_internal(Value arg)
{
  return make_fixnum(the_vector(arg)->length());
}

// ### %list-length
Value SYS_xlist_length(Value arg)
{
  INDEX length = 0;
  while (consp(arg))
    {
      ++length;
      arg = xcdr(arg);
    }
  if (arg == NIL)
    return make_fixnum(length);
  // not CONSP, not NIL...
  return signal_type_error(arg, S_list);
}

// ### length-eql sequence n
Value SYS_length_eql(Value arg1, Value arg2)
{
  long n = fixnum_value(arg2);
  if (n < 0)
    return NIL;
  if (arg1 == NIL)
    return (n == 0) ? T : NIL;
  if (consp(arg1))
    {
      INDEX length = 0;
      while (consp(arg1))
        {
          ++length;
          if (length > (INDEX) n)
            return NIL;
          arg1 = xcdr(arg1);
        }
      if (arg1 == NIL)
        return (length == (INDEX) n) ? T : NIL;
      // not consp, not NIL...
      return signal_type_error(arg1, S_list);
    }
  if (vectorp(arg1))
    return (the_vector(arg1)->length() == (INDEX) n) ? T : NIL;
  return signal_type_error(arg1, S_sequence);
}

/// ### vectorp
Value CL_vectorp(Value arg)
{
  return vectorp(arg) ? T : NIL;
}

// ### eq
Value CL_eq(Value arg1, Value arg2)
{
  return arg1 == arg2 ? T : NIL;
}

// ### neq
Value EXT_neq(Value arg1, Value arg2)
{
  return arg1 != arg2 ? T : NIL;
}

// ### eql
Value CL_eql(Value arg1, Value arg2)
{
  return eql(arg1, arg2) ? T : NIL;
}

// ### special-operator-p symbol => generalized-boolean
Value CL_special_operator_p(Value arg)
{
  return check_symbol(arg)->is_special_operator() ? T : NIL;
}

// ### special-variable-p
Value EXT_special_variable_p(Value arg)
{
  return check_symbol(arg)->is_special_variable() ? T : NIL;
}

Value progn(Value body, Environment * env, Thread * thread)
{
  Value result = NIL;
  while (body != NIL)
    {
      result = eval(car(body), env, thread);
      body = xcdr(body);
    }
  return result;
}

// ### progn
Value CL_progn(Value args, Environment * env, Thread * thread)
{
  Value result = NIL;
  while (args != NIL)
    {
      result = eval(car(args), env, thread);
      args = xcdr(args);
    }
  return result;
}

// ### multiple-value-prog1 values-form &rest forms => values-form-results
Value CL_multiple_value_prog1(Value args, Environment * env, Thread * thread)
{
  if (length(args) == 0)
    return wrong_number_of_arguments(S_multiple_value_prog1, length(args), 1, MANY);
  Value saved_values[MULTIPLE_VALUES_LIMIT];
  Value result = eval(car(args), env, thread);
  const long values_length = thread->values_length();
  if (values_length >= 0)
    {
      Value * values = thread->values();
      assert(values != NULL);
      for (long i = 0; i < values_length; i++)
        saved_values[i] = values[i];
    }
  while ((args = xcdr(args)) != NIL)
    eval(car(args), env, thread);
  thread->set_values(values_length, saved_values);
  return result;
}

// ### load-time-value form &optional read-only-p => object
Value CL_load_time_value(Value args, Environment * env, Thread * thread)
{
  switch (length(args))
    {
    case 1:
    case 2:
      return eval(car(args), new Environment(), thread);
    default:
      return wrong_number_of_arguments(S_load_time_value, length(args), 1, 2);
    }
}

// ### setq
Value CL_setq(Value args, Environment * env, Thread * thread)
{
  Value value = NIL;
  while (args != NIL)
    {
      Value arg1 = car(args);
      args = xcdr(args);
      if (args == NIL)
        return signal_lisp_error(new ProgramError("Odd number of arguments for SETQ."));
      Symbol * sym = check_symbol(arg1);
      if (sym->is_constant())
        {
          String * s = new String(sym->prin1_to_string());
          s->append(" is a constant and may not be set.");
          return signal_lisp_error(new ProgramError(s));
        }
      Value arg2 = car(args);
      args = xcdr(args);
      if (sym->is_special_variable() || env->is_declared_special(arg1))
        {
          value = eval(arg2, env, thread);
          thread->set_symbol_value(arg1, value);
        }
      else
        {
          Binding * binding = env->get_binding(arg1);
          if (binding)
            {
              if (symbol_macro_p(binding->value()))
                {
                  Value expansion = the_symbol_macro(binding->value())->expansion();
                  Value form = list3(S_setf, expansion, arg2);
                  value = eval(form, env, thread);
                }
              else
                {
                  value = eval(arg2, env, thread);
                  binding->set_value(value);
                }
            }
          else
            {
              value = eval(arg2, env, thread);
              sym->set_value(value);
            }
        }
    }
  // return the primary value of the last form
  thread->clear_values();
  return value;
}

// ### incq symbol number => result
Value SYS_incq(Value args, Environment * env, Thread * thread)
{
  assert(length(args) == 2);
  Value var = xcar(args);
  assert(symbolp(var));
  Value delta = xcar(xcdr(args));
  assert(numberp(delta));
  Symbol * sym = the_symbol(var);
  if (sym->is_special_variable() || env->is_declared_special(var))
    {
      Value value = thread->symbol_value(var);
      if (value == NULL_VALUE)
        return signal_lisp_error(new UnboundVariable(var));
      value = SYS_add_2(value, delta);
      thread->set_symbol_value(var, value);
      return value;
    }
  else
    {
      Binding * binding = env->get_binding(var);
      Value value;
      if (binding)
        {
          value = SYS_add_2(binding->value(), delta);
          binding->set_value(value);
        }
      else
        {
          value = sym->value();
          if (value == NULL_VALUE)
            return signal_lisp_error(new UnboundVariable(var));
          value = SYS_add_2(value, delta);
          sym->set_value(value);
        }
      return value;
    }
}

// ### lambda
Value CL_lambda(Value args, Environment * env, Thread * thread)
{
  Value lambda_expression = make_cons(S_lambda, args);
  return make_value(new Closure(lambda_expression, env));
}

// ### function
Value CL_function(Value args, Environment * env, Thread * thread)
{
  const unsigned long numargs = length(args);
  if (numargs != 1)
    return wrong_number_of_arguments(S_function, numargs, 1, 1);
  Value arg = xcar(args);
  if (symbolp(arg))
    {
      TypedObject * function = env->lookup_function(arg);
      if (function && function->widetag() == WIDETAG_AUTOLOAD)
        {
          reinterpret_cast<Autoload *>(function)->load();
          function = the_symbol(arg)->function();
        }
      if (function)
        return make_value(function);
      else
        return signal_lisp_error(new UndefinedFunction(arg));
    }
  if (consp(arg))
    {
      if (xcar(arg) == S_lambda)
        return make_value(new Closure(arg, env));
      if (is_valid_setf_function_name(arg))
        {
          TypedObject * function = env->lookup_function(arg);
          if (function)
            return make_value(function);
          Value value = the_symbol(CL_cadr(arg))->get(S_setf_function);
          if (value != NIL)
            return value;
        }
    }
  return signal_type_error(arg, S_symbol);
}

// ### multiple-value-list
Value CL_multiple_value_list(Value args, Environment * env, Thread * thread)
{
  if (length(args) != 1)
    return wrong_number_of_arguments(S_multiple_value_list, length(args), 1, 1);
  Value result = eval(xcar(args), env, thread);
  long values_length = thread->values_length();
  thread->set_values_length(-1);
  if (values_length < 0)
    return make_cons(result);
  Value * values = thread->values();
  Value list = NIL;
  for (long i = values_length; i-- > 0;)
    list = make_cons(values[i], list);
  return list;
}

Value parse_body(Value forms, bool doc_string_allowed, Thread * thread)
{
  Value decls = NIL;
  Value doc = NIL;
  Value tail = forms;
  while (tail != NIL)
    {
      Value form = car(tail);
      if (stringp(form) && xcdr(tail) != NIL)
        {
          if (doc_string_allowed)
            {
              doc = form;
              // only one doc string is allowed
              doc_string_allowed = false;
            }
          else
            break;
        }
      else if (consp(form) && xcar(form) == S_declare)
        decls = make_cons(form, decls);
      else
        break;
      tail = xcdr(tail);
    }
  return thread->set_values(tail, CL_nreverse(decls), doc);
}

// ### parse-body forms &optional (doc-string-allowed t) => body, decls, doc
Value SYS_parse_body(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_parse_body, numargs, 1, 2);
  Value forms = args[0];
  bool doc_string_allowed;
  if (numargs == 2)
    doc_string_allowed = (args[1] != NIL);
  else
    doc_string_allowed = true;
  return parse_body(forms, doc_string_allowed, current_thread());
}

// ### defun
Value CL_defun(Value args, Environment * env, Thread * thread)
{
  INDEX numargs = length(args);
  if (numargs < 2)
    return wrong_number_of_arguments(S_defun, numargs, 2, MANY);
  Value name = xcar(args);
  Value lambda_list = CL_cadr(args);
  Value body = CL_cddr(args);

  body = parse_body(body, true, thread);
  assert(thread->values_length() == 3);
  Value decls = thread->nth_value(1);
  Value doc = thread->nth_value(2);
  thread->clear_values();

  body = make_cons(SYS_fdefinition_block_name(name), body);
  body = make_cons(S_block, body);
  body = list1(body);

  Closure * closure = new Closure(name, lambda_list, body, decls, doc, env);

  if (symbolp(name))
    the_symbol(name)->set_function(closure);
  else if (is_valid_setf_function_name(name))
    check_symbol(CL_cadr(name))->put(S_setf_function, make_value(closure));
  closure->set_operator_name(name);
  SYS_record_source_information(name);
  return name;
}

// ### %defun name lambda-expression => name
Value SYS_defun_internal(Value name, Value function)
{
  SYS_set_fdefinition(name, function);
  return name;
}

// ### defmacro
Value CL_defmacro(Value args, Environment * env, Thread * thread)
{
  Value name_arg = car(args);
  Symbol * symbol = check_symbol(name_arg);
  Value lambda_list = check_list(CL_cadr(args));
  Value body = CL_cddr(args);
  Value block = make_cons(S_block, make_cons(name_arg, body));
  Value to_be_applied =
    make_value(new Closure(list3(S_lambda, lambda_list, block), env));
  Value form_arg = gensym(thread);
  Value env_arg = gensym(thread); // ignored
  Value expander =
    list3(S_lambda, list2(form_arg, env_arg),
          list3(S_apply, to_be_applied,
                list2(S_cdr, form_arg)));
  Closure * expansion_function =
    new Closure(list2(S_macro_function, name_arg), expander, env);
  symbol->set_macro_function(expansion_function);
  SYS_record_source_information(name_arg);
  return name_arg;
}

// ### macrolet
Value CL_macrolet(Value args, Environment * env, Thread * thread)
{
  Value defs = check_list(car(args));
  Value body = check_list(xcdr(args));
  Environment * ext = new Environment(env);
  while (defs != NIL)
    {
      Value def = check_list(car(defs));
      Symbol * name = check_symbol(car(def));
      Value expander =
        thread->execute(reinterpret_cast<Function *>(the_symbol(S_make_expander_for_macrolet)->function()),
                        def);
      Closure * closure = new Closure(expander, env);
      ext->add_local_function(make_value(name),
                              make_value(new Macro(make_value(name), closure)));
      defs = xcdr(defs);
    }
  // process declarations
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
  while (specials != NIL)
    {
      ext->declare_special(xcar(specials));
      specials = xcdr(specials);
    }
  // implicit PROGN
  return progn(body, ext, thread);
}

static Value flet_internal(Value args, Environment * env, Thread * thread, bool recursive)
{
  // first argument is a list of local function definitions
  Value defs = check_list(car(args));
  Value result;
  if (defs != NIL)
    {
      void * last_special_binding = thread->last_special_binding();
      Environment * ext = new Environment(env);
      while (defs != NIL)
        {
          Value def = check_list(car(defs));
          Value name = car(def);
          Symbol * symbol;
          if (symbolp(name))
            {
              symbol = check_symbol(name);
              if (symbol->is_special_operator())
                {
                  String * message = new String(symbol->prin1_to_string());
                  message->append(" is a special operator and may not be redefined.");
                  return signal_lisp_error(new ProgramError(message));
                }
            }
          else if (is_valid_setf_function_name(name))
            symbol = check_symbol(CL_cadr(name));
          else
            return signal_type_error(name, FUNCTION_NAME);
          Value rest = xcdr(def);
          Value parameters = car(rest);
          Value body = parse_body(xcdr(rest), true, thread);
          Value decls = thread->nth_value(1);
          body = make_cons(SYS_fdefinition_block_name(name), body);
          body = make_cons(S_block, body);
          body = make_cons(body, NIL);
          // FIXME don't ignore special declarations!
          while (decls != NIL)
            {
              body = make_cons(xcar(decls), body);
              decls = xcdr(decls);
            }
          Value lambda_expression =
            make_cons(S_lambda, make_cons(parameters, body));
          Value lambda_name =
            list2(recursive ? S_labels : S_flet, name);
          Closure * closure =
            new Closure(lambda_name, lambda_expression,
                        recursive ? ext : env);
          ext->add_local_function(name, make_value(closure));
          defs = xcdr(defs);
        }
      result = CL_locally(xcdr(args), ext, thread);
      thread->set_last_special_binding(last_special_binding);
    }
  else
    result = CL_locally(xcdr(args), env, thread);
  return result;
}

// ### flet
Value CL_flet(Value args, Environment * env, Thread * thread)
{
  return flet_internal(args, env, thread, false);
}

// ### labels
Value CL_labels(Value args, Environment * env, Thread * thread)
{
  return flet_internal(args, env, thread, true);
}

// ### aver
Value EXT_aver(Value args, Environment * env, Thread * thread)
{
  if (length(args) < 1)
    return wrong_number_of_arguments(S_aver, length(args), 1, MANY);
  if (eval(xcar(args), env, thread) == NIL)
    {
      // Assertion failed.
      String * string = new String("Failed AVER: ");
      string->append(::prin1_to_string(xcar(args)));
      return signal_lisp_error(string);
    }
  return NIL;
}

// ### cond
Value CL_cond(Value args, Environment * env, Thread * thread)
{
  Value result = NIL;
  while (args != NIL)
    {
      Value clause = car(args);
      result = eval(car(clause), env, thread);
      thread->clear_values();
      if (result != NIL)
        {
          Value body = xcdr(clause);
          while (body != NIL)
            {
              result = eval(car(body), env, thread);
              body = xcdr(body);
            }
          return result;
        }
      args = xcdr(args);
    }
  return result;
}

// ### case
Value CL_case(Value args, Environment * env, Thread * thread)
{
  Value key = eval(car(args), env, thread);
  Value clauses = xcdr(args);
  while (clauses != NIL)
    {
      Value clause = car(clauses);
      Value keys = car(clause);
      bool match = false;
      if (listp(keys))
        {
          while (keys != NIL)
            {
              Value candidate = car(keys);
              if (eql(key, candidate))
                {
                  match = true;
                  break;
                }
              keys = cdr(keys);
            }
        }
      else
        {
          Value candidate = keys;
          if (candidate == T || candidate == S_otherwise)
            match = true;
          else if (eql(key, candidate))
            match = true;
        }
      if (match)
        return progn(cdr(clause), env, thread);
      clauses = xcdr(clauses);
    }
  return NIL;
}

// ### ecase
Value CL_ecase(Value args, Environment * env, Thread * thread)
{
  Value key = eval(car(args), env, thread);
  Value clauses = xcdr(args);
  while (clauses != NIL)
    {
      Value clause = car(clauses);
      Value keys = car(clause);
      bool match = false;
      if (listp(keys))
        {
          while (keys != NIL)
            {
              Value candidate = car(keys);
              if (eql(key, candidate))
                {
                  match = true;
                  break;
                }
              keys = xcdr(keys);
            }
        }
      else
        {
          if (eql(key, keys))
            match = true;
        }
      if (match)
        return progn(cdr(clause), env, thread);
      clauses = xcdr(clauses);
    }
  // No match.
  Value expected_type = NIL;
  clauses = xcdr(args);
  while (clauses != NIL)
    {
      Value clause = car(clauses);
      Value keys = car(clause);
      if (listp(keys))
        {
          while (keys != NIL)
            {
              expected_type = make_cons(car(keys), expected_type);
              keys = xcdr(keys);
            }
        }
      else
        expected_type = make_cons(keys, expected_type);
      clauses = xcdr(clauses);
    }
  expected_type = CL_nreverse(expected_type);
  expected_type = make_cons(S_member, expected_type);
  return signal_type_error(key, expected_type);
}

// ### if
Value CL_if(Value args, Environment * env, Thread * thread)
{
  switch (length(args))
    {
    case 2:
      {
        if (eval(xcar(args), env, thread) != NIL)
          return eval(xcadr(args), env, thread);
        thread->clear_values();
        return NIL;
      }
    case 3:
      {
        if (eval(xcar(args), env, thread) != NIL)
          return eval(xcadr(args), env, thread);
        return eval(CL_caddr(args), env, thread);
      }
    default:
      return wrong_number_of_arguments(S_if, length(args), 2, 3);
    }
}

Value CL_when(Value args, Environment * env, Thread * thread)
{
  if (args == NIL)
    return wrong_number_of_arguments(S_when, 0, 1, MANY);
  Value value = eval(car(args), env, thread);
  thread->clear_values();
  if (value != NIL)
    {
      args = xcdr(args);
      Value result = NIL;
      while (args != NIL)
        {
          result = eval(car(args), env, thread);
          args = xcdr(args);
        }
      return result;
    }
  return NIL;

}

Value CL_unless(Value args, Environment * env, Thread * thread)
{
  if (args == NIL)
    return wrong_number_of_arguments(S_unless, 0, 1, MANY);
  Value value = eval(car(args), env, thread);
  thread->clear_values();
  if (value == NIL)
    {
      args = xcdr(args);
      Value result = NIL;
      while (args != NIL)
        {
          result = eval(car(args), env, thread);
          args = xcdr(args);
        }
      return result;
    }
  return NIL;
}

// ### defvar name [initial-value [documentation]] => name
Value CL_defvar(Value args, Environment * env, Thread * thread)
{
  unsigned long numargs = length(args);
  if (numargs < 1 || numargs > 3)
    return wrong_number_of_arguments(S_defvar, numargs, 1, 3);
  Value first = xcar(args);
  Symbol * sym = check_symbol(first);
  if (numargs > 1)
    {
      // initial-value was supplied
      if (sym->value() == NULL_VALUE)
        sym->initialize_special(eval(xcar(xcdr(args)), env, thread));
    }
  else
    sym->initialize_special();
  return first;
}

// ### %defvar name => name
Value SYS_defvar_internal(Value arg)
{
  check_symbol(arg)->initialize_special();
  return arg;
}

// ### %defparameter name initial-value => name
Value SYS_defparameter_internal(Value name, Value initial_value)
{
  check_symbol(name)->initialize_special(initial_value);
  return name;
}

// ### and
// Should be a macro.
Value CL_and(Value args, Environment * env, Thread * thread)
{
  Value result = T;
  while (args != NIL)
    {
      result = eval(car(args), env, thread);
      if (result == NIL)
        {
          if (xcdr(args) != NIL)
            {
              // Not the last form.
              thread->set_values_length(-1); // REVIEW
            }
          break;
        }
      args = xcdr(args);
    }
  return result;
}

// ### or
// Should be a macro.
Value CL_or(Value args, Environment * env, Thread * thread)
{
  Value result = NIL;
  while (args != NIL)
    {
      result = eval(car(args), env, thread);
      if (result != NIL)
        {
          if (xcdr(args) != NIL)
            {
              // Not the last form.
              thread->set_values_length(-1); // REVIEW
            }
          break;
        }
      args = xcdr(args);
    }
  return result;
}

// ### eval-when
Value CL_eval_when(Value args, Environment * env, Thread * thread)
{
  Value situations = car(args);
  if (situations != NIL)
    {
      if (memq(K_execute, situations) || memq(S_eval, situations))
        return progn(xcdr(args), env, thread);
    }
  return NIL;
}

// ### the value-type form => result*
Value CL_the(Value args, Environment * env, Thread * thread)
{
  const long numargs = length(args);
  if (numargs != 2)
    return wrong_number_of_arguments(S_the, numargs, 2, 2);
  return eval(CL_cadr(args), env, thread);
}

// ### truly-the value-type form => result*
Value SYS_truly_the(Value args, Environment * env, Thread * thread)
{
  const long numargs = length(args);
  if (numargs != 2)
    return wrong_number_of_arguments(S_truly_the, numargs, 2, 2);
  return eval(CL_cadr(args), env, thread);
}

// ### int3
Value SYS_int3()
{
  asm("int3");
  return NIL;
}
