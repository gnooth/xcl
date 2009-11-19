// primitives.cpp
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

#ifndef WIN32
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
#include "primitives.hpp"
#include "Closure.hpp"
#include "Complex.hpp"
#include "Environment.hpp"
#include "HashTable.hpp"
#include "Macro.hpp"
#include "Package.hpp"
#include "SimpleArray_UB8_1.hpp"
#include "TypeError.hpp"
#include "UndefinedFunction.hpp"
#include "WrongNumberOfArgumentsError.hpp"

bool equal(Value first, Value second)
{
  if (first == second)
    return true;
  if (consp(first))
    {
      if (consp(second))
        return (equal(xcar(first), xcar(second)) &&
                equal(xcdr(first), xcdr(second)));
      else
        return false;
    }
  if (typed_object_p(first))
    return the_typed_object(first)->equal(second);
  return false;
}

// ### equal
Value CL_equal(Value first, Value second)
{
  return equal(first, second) ? T : NIL;
}

bool equalp(Value first, Value second)
{
  if (first == second)
    return true;
  if (consp(first))
    {
      if (consp(second))
        return (equalp(xcar(first), xcar(second)) &&
                equalp(xcdr(first), xcdr(second)));
      else
        return false;
    }
  if (characterp(first))
    {
      if (characterp(second))
        {
          char c1 = xchar(first);
          char c2 = xchar(second);
          if (toupper(c1) == toupper(c2))
            return true;
          if (tolower(c1) == tolower(c2))
            return true;
        }
      return false;
    }
  if (numberp(first))
    {
      if (numberp(second))
        return equals(first, second);
      else
        return false;
    }
  if (hash_table_p(first))
    {
      if (hash_table_p(second))
        {
          HashTable * ht1 = the_hash_table(first);
          HashTable * ht2 = the_hash_table(second);
          if (ht1->count() != ht2->count())
            return false;
          if (ht1->test() != ht2->test())
            return false;
          Value entries = ht1->entries();
          while (entries != NIL)
            {
              Value entry = car(entries);
              Value key = car(entry);
              Value value = cdr(entry);
              if (!equalp(value, ht2->get(key)))
                return false;
              entries = xcdr(entries);
            }
          return true;
        }
      else
        return false;
    }
  if (typed_object_p(first))
    return the_typed_object(first)->equalp(second);
  return false;
}

Value CL_equalp(Value first, Value second)
{
  return equalp(first, second) ? T : NIL;
}

// ### list-delete-eq item list
Value SYS_list_delete_eq(Value item, Value list)
{
    if (consp(list))
      {
        Value tail = list;
        Value splice = list;
        while (consp(tail))
          {
            if (xcar(tail) == item)
              {
                Value temp = xcdr(tail);
                if (temp != NIL)
                  {
                    setcar(tail, car(temp));
                    setcdr(tail, cdr(temp));
                  }
                else
                  {
                    // last item
                    if (tail == list)
                      return NIL;
                    setcdr(splice, NIL);
                    return list;
                  }
              }
            else
              {
                splice = tail;
                tail = xcdr(tail);
              }
          }
        if (tail == NIL)
          return list;
        else
          return signal_type_error(tail, S_list);
      }
    else if (list == NIL)
      return list;
    else
      return signal_type_error(list, S_list);
}

// ### list-delete-eql item list
Value SYS_list_delete_eql(Value item, Value list)
{
  if (consp(list))
    {
      Value tail = list;
      Value splice = list;
      while (consp(tail))
        {
          Value value = xcar(tail);
          if (eql(value, item))
            {
              Value temp = xcdr(tail);
              if (temp != NIL)
                {
                  setcar(tail, car(temp));
                  setcdr(tail, cdr(temp));
                }
              else
                {
                  // last item
                  if (tail == list)
                    return NIL;
                  setcdr(splice, NIL);
                  return list;
                }
            }
          else
            {
              splice = tail;
              tail = xcdr(tail);
            }
        }
      if (tail == NIL)
        return list;
      else
        return signal_type_error(tail, S_list);
    }
  else if (list == NIL)
    return list;
  else
    return signal_type_error(list, S_list);
}

Value funcall(TypedObject * function, long numargs, Value args[], Thread * thread)
{
  const int arity = function->arity();
  if (arity >= 0)
    {
      if (arity != numargs)
        return wrong_number_of_arguments(make_value(function), numargs, arity, arity);
      switch (numargs)
        {
        case 0:
          return thread->execute(function);
        case 1:
          return thread->execute(function, args[0]);
        case 2:
          return thread->execute(function, args[0], args[1]);
        case 3:
          return thread->execute(function, args[0], args[1], args[2]);
        case 4:
          return thread->execute(function, args[0], args[1], args[2], args[3]);
        case 5:
          return thread->execute(function, args[0], args[1], args[2], args[3], args[4]);
        case 6:
          return thread->execute(function, args[0], args[1], args[2], args[3], args[4], args[5]);
        }
      printf("funcall: unhandled situation\n"); // FIXME
      return NIL;
    }
  else
    return thread->execute(function, numargs, args);
}

// ### funcall
Value CL_funcall(unsigned int numargs, Value args[])
{
  if (numargs == 0)
    return wrong_number_of_arguments(S_funcall, numargs, 1, MANY);
  TypedObject * function;
  if (symbolp(args[0]))
    {
      Symbol * sym = the_symbol(args[0]);
      if (sym->is_autoload())
        {
          Autoload * autoload = reinterpret_cast<Autoload *>(sym->function());
          assert(autoload != NULL);
          autoload->load();
        }
      if (sym->is_special_operator() || sym->is_macro()
          || (function = sym->function()) == NULL)
        {
          return signal_lisp_error(new UndefinedFunction(args[0]));
        }
    }
  else if (functionp(args[0]))
    function = the_typed_object(args[0]);
  else
    return signal_type_error(args[0], S_function_designator);
  Thread *  thread = current_thread();
  thread->clear_values();
  if (function->arity() >= 0)
    {
      // fixed arity
      if (numargs - 1 != (unsigned int) function->arity())
        return wrong_number_of_arguments(args[0], numargs - 1, function->arity(), function->arity());
      switch (numargs)
        {
        case 1:
          return thread->execute(function);
        case 2:
          return thread->execute(function, args[1]);
        case 3:
          return thread->execute(function, args[1], args[2]);
        case 4:
          return thread->execute(function, args[1], args[2], args[3]);
        case 5:
          return thread->execute(function, args[1], args[2], args[3], args[4]);
        case 6:
          return thread->execute(function, args[1], args[2], args[3], args[4], args[5]);
        case 7:
          return thread->execute(function, args[1], args[2], args[3], args[4], args[5], args[6]);
        default:
          {
            Value * execute_args = &args[1];
            return thread->execute(function, numargs - 1, execute_args);
          }
        }
    }
  else
    {
      if (numargs - 1 < function->minargs()
          || (function->maxargs() >= 0 && numargs - 1 > function->maxargs()))
        return wrong_number_of_arguments(args[0], numargs, function->minargs(),
                                         function->maxargs());
      Value * execute_args = &args[1];
      return thread->execute(function, numargs - 1, execute_args);
    }
}

// ### apply function &rest args+ => result*
Value CL_apply(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
    case 1:
      return wrong_number_of_arguments(S_apply, numargs, 2, MANY);
    default:
      {
        Value last = args[numargs - 1];
        if (listp(last))
          {
            const long len = numargs - 1 + length(last);
            Value * funcall_args = new (GC) Value[len];
            long j = 0;
            for (unsigned int i = 0; i < numargs - 1; i++)
              funcall_args[j++] = args[i];
            while (last != NIL)
              {
                funcall_args[j++] = car(last);
                last = xcdr(last);
              }
            // delegate to CL_funcall()
            return funcall((Function *)the_symbol(S_funcall)->function(), len,
                           funcall_args, current_thread());
          }
        return signal_type_error(last, S_list);
      }
    }
  return NIL;
}

Value gensym(Thread * thread)
{
  return gensym("G", thread);
}

Value gensym(const char * prefix, Thread * thread)
{
  String * string = new String(prefix);
  Value old_value = thread->symbol_value(S_gensym_counter);
  Value new_value;
  if (fixnump(old_value))
    {
      long n = xlong(old_value);
      string->append_long(n);
      new_value = make_number(n + 1);
    }
  else if (bignump(old_value))
    {
      Bignum * b = the_bignum(old_value);
      string->append(b->prin1_to_string());
      new_value = b->add(1);
    }
  else
    {
      // restore sanity
      thread->set_symbol_value(S_gensym_counter, FIXNUM_ZERO);
      return signal_type_error(old_value, S_unsigned_byte);
    }
  thread->set_symbol_value(S_gensym_counter, new_value);
  return make_symbol(string);
}

// ### gensym
Value CL_gensym(unsigned int numargs, Value args[])
{
  // FIXME not thread-safe
  switch (numargs)
    {
    case 0:
      return gensym("G", current_thread());
    case 1:
      {
        if (fixnump(args[0]))
          {
            long n = xlong(args[0]);
            if (n >= 0)
              {
                String * string = new String("G");
                string->append_long(xlong(args[0]));
                return make_symbol(string);
              }
          }
        else if (bignump(args[0]))
          {
            Bignum * b = the_bignum(args[0]);
            if (!b->minusp())
              {
                String * string = new String("G");
                string->append(b->prin1_to_string());
                return make_symbol(string);
              }
          }
        else if (stringp(args[0]))
          return gensym(the_string(args[0])->as_c_string(), current_thread());

        return signal_type_error(args[0],
                                 list3(S_or, S_string, S_unsigned_byte));
      }
    default:
      return wrong_number_of_arguments(S_gensym, numargs, 0, 1);
    }
}

// ### make-macro
Value SYS_make_macro(Value name, Value expander)
{
  return make_value(new Macro(name, check_function(expander)));
}

// ### macro-function
Value CL_macro_function(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_macro_function, numargs, 1, 2);
  Symbol * sym = check_symbol(args[0]);
  if (sym->is_special_operator())
    {
      Value value = sym->get(S_macro);
      if (value != NIL)
        {
          if (autoloadp(value))
            {
              the_autoload(value)->load();
              value = sym->get(S_macro);
            }
          assert(macrop(value));
          return make_value(the_macro(value)->expansion_function());
        }
      else
        return NIL;
    }
  if (sym->is_autoload_macro())
    {
      Autoload * autoload = reinterpret_cast<Autoload *>(sym->function());
      assert(autoload != NULL);
      autoload->load();
    }
  if (numargs == 2 && args[1] != NIL)
    {
      Environment * env = check_environment(args[1]);
      TypedObject * op = env->lookup_function(args[0]);
      if (op)
        {
          if (op->widetag() == WIDETAG_MACRO)
            return make_value(reinterpret_cast<Macro *>(op)->expansion_function());
          else
            return NIL;
        }
    }
  if (sym->is_macro())
    {
      assert(sym->function());
      assert(sym->function()->widetag() == WIDETAG_MACRO);
      return make_value(reinterpret_cast<Macro *>(sym->function())->expansion_function());
    }
  else
    return NIL;
}

// ### set-macro-function
// "The consequences are undefined if ENVIRONMENT is non-NIL in a use of SETF
// of MACRO-FUNCTION."
Value SYS_set_macro_function(unsigned int numargs, Value args[])
{
  Value name;
  Value value;
  switch (numargs)
    {
    case 2:
      name = args[0];
      value = args[1];
      break;
    case 3:
      name = args[0];
      // args[1] is environment (ignored)
      value = args[2];
      break;
    default:
      return wrong_number_of_arguments(S_set_macro_function, numargs, 2, 3);
    }
  check_symbol(name)->set_macro_function(check_function(value));
  SYS_record_source_information(name);
  return value;
}

// ### symbol-function
Value CL_symbol_function(Value arg)
{
  TypedObject * op = check_symbol(arg)->function();
  if (op)
    return make_value(op);
  return signal_lisp_error(new UndefinedFunction(arg));
}

// ### set-symbol-function
Value SYS_set_symbol_function(Value name, Value function)
{
  check_symbol(name)->set_function(check_typed_object(function));
  return function;
}

// ### fdefinition
Value CL_fdefinition(Value name)
{
  Value traced_names = current_thread()->symbol_value(S_traced_names);
  if (traced_names != NIL)
    {
      // FIXME setf functions
      if (memq(name, traced_names))
        {
          extern Value RT_fast_call_symbol_1(Value symbol, Value arg);
          return RT_fast_call_symbol_1(S_untraced_function, name);
        }
    }

  if (symbolp(name))
    return CL_symbol_function(name);
  if (is_valid_setf_function_name(name))
    {
      Value value = the_symbol(CL_cadr(name))->get(S_setf_function);
      if (value != NIL)
        return value;
      else
        return signal_lisp_error(new UndefinedFunction(name));
    }
  return signal_type_error(name, FUNCTION_NAME);
}

// ### set-fdefinition
Value SYS_set_fdefinition(Value name, Value value)
{
  if (symbolp(name))
    {
      unsigned long call_count = 0;
      TypedObject * op = the_symbol(name)->function();
      if (op)
        call_count = op->call_count();
      if (functionp(value))
        the_function(value)->set_call_count(call_count);
    }

  if (symbolp(name))
    the_symbol(name)->set_function(check_typed_object(value));
  else if (is_valid_setf_function_name(name))
    the_symbol(CL_cadr(name))->put(S_setf_function, value);
  else
    return signal_type_error(name, FUNCTION_NAME);

  // REVIEW fragile
//   if (standard_generic_function_p(value))
//     the_standard_generic_function(value)->set_name(name);
//   else
  if (typep(value, S_funcallable_standard_object))
    ; // FIXME
  else
    if (functionp(value))
      the_function(value)->set_operator_name(name);

  SYS_record_source_information(name);

  extern Value RT_fast_call_symbol_2(Value symbol, Value arg1, Value arg2); // FIXME
  RT_fast_call_symbol_2(S_trace_redefined_update, name, value);

  return value;
}

// ### record-source-information
Value SYS_record_source_information(Value name)
{
  if (symbolp(name)) // FIXME support setf functions too
    {
      Thread * const thread = current_thread();
      Value source = thread->symbol_value(S_source_file);
      if (source != NIL)
        {
          Value source_position = thread->symbol_value(S_source_position);
          if (source_position != NIL)
            the_symbol(name)->put(S_source_internal, make_cons(source, source_position));
        }
    }
  return T;
}

// ### untraced-function
// redefined in trace.lisp
Value SYS_untraced_function(Value name)
{
  return NIL;
}

// ### trace-redefined-update
// redefined in trace.lisp
Value SYS_trace_redefined_update(Value name, Value function)
{
  return NIL;
}

Value CL_functionp(Value arg)
{
  return functionp(arg) ? T : NIL;
}

Value list2(Value arg1, Value arg2)
{
  return make_cons(arg1, make_cons(arg2));
}

Value list3(Value arg1, Value arg2, Value arg3)
{
  return make_cons(arg1, make_cons(arg2, make_cons(arg3)));
}

Value list4(Value arg1, Value arg2, Value arg3, Value arg4)
{
  return make_cons(arg1, make_cons(arg2, make_cons(arg3, make_cons(arg4))));
}

Value list5(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  return make_cons(arg1, make_cons(arg2, make_cons(arg3, make_cons(arg4, (make_cons(arg5))))));
}

// ### list
Value CL_list(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return NIL;
    case 1:
      return make_cons(args[0]);
    case 2:
      return make_cons(args[0], make_cons(args[1]));
    case 3:
      return make_cons(args[0], make_cons(args[1], make_cons(args[2])));
    default:
      {
        Value result = NIL;
        for (long i = numargs; i-- > 0;)
          result = make_cons(args[i], result);
        return result;
      }
    }
}

// ### list1
Value SYS_list1(Value arg)
{
  return make_cons(arg);
}

// ### list2
Value SYS_list2(Value arg1, Value arg2)
{
  return make_cons(arg1, make_cons(arg2));
}

// ### list3
Value SYS_list3(Value arg1, Value arg2, Value arg3)
{
  return make_cons(arg1, make_cons(arg2, make_cons(arg3)));
}

// ### list4
Value SYS_list4(Value arg1, Value arg2, Value arg3, Value arg4)
{
  return make_cons(arg1, make_cons(arg2, make_cons(arg3, make_cons(arg4))));
}

// ### list5
Value SYS_list5(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  return make_cons(arg1, make_cons(arg2, make_cons(arg3, make_cons(arg4, (make_cons(arg5))))));
}

// ### list*
Value CL_list_star(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_list_star, numargs, 1, MANY);
    case 1:
      return args[0];
    case 2:
      return make_cons(args[0], args[1]);
    case 3:
      return make_cons(args[0], make_cons(args[1], args[2]));
    case 4:
      return make_cons(args[0], make_cons(args[1], make_cons(args[2], args[3])));
    default:
      {
        long i = numargs - 1;
        Value result = args[i];
        while (i-- > 0)
          result = make_cons(args[i], result);
        return result;
      }
    }
}

// ### designator-list
Value SYS_designator_list(Value arg)
{
  if (listp(arg))
    return arg;
  else
    return make_cons(arg);
}

// ### boundp
Value CL_boundp(Value arg)
{
  Symbol * symbol = check_symbol(arg);
  if (current_thread()->lookup_special(arg) != NULL_VALUE)
    return T;
  return (symbol->value() != NULL_VALUE) ? T : NIL;
}

// ### makunbound
Value CL_makunbound(Value arg)
{
  if (!symbolp(arg))
    return signal_type_error(arg, S_symbol);
  current_thread()->set_symbol_value(arg, NULL_VALUE);
  return arg;
}

// ### fboundp
Value CL_fboundp(Value name)
{
  if (symbolp(name))
    return the_symbol(name)->function() ? T : NIL;
  if (is_valid_setf_function_name(name))
    return (the_symbol(CL_cadr(name))->get(S_setf_function) == NIL) ? NIL : T;
  return signal_type_error(name, FUNCTION_NAME);
}

// ### fmakunbound
Value CL_fmakunbound(Value name)
{
  if (symbolp(name))
    {
      the_symbol(name)->fmakunbound();
      return name;
    }
  else if (is_valid_setf_function_name(name))
    {
      the_symbol(CL_cadr(name))->remprop(S_setf_function);
      return name;
    }
  else
    return signal_type_error(name, FUNCTION_NAME);
}

// ### two-arg-append
Value SYS_two_arg_append(Value arg1, Value arg2)
{
  if (arg1 == NIL)
    return arg2;
  // APPEND is required to copy its first argument
  Value result = make_cons(car(arg1));
  Value splice = result;
  arg1 = xcdr(arg1);
  while (arg1 != NIL)
    {
      Value temp = make_cons(car(arg1));
      setcdr(splice, temp);
      splice = temp;
      arg1 = xcdr(arg1);
    }
  setcdr(splice, arg2);
  return result;
}

// ### append
Value CL_append(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return NIL;
    case 1:
      return args[0];
    case 2:
      return SYS_two_arg_append(args[0], args[1]);
    default:
      {
        Value result = NULL_VALUE;
        Value splice = NULL_VALUE;
        const long limit = numargs - 1;
        long i;
        for (i = 0; i < limit; i++)
          {
            Value top = args[i];
            if (top == NIL)
              continue;
            result = make_cons(car(top));
            splice = result;
            top = cdr(top);
            while (top != NIL)
              {
                Value temp = make_cons(car(top));
                setcdr(splice, temp);
                splice = temp;
                top = cdr(top);
              }
            break;
          }
        if (result == NULL_VALUE)
          return args[i];
        for (++i; i < limit; i++)
          {
            Value top = args[i];
            while (top != NIL)
              {
                Value temp = make_cons(car(top));
                setcdr(splice, temp);
                splice = temp;
                top = cdr(top);
              }
          }
        setcdr(splice, args[i]);
        return result;
      }
    }
}

// ### nconc
Value CL_nconc(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return NIL;
    case 1:
      return args[0];
    default:
      {
        Value result = NULL_VALUE;
        Value splice = NULL_VALUE;
        const unsigned long limit = numargs - 1;
        unsigned long i;
        for (i = 0; i < limit; i++)
          {
            Value list = args[i];
            if (list == NIL)
              continue;
            if (consp(list))
              {
                if (splice != NULL_VALUE)
                  {
                    setcdr(splice, list);
                    splice = list;
                  }
                while (consp(list))
                  {
                    if (result == NULL_VALUE)
                      {
                        result = list;
                        splice = result;
                      }
                    else
                      splice = list;
                    list = cdr(splice);
                  }
              }
            else
              return signal_type_error(list, S_list);
          }
        if (result == NULL_VALUE)
          return args[i];
        setcdr(splice, args[i]);
        return result;
      }
    }
}

// ### atom
Value CL_atom(Value arg)
{
  return consp(arg) ? NIL : T;
}

// ### make-symbol
Value CL_make_symbol(Value arg)
{
  AbstractString * s = check_string(arg);
  if (s->length() == 0) // possibly a nil vector
    return make_symbol(new_simple_string(""));
  else
    return make_symbol(s);
}

// ### copy-list
Value CL_copy_list(Value arg)
{
  if (arg == NIL)
    return NIL;
  Value result = make_cons(car(arg));
  Value splice = result;
  arg = cdr(arg);
  while (consp(arg))
    {
      Value temp = make_cons(xcar(arg));
      setcdr(splice, temp);
      splice = temp;
      arg = xcdr(arg);
    }
  setcdr(splice, arg);
  return result;
}

// ### nreconc
Value CL_nreconc(Value list, Value tail)
{
  if (consp(list))
    {
      Value list3 = cdr(list);
      if (consp(list3))
        {
          if (consp(cdr(list3)))
            {
              Value list1 = list3;
              Value list2 = NIL;
              do
                {
                  Value h = cdr(list3);
                  setcdr(list3, list2);
                  list2 = list3;
                  list3 = h;
                }
              while (consp(cdr(list3)));
              setcdr(list, list2);
              setcdr(list1, list3);
            }
          Value h = car(list);
          setcar(list, car(list3));
          setcar(list3, h);
          setcdr(list3, tail);
        }
      else if (list3 == NIL)
          setcdr(list, tail);
      else
        return signal_type_error(list3, S_list);
      return list;
    }
  else if (list == NIL)
    return tail;
  else
    return signal_type_error(list, S_list);
}

// ### reverse
Value CL_reverse(Value arg)
{
  if (consp(arg))
    return the_cons(arg)->reverse();
  if (vectorp(arg))
    return the_vector(arg)->reverse();
  if (arg == NIL)
    return NIL;
  return signal_type_error(arg, S_sequence);
}

// ### nreverse
Value CL_nreverse(Value arg)
{
  if (consp(arg))
    return the_cons(arg)->nreverse();
  if (vectorp(arg))
    return the_vector(arg)->nreverse();
  if (arg == NIL)
    return NIL;
  return signal_type_error(arg, S_sequence);
}

bool memq(Value item, Value list)
{
  while (list != NIL)
    {
      if (item == car(list))
        return true;
      list = xcdr(list);
    }
  return false;
}

// ### memq item list => tail
Value EXT_memq(Value item, Value list)
{
  while (list != NIL)
    {
      if (item == car(list))
        return list;
      list = xcdr(list);
    }
  return NIL;
}

// ### memql item list => tail
Value EXT_memql(Value item, Value list)
{
  while (list != NIL)
    {
      if (eql(item, car(list)))
        return list;
      list = xcdr(list);
    }
  return NIL;
}

// ### list-find-eq item list => element
Value SYS_list_find_eq(Value item, Value list)
{
  while (list != NIL)
    {
      if (item == car(list))
        return item;
      list = xcdr(list);
    }
  return NIL;
}

// ### list-find-eql item list => element
Value SYS_list_find_eql(Value item, Value list)
{
  while (list != NIL)
    {
      Value element = car(list);
      if (eql(item, element))
        return element;
      list = xcdr(list);
    }
  return NIL;
}

// ### find-eql item sequence => element
Value SYS_find_eql(Value item, Value sequence)
{
  if (listp(sequence))
    {
      while (sequence != NIL)
        {
          Value element = car(sequence);
          if (eql(item, element))
            return element;
          sequence = xcdr(sequence);
        }
      return NIL;
    }
  if (vectorp(sequence))
    {
      AbstractVector * vector = the_vector(sequence);
      const INDEX len = vector->length();
      for (INDEX i = 0; i < len; i++)
        {
          Value element = vector->elt(i);
          if (eql(item, element))
            return element;
        }
      return NIL;
    }
  return signal_type_error(sequence, S_sequence);
}

// ### string-find char string => element
Value SYS_string_find(Value arg1, Value arg2)
{
  if (characterp(arg1))
    {
      BASE_CHAR c = xchar(arg1);
      if (check_string(arg2)->index_of(c) >= 0)
        return arg1;
    }
  return NIL;
}

// ### symbol-package symbol => package or nil
Value CL_symbol_package(Value arg)
{
  return check_symbol(arg)->package();
}

AbstractString * string(Value arg)
{
  if (stringp(arg))
    return the_string(arg);
  if (symbolp(arg))
    return the_symbol(arg)->name();
  if (characterp(arg))
    return new_simple_string(1, xchar(arg));
  signal_type_error(arg, S_string_designator);
  // not reached
  return NULL;
}

// ### string
Value CL_string(Value arg)
{
  if (stringp(arg))
    return arg;
  if (symbolp(arg))
    return make_value(the_symbol(arg)->name());
  if (characterp(arg))
    return make_value(new_simple_string(1, xchar(arg)));
  return signal_type_error(arg, S_string_designator);
}

// ### endp
Value CL_endp(Value arg)
{
  if (consp(arg))
    return NIL;
  if (arg == NIL)
    return T;
  return signal_type_error(arg, S_list);
}

bool typep(Value object, Value type)
{
  if (type == T || type == C_t)
    return true;
  if (object == NIL)
    {
      if (type == S_list || type == S_sequence || type == S_symbol || type == S_null
          || type == S_atom || type == S_boolean)
        return true;
      if (type == C_list || type == C_sequence || type == C_symbol || type == C_null)
        return true;
      return false;
    }
  if (object == T)
    return (type == S_symbol || type == S_atom || type == S_boolean || type == C_symbol);
  if (fixnump(object))
    {
      if (consp(type))
        {
          if (xcar(type) == S_integer)
            {
              Value low = CL_cadr(type);
              Value high = CL_caddr(type);
              // REVIEW
              if (fixnump(low))
                {
                  long n = xlong(object);
                  long lo = xlong(low);
                  if (high == NIL || high == UNSPECIFIED)
                    return n >= lo;
                  if (fixnump(high))
                    return (n >= lo && n <= xlong(high));
                  if (bignump(high))
                    return n >= lo;
                  // REVIEW signal an error here?
                }
              else if (low == NIL || low == UNSPECIFIED)
                {
                  long n = xlong(object);
                  if (high == NIL || high == UNSPECIFIED)
                    return true;
                  if (fixnump(high))
                    return (n <= xlong(high));
                  if (bignump(high))
                    return true;
                }
            }
        }
      else if (type == S_fixnum || type == S_integer || type == S_rational
               || type == S_real || type == S_number || type == S_atom)
        return true;
      else if (type == C_integer || type == C_rational || type == C_real
               || type == C_number)
        return true;
      else if (type == S_bit && (object == FIXNUM_ZERO || object == FIXNUM_ONE))
        return true;
      else
        return false;
    }
  if (characterp(object))
    {
      if (type == S_character || type == S_base_char || type == S_atom
          || type == C_character)
        return true;
      if (type == S_standard_char)
        return standard_char_p(object);
      return false;
    }
  if (consp(object))
    return (type == S_cons || type == S_list || type == S_sequence
            || type == C_cons || type == C_list || type == C_sequence);
  if (symbolp(object))
    {
      if (type == S_symbol || type == S_atom || type == C_symbol)
        return true;
      if (type == S_keyword)
        return (the_symbol(object)->package() == make_value(PACKAGE_KEYWORD));
      return false;
    }
  if (typed_object_p(object))
    return the_typed_object(object)->typep(type);
  return false;
}

// ### builtin-typep object type-specifier => generalized-boolean
Value SYS_builtin_typep(Value object, Value type_specifier)
{
  if (type_specifier == S_values)
    {
      String * s = new String("The symbol ");
      s->append(the_symbol(S_values)->prin1_to_string());
      s->append(" is not valid as a type specifier.");
      return signal_lisp_error(s);
    }
  return typep(object, type_specifier) ? T : NIL;
}

// ### elt sequence index => object
Value CL_elt(Value arg1, Value arg2)
{
  if (vectorp(arg1))
    return the_vector(arg1)->elt(check_index(arg2));
  if (consp(arg1))
    {
      Value list = arg1;
      INDEX index = check_index(arg2);
      for (INDEX i = 0; i < index; i++)
        {
          if (list == NIL)
            return bad_index(index, 0, length(arg1));
          list = cdr(list);
        }
      if (consp(list))
        return xcar(list);
      if (list == NIL)
        return bad_index(index, 0, length(arg1));
      // Dotted list.
      return signal_type_error(list, S_list);
    }
  if (arg1 == NIL)
    return signal_type_error(arg1, S_cons);
  return signal_type_error(arg1, S_sequence);
}

// ### setelt sequence index new-value => new-value
Value SYS_setelt(Value arg1, Value arg2, Value new_value)
{
  if (vectorp(arg1))
    return the_vector(arg1)->aset(check_index(arg2), new_value);
  if (consp(arg1))
    {
      Value list = arg1;
      INDEX index = check_index(arg2);
      for (INDEX i = 0; i < index; i++)
        {
          if (list == NIL)
            return bad_index(index, 0, length(arg1));
          list = cdr(list);
        }
      if (consp(list))
        {
          setcar(list, new_value);
          return new_value;
        }
      if (list == NIL)
        return bad_index(index, 0, length(arg1));
      // Dotted list.
      return signal_type_error(list, S_list);
    }
  if (arg1 == NIL)
    return signal_type_error(arg1, S_cons);
  return signal_type_error(arg1, S_sequence);
}

// ### vector-push new-element vector => new-index-p
Value CL_vector_push(Value new_element, Value vector)
{
  if (vectorp(vector))
    return the_vector(vector)->push(new_element);
  else
    return signal_type_error(vector, S_vector);
}

// ### vector-push-extend-2
Value SYS_vector_push_extend_2(Value arg1, Value arg2)
{
  return check_vector(arg2)->push_extend(arg1);
}

// ### vector-push-extend-3
Value SYS_vector_push_extend_3(Value arg1, Value arg2, Value arg3)
{
  AbstractVector * vector = check_vector(arg2);
  if (fixnump(arg3))
    {
      long extension = xlong(arg3);
      if (extension < 1)
        extension = 1;
      // REVIEW enforce array-dimension-limit too
      return vector->push_extend(arg1, extension);
    }
  return signal_type_error(arg3, S_fixnum);
}

// ### vector-push-extend new-element vector &optional extension => new-index
Value CL_vector_push_extend(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 3)
    return wrong_number_of_arguments(S_vector_push_extend, numargs, 2, 3);
  AbstractVector * vector = check_vector(args[1]);
  if (numargs == 3)
    {
      if (fixnump(args[2]))
        {
          long extension = xlong(args[2]);
          if (extension < 1)
            extension = 1;
          // REVIEW enforce array-dimension-limit too
          return vector->push_extend(args[0], extension);
        }
      return signal_type_error(args[2], S_fixnum);
    }
  else
    return vector->push_extend(args[0], 1);
}

// ### vector-pop vector => element
Value CL_vector_pop(Value vector)
{
  if (vectorp(vector))
    return the_vector(vector)->pop();
  else
    return signal_type_error(vector, S_vector);
}

#ifdef __x86_64__
// ### value-to-ub64
Value SYS_value_to_ub64(Value arg)
{
  unsigned long n = (unsigned long) arg;
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  return make_value(new Bignum(n));
}
#else
// ### value-to-ub32
Value SYS_value_to_ub32(Value arg)
{
  unsigned long n = (unsigned long) arg;
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  return make_value(new Bignum(n));
}

#endif

// ### vector-data
Value SYS_vector_data(Value arg)
{
  SimpleArray_UB8_1 * vector = check_simple_array_ub8_1(arg);
  return make_number((unsigned long)vector->data());
}

// ### concatenate-to-string
Value SYS_concatenate_to_string(Value sequences)
{
  INDEX len = 0;
  Value tail = check_list(sequences);
  while (tail != NIL)
    {
      len += length(car(tail));
      tail = xcdr(tail);
    }
  SimpleString * result = new_simple_string(len);
  BASE_CHAR * data = result->data();
  INDEX i = 0;
  tail = sequences;
  while (tail != NIL)
    {
      Value seq = car(tail);
      if (stringp(seq))
        {
          AbstractString * string = the_string(seq);
          INDEX limit = string->length();
          for (INDEX j = 0; j < limit; j++)
            data[i++] = string->fast_char_at(j);
        }
      else if (vectorp(seq))
        {
          AbstractVector * vector = the_vector(seq);
          INDEX limit = vector->length();
          for (INDEX j = 0; j < limit; j++)
            data[i++] = char_value(vector->elt(j));
        }
      else
        {
          // a list
          while (seq != NIL)
            {
              data[i++] = char_value(car(seq));
              seq = xcdr(seq);
            }
        }
      tail = xcdr(tail);
    }
  return make_value(result);
}

// ### acons key datum alist => new-alist
Value CL_acons(Value key, Value datum, Value alist)
{
  return make_cons(make_cons(key, datum), alist);
}

// ### assq
Value EXT_assq(Value item, Value alist)
{
  while (consp(alist))
    {
      Value entry = xcar(alist);
      if (consp(entry))
        {
          if (xcar(entry) == item)
            return entry;
        }
      else if (entry != NIL)
        return signal_type_error(entry, S_list);
      alist = xcdr(alist);
    }
  if (alist != NIL)
    return signal_type_error(alist, S_list);
  return NIL;
}

// ### assql
Value EXT_assql(Value item, Value alist)
{
  while (consp(alist))
    {
      Value entry = xcar(alist);
      if (consp(entry))
        {
          if (eql(xcar(entry), item))
            return entry;
        }
      else if (entry != NIL)
        return signal_type_error(entry, S_list);
      alist = xcdr(alist);
    }
  if (alist != NIL)
    return signal_type_error(alist, S_list);
  return NIL;
}

bool is_valid_setf_function_name(Value arg)
{
  if (consp(arg))
    {
      Cons * cons = the_cons(arg);
      if (cons->xcar() == S_setf && consp(cons->xcdr()))
        {
          Cons * cdr = the_cons(cons->xcdr());
          return (symbolp(cdr->xcar()) && cdr->xcdr() == NIL);
        }
    }
  return false;
}

// ### setf-function-name-p
Value SYS_setf_function_name_p(Value arg)
{
  return is_valid_setf_function_name(arg) ? T : NIL;
}

// ### fdefinition-block-name
Value SYS_fdefinition_block_name(Value arg)
{
  if (symbolp(arg))
    return arg;
  if (is_valid_setf_function_name(arg))
    return CL_cadr(arg);
  return signal_type_error(arg, FUNCTION_NAME);
}

// ### lambda-expression-p
Value SYS_lambda_expression_p(Value arg)
{
  if (!consp(arg))
    return NIL;
  if (xcar(arg) != S_lambda)
    return NIL;
  if (!consp(xcdr(arg)))
    return NIL;
  if (!listp(xcadr(arg)))
    return NIL;
  return T;
}

// ### make-keyword string-designator => keyword
Value SYS_make_keyword(Value arg)
{
  Value value = PACKAGE_KEYWORD->intern(string(arg), true);
  current_thread()->clear_values();
  return value;
}

// ### nthcdr n list => tail
Value CL_nthcdr(Value index, Value list)
{
  const long n = check_index(index);
  for (long i = 0; i < n; i++)
    {
      list = cdr(list);
      if (list == NIL)
        break;
    }
  return list;
}

// ### copy-tree
Value CL_copy_tree(Value arg)
{
  if (consp(arg))
    return make_cons(CL_copy_tree(xcar(arg)), CL_copy_tree(xcdr(arg)));
  else
    return arg;
}

TypedObject * coerce_to_function(Value value)
{
  if (functionp(value))
    return the_function(value);
  if (symbolp(value))
    {
      Symbol * sym = the_symbol(value);
      if (sym->is_autoload())
        {
          Autoload * autoload = reinterpret_cast<Autoload *>(sym->function());
          assert(autoload != NULL);
          autoload->load();
        }
      if (sym->is_macro() || sym->is_special_operator())
        {
          signal_lisp_error(new UndefinedFunction(value));
          // not reached
          return NULL;
        }
      else
        {
          TypedObject * function = sym->function();
          if (function)
            return function;
        }
    }
  else if (is_valid_setf_function_name(value))
    {
      Value setf_function = the_symbol(CL_cadr(value))->get(S_setf_function);
      if (functionp(setf_function))
        return the_function(setf_function);
   }
  else if (consp(value) && xcar(value) == S_lambda)
    return new Closure(value, new Environment());
  signal_lisp_error(new UndefinedFunction(value));
  // not reached
  return NULL;
}

// ### coerce-to-function
Value SYS_coerce_to_function(Value value)
{
  return make_value(coerce_to_function(value));
}

// ### %member item list key test test-not => tail
Value SYS_member_internal(Value item, Value list, Value key, Value test, Value test_not)
{
  Value tail = check_list(list);
  if (test != NIL && test_not != NIL)
    return signal_lisp_error("MEMBER: test and test-not both supplied");
  if (test_not == NIL)
    {
      if (test == NIL || test == make_value(the_symbol(S_eql)->function()))
        test = S_eql;
    }
  if (key == NIL)
    {
      if (test == S_eql)
        {
          while (consp(tail))
            {
              if (eql(item, xcar(tail)))
                return tail;
              tail = xcdr(tail);
            }
        }
      else if (test != NIL)
        {
          TypedObject * test_function = coerce_to_function(test);
          Thread * thread = current_thread();
          while (consp(tail))
            {
              Value candidate = xcar(tail);
              if (thread->execute(test_function, item, candidate) != NIL)
                return tail;
              tail = xcdr(tail);
            }
        }
      else
        {
          // test == NIL
          TypedObject * test_not_function = coerce_to_function(test_not);
          Thread * thread = current_thread();
          while (consp(tail))
            {
              Value candidate = xcar(tail);
              if (thread->execute(test_not_function, item, candidate) == NIL)
                return tail;
              tail = xcdr(tail);
            }
        }
    }
  else
    {
      // key != NIL
      TypedObject * key_function = coerce_to_function(key);
      TypedObject * test_function = NULL;
      TypedObject * test_not_function = NULL;
      if (test != NIL)
        test_function = coerce_to_function(test);
      else
        {
          assert(test_not != NIL);
          test_not_function = coerce_to_function(test_not);
        }
      Thread * thread = current_thread();
      while (consp(tail))
        {
          Value candidate = thread->execute(key_function, xcar(tail));
          if (test_function)
            {
              if (thread->execute(test_function, item, candidate) != NIL)
                return tail;
            }
          else
            {
              if (thread->execute(test_not_function, item, candidate) == NIL)
                return tail;
            }
          tail = xcdr(tail);
        }
    }
  if (tail != NIL)
    return signal_type_error(tail, S_list);
  return NIL;
}

long constantp(Value value)
{
  if (consp(value))
    return (xcar(value) == S_quote && consp(xcdr(value)) && CL_cddr(value) == NIL);
  else if (symbolp(value))
    return the_symbol(value)->is_constant();
  else
    return true;
}

// ### constantp form &optional environment => generalized-boolean
Value CL_constantp(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
    case 2:
      return constantp(args[0]) ? T : NIL;
    default:
      return wrong_number_of_arguments(S_constantp, numargs, 1, 2);
    }
}

Value CL_bit_vector_p(Value arg)
{
  return bit_vector_p(arg) ? T : NIL;
}

// ### function-lambda-expression function => lambda-expression, closure-p, name
Value CL_function_lambda_expression(Value function)
{
  Thread * thread = current_thread();
  if (closurep(function))
    {
      Closure * closure = the_closure(function);
      Value lambda_expression = closure->body();
      Value decls = closure->declarations();
      while (decls != NIL)
        {
          lambda_expression = make_cons(car(decls), lambda_expression);
          decls = xcdr(decls);
        }
      lambda_expression = make_cons(closure->lambda_list(), lambda_expression);
      lambda_expression = make_cons(S_lambda, lambda_expression);
      Environment * env = closure->environment();
      return thread->set_values(lambda_expression,
                                (env && !env->is_empty()) ? make_value(env) : NIL,
                                NIL);
    }
  if (functionp(function))
    return thread->set_values(NIL, NIL, NIL);
  return signal_type_error(function, S_function);
}

// ### last1 list => tail
// returns the last cons (not the last element) of LIST
Value SYS_last1(Value arg)
{
  if (arg == NIL)
    return arg;
  if (consp(arg))
    {
      while (true)
        {
          Value cdr = xcdr(arg);
          if (!consp(cdr))
            return arg;
          arg = cdr;
        }
    }
  else
    return signal_type_error(arg, S_list);
}

// ### last list &optional (n 1) => tail
// returns the last N conses of LIST
Value CL_last(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
//       {
//         Value arg = args[0];
//         if (arg == NIL)
//           return arg;
//         if (consp(arg))
//           {
//             while (true)
//               {
//                 Value cdr = xcdr(arg);
//                 if (!consp(cdr))
//                   return arg;
//                 arg = cdr;
//               }
//           }
//         else
//           return signal_type_error(arg, S_list);
//       }
      return SYS_last1(args[0]);
    case 2:
      {
        Value list = check_list(args[0]);
        if (fixnump(args[1]))
          {
            long n = xlong(args[1]);
            if (n >= 0)
              {
                if (list == NIL)
                  return list;
                Value result = list;
                while (consp(list))
                  {
                    list = xcdr(list);
                    if (n-- <= 0)
                      result = xcdr(result);
                  }
                return result;
              }
          }
        else if (bignump(args[1]))
          {
            if (list == NIL)
              return NIL;
            Value n = args[1];
            Value result = list;
            while (consp(list))
              {
                list = xcdr(list);
                if (!plusp(n))
                  result = xcdr(result);
                n = CL_one_minus(n);
              }
            return result;
          }
        return signal_type_error(args[1], S_unsigned_byte);
      }
    default:
      return wrong_number_of_arguments(S_last, numargs, 1, 2);
    }
}

// ### getf plist indicator &optional default => value
Value CL_getf(unsigned int numargs, Value args[])
{
  Value plist, indicator, default_value;
  switch (numargs)
    {
    case 2:
      plist = check_list(args[0]);
      indicator = args[1];
      default_value = NIL;
      break;
    case 3:
      plist = check_list(args[0]);
      indicator = args[1];
      default_value = args[2];
      break;
    default:
      return wrong_number_of_arguments(S_getf, numargs, 2, 3);
    }
  Value list = plist;
  while (list != NIL)
    {
      if (car(list) == indicator)
        return CL_cadr(list);
      if (consp(xcdr(list)))
        list = CL_cddr(list);
      else
        {
          TypeError * error = new TypeError(xcdr(list), S_cons);
          String * s = new String("Malformed property list: ");
          s->append(::prin1_to_string(plist));
          error->set_slot_value(S_format_control, make_value(s));
          return signal_lisp_error(error);
        }
    }
  return default_value;
}

// ### putf
Value SYS_putf(Value plist, Value indicator, Value new_value)
{
  Value list = plist;
  while (list != NIL)
    {
      if (car(list) == indicator)
        {
          // Found it!
          setcar(xcdr(list), new_value);
          return plist;
        }
      list = CL_cddr(list);
    }
  // Not found.
  return make_cons(indicator, make_cons(new_value, plist));
}

// ### get-properties plist indicator-list => indicator, value, tail
Value CL_get_properties(Value plist, Value indicator_list)
{
  Thread * const thread = current_thread();
  while (plist != NIL)
    {
      if (consp(cdr(plist)))
        {
          Value indicator = xcar(plist);
          Value indicators = indicator_list;
          while (consp(indicators))
            {
              if (indicator == xcar(indicators))
                return thread->set_values(indicator, CL_cadr(plist), plist);
              indicators = xcdr(indicators);
            }
          if (indicators != NIL)
            return signal_type_error(indicators, S_list);
          plist = CL_cddr(plist);
        }
      else
        return signal_type_error(xcdr(plist), S_cons);
    }
  return thread->set_values(NIL, NIL, NIL);
}

// ### values-list list => element*
// Returns the elements of the list as multiple values.
Value CL_values_list(Value list)
{
  Thread * thread = current_thread();
  if (list == NIL)
    return thread->set_values();
  if (cdr(list) == NIL)
    return xcar(list);
  long values_length = length(list);
  Value * values = new Value[values_length];
  for (long i = 0; i < values_length; i++)
    {
      values[i] = xcar(list);
      list = xcdr(list);
    }
  return thread->set_values(values_length, values);
}

// ### apropos-list string &optional package => <no values>
Value CL_apropos_list(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_apropos_list, numargs, 1, 2);
  const char * s = string(args[0])->as_c_string();
  Value list = NIL;
  Value packages;
  if (numargs == 2 && args[1] != NIL)
    packages = list1(make_value(coerce_to_package(args[1])));
  else
    packages = CL_list_all_packages();
  while (packages != NIL)
    {
      Package * package = the_package(xcar(packages));
      Value external_symbols = package->external_symbols();
      while (external_symbols != NIL)
        {
          Symbol * sym = the_symbol(xcar(external_symbols));
          if (sym->apropos(s))
            list = make_cons(make_value(sym), list);
          external_symbols = xcdr(external_symbols);
        }
      Value internal_symbols = package->internal_symbols();
      while (internal_symbols != NIL)
        {
          Symbol * sym = the_symbol(xcar(internal_symbols));
          if (sym->apropos(s))
            list = make_cons(make_value(sym), list);
          internal_symbols = xcdr(internal_symbols);
        }
      packages = xcdr(packages);
    }
  return list;
}

Value list_subseq(Value list, unsigned long start, unsigned long end)
{
  unsigned long index = 0;
  Value result = NIL;
  while (list != NIL)
    {
      if (index == end)
        break;
      if (index++ >= start)
        result = make_cons(car(list), result);
      list = cdr(list);
    }
  return CL_nreverse(result);
}

// ### subseq sequence start &optional end => subsequence
Value CL_subseq(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 3)
    return wrong_number_of_arguments(S_subseq, numargs, 2, 3);
  Value sequence = args[0];
  INDEX len = length(sequence);
  INDEX start = check_index(args[1], 0, len);
  INDEX end;
  if (numargs == 2 || args[2] == NIL)
    end = len;
  else
    end = check_index(args[2], start, len);
  if (listp(sequence))
    return list_subseq(sequence, start, end);
  if (vectorp(sequence))
    return the_vector(sequence)->subseq(start, end);
  return signal_type_error(sequence, S_sequence);
}

// ### subseq2 sequence start => subsequence
Value SYS_subseq2(Value arg1, Value arg2)
{
  Value sequence = arg1;
  INDEX end = length(sequence);
  INDEX start = check_index(arg2, 0, end);
  if (listp(sequence))
    return list_subseq(sequence, start, end);
  if (vectorp(sequence))
    return the_vector(sequence)->subseq(start, end);
  return signal_type_error(sequence, S_sequence);
}

// ### subseq3 sequence start end => subsequence
Value SYS_subseq3(Value arg1, Value arg2, Value arg3)
{
  Value sequence = arg1;
  INDEX len = length(sequence);
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 == NIL) ? len : check_index(arg3, start, len);
  if (listp(sequence))
    return list_subseq(sequence, start, end);
  if (vectorp(sequence))
    return the_vector(sequence)->subseq(start, end);
  return signal_type_error(sequence, S_sequence);
}

// ### nth n list => object
Value CL_nth(Value index, Value list)
{
  long n = check_index(index);
  if (list == NIL)
    return NIL;
  if (consp(list))
    {
      long i = 0;
      while (true)
        {
          if (i == n)
            return car(list);
          list = xcdr(list);
          if (list == NIL)
            return list;
          ++i;
        }
    }
  return signal_type_error(list, S_list);
}

// ### set-nth n list new-value => new-value
Value SYS_set_nth(Value index, Value list, Value new_value)
{
  long n = check_index(index);
  long i = 0;
  while (consp(list))
    {
      if (i == n)
        {
          setcar(list, new_value);
          return new_value;
        }
      list = cdr(list);
      ++i;
    }
  return bad_index(n, 0, length(list));
}

// ### %defconstant name initial-value => name
Value SYS_defconstant_internal(Value name, Value initial_value)
{
  check_symbol(name)->initialize_constant(initial_value);
  return name;
}

// ### compiled-function-p
Value CL_compiled_function_p(Value arg)
{
  if (functionp(arg))
    {
      long widetag = the_function(arg)->widetag();
      return (widetag == WIDETAG_PRIMITIVE || widetag == WIDETAG_COMPILED_CLOSURE
              || widetag == WIDETAG_COMPILED_FUNCTION) ? T : NIL;
    }
  return NIL;
}

// ### intersection-eql
Value SYS_intersection_eql(Value list1, Value list2)
{
  Value result = NIL;
  while (list1 != NIL)
    {
      Value element = car(list1);
      Value list = list2;
      while (list != NIL)
        {
          if (eql(element, car(list)))
            {
              result = make_cons(element, result);
              break;
            }
          list = xcdr(list);
        }
      list1 = xcdr(list1);
    }
  return result;
}

// ### byte
Value CL_byte(Value size, Value position)
{
  return make_cons(size, position);
}

// ### byte-size
Value CL_byte_size(Value bytespec)
{
  return car(bytespec);
}

// ### byte-position
Value CL_byte_position(Value bytespec)
{
  return cdr(bytespec);
}

// ### logcount
Value CL_logcount(Value arg)
{
  // FIXME optimize fixnum case
  mpz_t z;
  if (fixnump(arg))
    mpz_init_set_si(z, xlong(arg));
  else if (bignump(arg))
    mpz_init_set(z, the_bignum(arg)->_z);
  else
    return signal_type_error(arg, S_integer);
  if (mpz_sgn(z) < 0)
    {
      // (logcount x) ==  (logcount (- (+ x 1)))
      mpz_add_ui(z, z, 1);
      mpz_neg(z, z);
    }
  return make_number(mpz_popcount(z));
}

// ### lognot
Value CL_lognot(Value arg)
{
  if (fixnump(arg))
    return make_fixnum(~xlong(arg));
  if (bignump(arg))
    {
      mpz_t z;
      mpz_init(z);
      mpz_com(z, the_bignum(arg)->_z);
      Value value = normalize(z);
      MPZ_CLEAR(z);
      return value;
    }
  return signal_type_error(arg, S_integer);
}

// ### lognand integer-1 integer-2 => result-integer
Value CL_lognand(Value arg1, Value arg2)
{
  if (fixnump(arg1))
    {
      if (fixnump(arg2))
        return make_number(~(xlong(arg1) & xlong(arg2)));
      if (bignump(arg2))
        {
          mpz_t z;
          mpz_init_set_si(z, xlong(arg1));
          mpz_and(z, z, the_bignum(arg2)->_z);
          mpz_com(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      return signal_type_error(arg2, S_integer);
    }
  if (bignump(arg1))
    {
      if (fixnump(arg2))
        {
          mpz_t z;
          mpz_init_set_si(z, xlong(arg2));
          mpz_and(z, z, the_bignum(arg1)->_z);
          mpz_com(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      if (bignump(arg2))
        {
          mpz_t z;
          mpz_init(z);
          mpz_and(z, the_bignum(arg1)->_z, the_bignum(arg2)->_z);
          mpz_com(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      return signal_type_error(arg2, S_integer);
    }
  return signal_type_error(arg1, S_integer);
}

// ### lognor integer-1 integer-2 => result-integer
Value CL_lognor(Value arg1, Value arg2)
{
  if (fixnump(arg1))
    {
      if (fixnump(arg2))
        return make_number(~(xlong(arg1) | xlong(arg2)));
      if (bignump(arg2))
        {
          mpz_t z;
          mpz_init_set_si(z, xlong(arg1));
          mpz_ior(z, z, the_bignum(arg2)->_z);
          mpz_com(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      return signal_type_error(arg2, S_integer);
    }
  if (bignump(arg1))
    {
      if (fixnump(arg2))
        {
          mpz_t z;
          mpz_init_set_si(z, xlong(arg2));
          mpz_ior(z, z, the_bignum(arg1)->_z);
          mpz_com(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      if (bignump(arg2))
        {
          mpz_t z;
          mpz_init(z);
          mpz_ior(z, the_bignum(arg1)->_z, the_bignum(arg2)->_z);
          mpz_com(z, z);
          Value value = normalize(z);
          MPZ_CLEAR(z);
          return value;
        }
      return signal_type_error(arg2, S_integer);
    }
  return signal_type_error(arg1, S_integer);
}

// ### logbitp index integer => generalized-boolean
Value CL_logbitp(Value arg1, Value arg2)
{
  if (fixnump(arg1))
    {
      long index = xlong(arg1);
      if (index < 0)
        return signal_type_error(arg1, S_unsigned_byte);
      if (fixnump(arg2))
        {
#ifdef __x86_64__
          if (index < 64)
            {
              long n = xlong(arg2);
              long mask = 1L << index;
              return (n & mask) ? T : NIL;
            }
#else
          if (index < 32)
            {
              long n = xlong(arg2);
              long mask = 1L << index;
              return (n & mask) ? T : NIL;
            }
#endif
          mpz_t z;
          mpz_init_set_si(z, xlong(arg2));
          Value result = mpz_tstbit(z, index) ? T : NIL;
          MPZ_CLEAR(z);
          return result;
        }
      if (bignump(arg2))
        return mpz_tstbit(the_bignum(arg2)->_z, index) ? T : NIL;
      return signal_type_error(arg2, S_integer);
    }
  if (bignump(arg1))
    {
      if (the_bignum(arg1)->minusp())
        return signal_type_error(arg1, S_unsigned_byte);
      if (fixnump(arg2))
        return xlong(arg2) < 0 ? T : NIL;
      if (bignump(arg2))
        return signal_lisp_error("Index too big");
      return signal_type_error(arg2, S_integer);
    }
  return signal_type_error(arg1, S_integer);
}

static BYTE * check_address(Value address)
{
  if (fixnump(address))
    {
      long n = xlong(address);
      if (n > 0)
        return reinterpret_cast<unsigned char *>(n);
    }
  else if (bignump(address))
    {
      Bignum * b = the_bignum(address);
      if (mpz_fits_uint_p (b->_z))
        return reinterpret_cast<unsigned char *>(mpz_get_ui(b->_z));
    }
  signal_type_error(address, list3(S_integer, list1(FIXNUM_ZERO),
                                   make_number(0xffffffff)));
  // not reached
  return NULL;
}

// ### mref-8 address offset => byte
// Returns the unsigned 8-bit byte at OFFSET bytes from SAP.
Value SYS_mref_8(Value address, Value offset)
{
  unsigned char * p = check_address(address) + fixnum_value(offset);
  return make_fixnum(*p);
}

// ### mref-8-signed address offset => signed byte
// Returns the unsigned 8-bit byte at OFFSET bytes from SAP.
Value SYS_mref_8_signed(Value address, Value offset)
{
  char * p = reinterpret_cast<char *>(check_address(address) + fixnum_value(offset));
  return make_fixnum(*p);
}

// ### mref-32 address offset => unsigned int
Value SYS_mref_32(Value address, Value offset)
{
  BYTE * p = check_address(address) + fixnum_value(offset);
  BYTE byte1 = *p++;
  BYTE byte2 = *p++;
  BYTE byte3 = *p++;
  BYTE byte4 = *p;
  unsigned long n = byte1 + (byte2 << 8) + (byte3 << 16) + (byte4 << 24);
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  return make_value(new Bignum((unsigned long)n));
}

// ### mref-32-signed address offset => signed int
Value SYS_mref_32_signed(Value address, Value offset)
{
  BYTE * p = check_address(address) + fixnum_value(offset);
  BYTE byte1 = *p++;
  BYTE byte2 = *p++;
  BYTE byte3 = *p++;
  BYTE byte4 = *p;
  long n = byte1 + (byte2 << 8) + (byte3 << 16) + (byte4 << 24);
#ifdef __x86_64__
  return make_fixnum(n);
#else
  if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  else
    return make_value(new Bignum(n));
#endif
}

#ifdef __x86_64__
// ### mref-64 address offset => unsigned long
Value SYS_mref_64(Value address, Value offset)
{
  BYTE * p = check_address(address) + fixnum_value(offset);
  unsigned long byte1 = *p++;
  unsigned long byte2 = *p++;
  unsigned long byte3 = *p++;
  unsigned long byte4 = *p++;
  unsigned long byte5 = *p++;
  unsigned long byte6 = *p++;
  unsigned long byte7 = *p++;
  unsigned long byte8 = *p;
  unsigned long n =
    byte1 + (byte2 << 8) + (byte3 << 16) + (byte4 << 24) + (byte5 << 32) + (byte6 << 40) + (byte7 << 48) + (byte8 << 56);
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum((long)n);
  return make_value(new Bignum((unsigned long)n));
}
#endif

// REVIEW
// ### address-of object => address (or NIL)
Value SYS_address_of(Value arg)
{
  if (symbolp(arg))
    {
      Symbol * sym = the_symbol(arg);
      unsigned long n = (unsigned long) sym;
      if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
        return make_fixnum((long)n);
      return make_value(new Bignum(n));
    }
  if (typed_object_p(arg))
    {
      TypedObject * obj = the_typed_object(arg);
      unsigned long n = (unsigned long) obj;
      if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
        return make_fixnum((long)n);
      return make_value(new Bignum(n));
    }
  if (consp(arg))
    {
      Cons * cons = the_cons(arg);
      unsigned long n = (unsigned long) cons;
      if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
        return make_fixnum((long)n);
      return make_value(new Bignum(n));
    }
  // FIXME support other cases
  return NIL;
}

// ### proclaim-special
Value SYS_proclaim_special(Value arg)
{
  check_symbol(arg)->proclaim_special();
  return T;
}

// ### set
Value CL_set(Value name, Value value)
{
  if (symbolp(name))
    return current_thread()->set_symbol_value(name, value);
  else
    return signal_type_error(name, S_symbol);
}

// ### check-subsequence sequence start end => T
Value SYS_check_subsequence(Value arg1, Value arg2, Value arg3)
{
  if (!sequencep(arg1))
    return signal_type_error(arg1, S_sequence);
  if (!indexp(arg2))
    return signal_type_error(arg2,
                             list3(S_integer, FIXNUM_ZERO,
                                   make_fixnum(MOST_POSITIVE_FIXNUM)));
  if (!indexp(arg3))
    return signal_type_error(arg3,
                             list3(S_integer, FIXNUM_ZERO,
                                   make_fixnum(MOST_POSITIVE_FIXNUM)));
  return T;
}

// ### copy-string string => simple-string
Value EXT_copy_string(Value arg)
{
  AbstractString * s1 = check_string(arg);
  INDEX len = s1->length();
  SimpleString * s2 = new_simple_string(len);
  BASE_CHAR * data = s2->data();
  for (INDEX i = 0; i < len; i++)
    data[i] = s1->fast_char_at(i);
  return make_value(s2);
}

// ### default-time-zone => time-zone, daylight-p
Value SYS_default_time_zone()
{
#ifdef WIN32
  TIME_ZONE_INFORMATION tzinfo;
  DWORD ret = GetTimeZoneInformation(&tzinfo);
  Value time_zone = make_number(tzinfo.Bias / 60);
  Value daylight_p = (ret == TIME_ZONE_ID_DAYLIGHT) ? T : NIL;
#else
  time_t gmt = time(NULL);
  struct tm * local = localtime(&gmt);
  long seconds = local->tm_gmtoff;
  Value time_zone = (seconds == 0) ? FIXNUM_ZERO : make_number(- seconds / 3600);
  Value daylight_p = (local->tm_isdst > 0) ? T : NIL;
#endif
  return current_thread()->set_values(time_zone, daylight_p);
}

// ### get-internal-real-time
Value CL_get_internal_real_time()
{
#ifdef WIN32
  FILETIME ft;
  GetSystemTimeAsFileTime(&ft);
//   ULARGE_INTEGER x;
//   memcpy(&x, &time, sizeof(time));
//   return make_double_float((double) x.QuadPart / 10000);
  mpz_t z;
  mpz_init_set_ui(z, ft.dwHighDateTime);
  mpz_mul_2exp(z, z, 32); // shift left 32 bits
  mpz_add_ui(z, z, ft.dwLowDateTime);
//   Value result = make_value(new Bignum(z));
  Value result = normalize(z);
  MPZ_CLEAR(z);
  return result;
#else
  struct timeval tv;
  gettimeofday(&tv, NULL);
  mpz_t z;
  mpz_init_set_si(z, tv.tv_sec);
  mpz_mul_ui(z, z, 1000000); // convert to microseconds
  mpz_add_ui(z, z, tv.tv_usec);
  Value result = normalize(z);
  MPZ_CLEAR(z);
  return result;
#endif
}

// ### get-process-times => user-time, system-time
Value SYS_get_process_times()
{
  Thread * thread = current_thread();
#ifdef WIN32
  HANDLE h = GetCurrentProcess();
  FILETIME creation_time;
  FILETIME exit_time;
  FILETIME kernel_time;
  FILETIME user_time;
  // "If the function succeeds, the return value is nonzero."
  GetProcessTimes(h, &creation_time, &exit_time,
                  &kernel_time, &user_time);

  mpz_t user;
  mpz_init_set_ui(user, user_time.dwHighDateTime);
  mpz_mul_2exp(user, user, 32); // shift left 32 bits
  mpz_add_ui(user, user, user_time.dwLowDateTime);
//   Value v1 = make_value(new Bignum(user));
  Value v1 = normalize(user);
  MPZ_CLEAR(user);

  mpz_t system;
  mpz_init_set_ui(system, kernel_time.dwHighDateTime);
  mpz_mul_2exp(system, system, 32); // shift left 32 bits
  mpz_add_ui(system, system, kernel_time.dwLowDateTime);
//   Value v2 = make_value(new Bignum(system));
  Value v2 = normalize(system);
  MPZ_CLEAR(system);

  return thread->set_values(v1, v2);
#else
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);

  mpz_t user;
  mpz_init_set_si(user, rusage.ru_utime.tv_sec);
  mpz_mul_ui(user, user, 1000000); // convert to microseconds
  mpz_add_ui(user, user, rusage.ru_utime.tv_usec);
  Value v1 = normalize(user);
  MPZ_CLEAR(user);

  mpz_t system;
  mpz_init_set_si(system, rusage.ru_stime.tv_sec);
  mpz_mul_ui(system, system, 1000000); // convert to microseconds
  mpz_add_ui(system, system, rusage.ru_stime.tv_usec);
  Value v2 = normalize(system);
  MPZ_CLEAR(system);

  return thread->set_values(v1, v2);
#endif
}

// ### get-internal-run-time
Value CL_get_internal_run_time()
{
  SYS_get_process_times();
  Thread * thread = current_thread();
  Value result = SYS_two_arg_plus(thread->nth_value(0), thread->nth_value(1));
  thread->clear_values();
  return result;
}
