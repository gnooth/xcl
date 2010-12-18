// runtime.cpp
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
#include "runtime.h"
#include "Closure.hpp"
#include "HashTable.hpp"
#include "UnboundVariable.hpp"
#include "UndefinedFunction.hpp"
#include "ValueCell.hpp"

static HashTable * ht_names;

void * RT_malloc(size_t size_in_bytes)
{
  return GC_malloc(size_in_bytes);
}

Value RT_current_thread_call_symbol_0(Value symbol)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function);
}

Value RT_current_thread_call_symbol_1(Value symbol, Value arg)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, arg);
}

Value RT_current_thread_call_symbol_2(Value symbol, Value arg1, Value arg2)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, arg1, arg2);
}

Value RT_current_thread_call_symbol_3(Value symbol, Value arg1, Value arg2, Value arg3)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, arg1, arg2, arg3);
}

Value RT_current_thread_call_symbol_4(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, arg1, arg2, arg3, arg4);
}

Value RT_current_thread_call_symbol_5(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, arg1, arg2, arg3, arg4, arg5);
}

Value RT_current_thread_call_symbol_6(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, arg1, arg2, arg3, arg4, arg5, arg6);
}

Value RT_current_thread_call_symbol(Value symbol, unsigned int numargs, Value args[])
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return current_thread()->execute(function, numargs, args);
}

inline Value thread_call_symbol_0(Thread * thread, Value symbol)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function);
}

Value RT_thread_call_symbol_0(Thread * thread, Value symbol)
{
//   Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
//   if (!function)
//     return signal_lisp_error(new UndefinedFunction(symbol));
//   return thread->execute(function);
  return thread_call_symbol_0(thread, symbol);
}

inline Value thread_call_symbol_1(Thread * thread, Value symbol, Value arg)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, arg);
}

Value RT_thread_call_symbol_1(Thread * thread, Value symbol, Value arg)
{
  return thread_call_symbol_1(thread, symbol, arg);
}

inline Value thread_call_symbol_2(Thread * thread, Value symbol, Value arg1, Value arg2)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, arg1, arg2);
}

Value RT_thread_call_symbol_2(Thread * thread, Value symbol, Value arg1, Value arg2)
{
  return thread_call_symbol_2(thread, symbol, arg1, arg2);
}

inline Value thread_call_symbol_3(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, arg1, arg2, arg3);
}

Value RT_thread_call_symbol_3(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3)
{
  return thread_call_symbol_3(thread, symbol, arg1, arg2, arg3);
}

inline Value thread_call_symbol_4(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, arg1, arg2, arg3, arg4);
}

Value RT_thread_call_symbol_4(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4)
{
  return thread_call_symbol_4(thread, symbol, arg1, arg2, arg3, arg4);
}

Value RT_thread_call_symbol_5(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, arg1, arg2, arg3, arg4, arg5);
}

Value RT_thread_call_symbol_6(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, arg1, arg2, arg3, arg4, arg5, arg6);
}

Value RT_thread_call_symbol(Thread * thread, Value symbol, unsigned int numargs, Value args[])
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return thread->execute(function, numargs, args);
}

inline Value fast_call_symbol_0(Value symbol)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute();
}

Value RT_fast_call_symbol_0(Value symbol)
{
  return fast_call_symbol_0(symbol);
}

inline Value fast_call_symbol_1(Value symbol, Value arg)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(arg);
}

Value RT_fast_call_symbol_1(Value symbol, Value arg)
{
  return fast_call_symbol_1(symbol, arg);
}

inline Value fast_call_symbol_2(Value symbol, Value arg1, Value arg2)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(arg1, arg2);
}

Value RT_fast_call_symbol_2(Value symbol, Value arg1, Value arg2)
{
  return fast_call_symbol_2(symbol, arg1, arg2);
}

inline Value fast_call_symbol_3(Value symbol, Value arg1, Value arg2, Value arg3)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(arg1, arg2, arg3);
}

Value RT_fast_call_symbol_3(Value symbol, Value arg1, Value arg2, Value arg3)
{
  return fast_call_symbol_3(symbol, arg1, arg2, arg3);
}

inline Value fast_call_symbol_4(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(arg1, arg2, arg3, arg4);
}

Value RT_fast_call_symbol_4(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4)
{
  return fast_call_symbol_4(symbol, arg1, arg2, arg3, arg4);
}

Value RT_fast_call_symbol_5(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(arg1, arg2, arg3, arg4, arg5);
}

Value RT_fast_call_symbol_6(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(arg1, arg2, arg3, arg4, arg5, arg6);
}

Value RT_fast_call_symbol(Value symbol, unsigned int numargs, Value args[])
{
  Function * function = reinterpret_cast<Function *>(the_symbol(symbol)->function());
  if (!function)
    return signal_lisp_error(new UndefinedFunction(symbol));
  return function->execute(numargs, args);
}

Value RT_current_thread_call_function_0(Value function)
{
  return current_thread()->execute(the_function(function));
}

Value RT_current_thread_call_function_1(Value function, Value arg)
{
  return current_thread()->execute(the_function(function), arg);
}

Value RT_current_thread_call_function_2(Value function, Value arg1, Value arg2)
{
  return current_thread()->execute(the_function(function), arg1, arg2);
}

Value RT_current_thread_call_function_3(Value function, Value arg1, Value arg2, Value arg3)
{
  return current_thread()->execute(the_function(function), arg1, arg2, arg3);
}

Value RT_current_thread_call_function_4(Value function, Value arg1, Value arg2, Value arg3, Value arg4)
{
  return current_thread()->execute(the_function(function), arg1, arg2, arg3, arg4);
}

Value RT_current_thread_call_function_5(Value function, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  return current_thread()->execute(the_function(function), arg1, arg2, arg3, arg4, arg5);
}

Value RT_current_thread_call_function_6(Value function, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  return current_thread()->execute(the_function(function), arg1, arg2, arg3, arg4, arg5, arg6);
}

Value RT_current_thread_call_function(Value function, unsigned int numargs, Value args[])
{
  return current_thread()->execute(the_function(function), numargs, args);
}

Value RT_thread_call_function_0(Thread * thread, Value function)
{
  return thread->execute(the_function(function));
}

Value RT_thread_call_function_1(Thread * thread, Value function,
                                Value arg)
{
  return thread->execute(the_function(function),
                         arg);
}

Value RT_thread_call_function_2(Thread * thread, Value function,
                                Value arg1, Value arg2)
{
  return thread->execute(the_function(function),
                         arg1, arg2);
}

Value RT_thread_call_function_3(Thread * thread, Value function,
                                Value arg1, Value arg2, Value arg3)
{
  return thread->execute(the_function(function),
                         arg1, arg2, arg3);
}

Value RT_thread_call_function_4(Thread * thread, Value function,
                                Value arg1, Value arg2, Value arg3,
                                Value arg4)
{
  return thread->execute(the_function(function),
                         arg1, arg2, arg3, arg4);
}

Value RT_thread_call_function_5(Thread * thread, Value function,
                                Value arg1, Value arg2, Value arg3,
                                Value arg4, Value arg5)
{
  return thread->execute(the_function(function),
                         arg1, arg2, arg3, arg4, arg5);
}

Value RT_thread_call_function_6(Thread * thread, Value function,
                                Value arg1, Value arg2, Value arg3,
                                Value arg4, Value arg5, Value arg6)
{
  return thread->execute(the_function(function),
                         arg1, arg2, arg3, arg4, arg5, arg6);
}

Value RT_thread_call_function(Thread * thread, Value function,
                              unsigned int numargs, Value args[])
{
  return thread->execute(the_function(function), numargs, args);
}

Value RT_fast_call_function_0(Value function, Value arg)
{
  return the_function(function)->execute();
}

Value RT_fast_call_function_1(Value function, Value arg)
{
  return the_function(function)->execute(arg);
}

Value RT_fast_call_function_2(Value function, Value arg1, Value arg2)
{
  return the_function(function)->execute(arg1, arg2);
}

Value RT_fast_call_function_3(Value function, Value arg1, Value arg2, Value arg3)
{
  return the_function(function)->execute(arg1, arg2, arg3);
}

Value RT_fast_call_function_4(Value function, Value arg1, Value arg2, Value arg3, Value arg4)
{
  return the_function(function)->execute(arg1, arg2, arg3, arg4);
}

Value RT_fast_call_function_5(Value function, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  return the_function(function)->execute(arg1, arg2, arg3, arg4, arg5);
}

Value RT_fast_call_function_6(Value function, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  return the_function(function)->execute(arg1, arg2, arg3, arg4, arg5, arg6);
}

Value RT_fast_call_function(Value function, unsigned int numargs, Value args[])
{
  return the_function(function)->execute(numargs, args);
}

Value RT_thread_funcall_0(Thread * thread, Value callable)
{
  if (functionp(callable))
    return thread->execute(the_function(callable));
  if (symbolp(callable))
    return thread_call_symbol_0(thread, callable);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_thread_funcall_1(Thread * thread, Value callable, Value arg)
{
  if (functionp(callable))
    return thread->execute(the_function(callable), arg);
  if (symbolp(callable))
    return thread_call_symbol_1(thread, callable, arg);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_thread_funcall_2(Thread * thread, Value callable, Value arg1, Value arg2)
{
  if (functionp(callable))
    return thread->execute(the_function(callable), arg1, arg2);
  if (symbolp(callable))
    return thread_call_symbol_2(thread, callable, arg1, arg2);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_thread_funcall_3(Thread * thread, Value callable, Value arg1, Value arg2, Value arg3)
{
  if (functionp(callable))
    return thread->execute(the_function(callable), arg1, arg2, arg3);
  if (symbolp(callable))
    return thread_call_symbol_3(thread, callable, arg1, arg2, arg3);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_thread_funcall_4(Thread * thread, Value callable, Value arg1, Value arg2, Value arg3, Value arg4)
{
  if (functionp(callable))
    return thread->execute(the_function(callable), arg1, arg2, arg3, arg4);
  if (symbolp(callable))
    return thread_call_symbol_4(thread, callable, arg1, arg2, arg3, arg4);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_fast_funcall_0(Value callable)
{
  if (functionp(callable))
    return the_function(callable)->execute();
  if (symbolp(callable))
    return fast_call_symbol_0(callable);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_fast_funcall_1(Value callable, Value arg)
{
  if (functionp(callable))
    return the_function(callable)->execute(arg);
  if (symbolp(callable))
    return fast_call_symbol_1(callable, arg);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_fast_funcall_2(Value callable, Value arg1, Value arg2)
{
  if (functionp(callable))
    return the_function(callable)->execute(arg1, arg2);
  if (symbolp(callable))
    return fast_call_symbol_2(callable, arg1, arg2);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_fast_funcall_3(Value callable, Value arg1, Value arg2, Value arg3)
{
  if (functionp(callable))
    return the_function(callable)->execute(arg1, arg2, arg3);
  if (symbolp(callable))
    return fast_call_symbol_3(callable, arg1, arg2, arg3);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_fast_funcall_4(Value callable, Value arg1, Value arg2, Value arg3, Value arg4)
{
  if (functionp(callable))
    return the_function(callable)->execute(arg1, arg2, arg3, arg4);
  if (symbolp(callable))
    return fast_call_symbol_4(callable, arg1, arg2, arg3, arg4);
  return signal_type_error(callable, list3(S_or, S_function, S_symbol));
}

Value RT_current_thread_symbol_value(Value name)
{
  Value value = current_thread()->symbol_value(name);
  if (value == NULL_VALUE)
    return signal_lisp_error(new UnboundVariable(name));
  return value;
}

Value RT_thread_symbol_value(Thread * thread, Value name)
{
  Value value = thread->symbol_value(name);
  if (value == NULL_VALUE)
    return signal_lisp_error(new UnboundVariable(name));
  return value;
}

Value * RT_current_thread_get_values(Value primary_value, int required)
{
  return current_thread()->get_values(primary_value, required);
}

Value * RT_thread_get_values(Thread * thread, Value primary_value, int required)
{
  return thread->get_values(primary_value, required);
}

Value RT_thread_set_values_1(Thread * thread, Value arg)
{
  return thread->set_values(arg);
}

Value RT_current_thread_set_values_0()
{
  return current_thread()->set_values();
}

Value RT_current_thread_set_values_2(Value arg1, Value arg2)
{
  return current_thread()->set_values(arg1, arg2);
}

Value RT_thread_set_values_2(Thread * thread, Value arg1, Value arg2)
{
  return thread->set_values(arg1, arg2);
}

Value RT_current_thread_set_values_3(Value arg1, Value arg2, Value arg3)
{
  return current_thread()->set_values(arg1, arg2, arg3);
}

Value RT_thread_set_values_3(Thread * thread, Value arg1, Value arg2, Value arg3)
{
  return thread->set_values(arg1, arg2, arg3);
}

Value RT_current_thread_set_values_4(Value arg1, Value arg2, Value arg3, Value arg4)
{
  return current_thread()->set_values(arg1, arg2, arg3, arg4);
}

Value RT_thread_set_values_4(Thread * thread, Value arg1, Value arg2, Value arg3, Value arg4)
{
  return thread->set_values(arg1, arg2, arg3, arg4);
}

void RT_current_thread_clear_values()
{
  current_thread()->clear_values();
}

void RT_thread_clear_values(Thread * thread)
{
  thread->clear_values();
}

Thread * RT_current_thread()
{
  return current_thread();
}

void * RT_current_thread_last_special_binding()
{
  return current_thread()->last_special_binding();
}

void * RT_thread_last_special_binding(Thread * thread)
{
  return thread->last_special_binding();
}

void RT_current_thread_set_last_special_binding(void * binding)
{
  current_thread()->set_last_special_binding(binding);
}

void RT_thread_set_last_special_binding(Thread * thread, void * binding)
{
  thread->set_last_special_binding(binding);
}

void RT_current_thread_bind_special(Value name, Value value)
{
  current_thread()->bind_special(name, value);
}

void RT_thread_bind_special(Thread * thread, Value name, Value value)
{
  thread->bind_special(name, value);
}

void RT_current_thread_bind_special_to_current_value(Value name)
{
  current_thread()->bind_special_to_current_value(name);
}

void RT_thread_bind_special_to_current_value(Thread * thread, Value name)
{
  thread->bind_special_to_current_value(name);
}

Value RT_current_thread_set_symbol_value(Value name, Value value)
{
  return current_thread()->set_symbol_value(name, value);
}

Value RT_thread_set_symbol_value(Thread * thread, Value name, Value value)
{
  return thread->set_symbol_value(name, value);
}

Value RT_symbol_function(Value symbol)
{
  TypedObject * op = the_symbol(symbol)->function();
  if (op && op->widetag() == WIDETAG_AUTOLOAD)
    {
      reinterpret_cast<Autoload *>(op)->load();
      op = the_symbol(symbol)->function();
    }
  if (op)
    return make_value(op);
  else
    return signal_lisp_error(new UndefinedFunction(symbol));
}

Value RT_symbol_setf_function(Value symbol)
{
  Value value = the_symbol(symbol)->get(S_setf_function);
  if (value != NIL)
    return value;
  else
    return signal_lisp_error(new UndefinedFunction(list2(S_setf, symbol)));
}

extern Value make_compiled_closure(Value template_function, Value * data);

Value RT_make_compiled_closure(Value template_function, Value * data)
{
  return make_compiled_closure(template_function, data);
}

Value RT_make_compiled_closure_2(Value template_function, Value * data, unsigned int data_length)
{
//   printf("RT_make_compiled_closure_2 called data_length = %d\n", data_length);
//   fflush(stdout);
  INDEX size = data_length * sizeof(Value);
  Value * copy = (Value *) GC_malloc(size);
  memcpy(copy, data, size);
//   return make_compiled_closure(template_function, data);
  return make_compiled_closure(template_function, copy);
}

ValueCell * RT_make_value_cell()
{
  return new ValueCell(NIL);
}

ValueCell * RT_make_value_cell_1(Value value)
{
  return new ValueCell(value);
}

Value RT_value_cell_value(ValueCell * value_cell)
{
  return value_cell->value();
}

void RT_set_value_cell_value(ValueCell * value_cell, Value value)
{
  value_cell->set_value(value);
}

ValueCell * * RT_allocate_closure_data_vector(INDEX data_length)
{
  INDEX size = data_length * sizeof(ValueCell *);
  ValueCell * * data = (ValueCell * *) GC_malloc(size);
  for (INDEX i = 0; i < data_length; i++)
    data[i] = new ValueCell(NIL);
  return data;
}

Value * RT_copy_closure_data_vector(Value * data, INDEX data_length)
{
  INDEX size = data_length * sizeof(Value);
  Value * copy = (Value *) GC_malloc(size);
  memcpy(copy, data, size);
  return copy;
}

void RT_unshare_variable(unsigned long index, ValueCell * data[])
{
  ValueCell * old = data[index];
  Value value = old->value();
  data[index] = new ValueCell(value);
}

Value RT_restify(Value args[], int start, int end)
{
  Value result = NIL;
  for (int i = end; i-- > start;)
    result = make_cons(args[i], result);
  return result;
}

Value * RT_process_args(unsigned int numargs, Value args[], Value closure, Value vals[])
{
  return check_closure(closure)->process_args(numargs, args, vals);
}

INDEX RT_accumulate_values(Thread * thread, Value result, Value values[], INDEX index)
{
  int len = thread->values_length();
  if (len < 0 || len == 1)
    values[index++] = result;
  else if (len > 1)
    {
      Value * new_values = thread->values();
      for (int i = 0; i < len; i++)
        values[index++] = new_values[i];
    }
  thread->clear_values();
  return index;
}

Value RT_thread_multiple_value_call(Thread * thread, Value callable, Value args[], unsigned int numargs)
{
  if (functionp(callable))
    return funcall(the_function(callable), numargs, args, thread);
  if (symbolp(callable))
    {
      Function * function = reinterpret_cast<Function *>(the_symbol(callable)->function());
      if (!function)
        return signal_lisp_error(new UndefinedFunction(callable));
      return funcall(function, numargs, args, thread);
    }
  return signal_type_error(callable, S_function_designator);
}

Value RT_multiple_value_list(Value result)
{
  Thread * thread = current_thread();
  int len = thread->values_length();
  if (len < 0 || len == 1)
    result = list1(result);
  else
    {
      result = NIL;
      if (len > 1)
        {
          Value * values = thread->values();
          for (int i = len; i-- > 0;)
            result = make_cons(values[i], result);
        }
    }
  thread->clear_values();
  return result;
}

bool RT_equal(Value arg1, Value arg2)
{
  return equal(arg1, arg2);
}

bool RT_eql(Value arg1, Value arg2)
{
  return eql(arg1, arg2);
}

bool RT_equals(Value arg1, Value arg2)
{
  return equals(arg1, arg2);
}

// predicates
bool RT_classp(Value arg)
{
  return classp(arg);
}

bool RT_endp(Value arg)
{
  if (consp(arg))
    return false;
  if (arg == NIL)
    return true;
  signal_type_error(arg, S_list);
  // not reached
  return false;
}

bool RT_functionp(Value arg)
{
  return functionp(arg);
}

bool RT_integerp(Value arg)
{
  return integerp(arg);
}

bool RT_listp(Value arg)
{
  return listp(arg);
}

bool RT_minusp(Value arg)
{
  return minusp(arg);
}

bool RT_numberp(Value arg)
{
  return numberp(arg);
}

bool RT_plusp(Value arg)
{
  return plusp(arg);
}

bool RT_stringp(Value arg)
{
  return stringp(arg);
}

bool RT_vectorp(Value arg)
{
  return vectorp(arg);
}

inline Value fast_apply_function_2(TypedObject * function, Value arg2)
{
  if (listp(arg2))
    {
      INDEX numargs = length(arg2);
      Value * args = (Value *) alloca(numargs * sizeof(Value));
      for (INDEX i = 0; i < numargs; i++)
        {
          args[i] = car(arg2);
          arg2 = xcdr(arg2);
        }
      if (function->arity() >= 0)
        {
          // fixed arity
          switch (numargs)
            {
            case 0:
              return function->execute();
            case 1:
              return function->execute(args[0]);
            case 2:
              return function->execute(args[0], args[1]);
            case 3:
              return function->execute(args[0], args[1], args[2]);
            case 4:
              return function->execute(args[0], args[1], args[2], args[3]);
            case 5:
              return function->execute(args[0], args[1], args[2], args[3], args[4]);
            case 6:
              return function->execute(args[0], args[1], args[2], args[3], args[4], args[5]);
            default:
              return function->execute(numargs, args);
            }
        }
      else
        return function->execute(numargs, args);
    }
  else
    return signal_type_error(arg2, S_list);
}

Value RT_fast_apply_function_2(Value arg1, Value arg2)
{
  return fast_apply_function_2(the_typed_object(arg1), arg2);
}

Value RT_fast_apply_2(Value arg1, Value arg2)
{
  return fast_apply_function_2(coerce_to_function(arg1), arg2);
}

void RT_handle_interrupt()
{
  Thread * thread = current_thread();
  if (thread == primordial_thread)
    {
      interrupted = 0;
      RT_thread_call_symbol_0(thread, S_break);
      interrupted = 0;
    }
}

#ifndef __x86_64__
unsigned long RT_unsigned_byte_to_raw_ub32(Value value)
{
  if (fixnump(value))
    {
      long n = xlong(value);
      if (n >= 0)
        {
//           printf("RT_unsigned_byte_to_raw_ub32 returning %ld\n", n);
//           fflush(stdout);
          return (unsigned long) n;
        }
    }
  else if (bignump(value))
    {
      Bignum * b = the_bignum(value);
      if (mpz_sgn(b->_z) >= 0)
        {
//           printf("RT_unsigned_byte_to_raw_ub32 returning %lu\n", mpz_get_ui(b->_z));
//           fflush(stdout);
          return mpz_get_ui(b->_z);
        }
    }
  signal_type_error(value, S_unsigned_byte);
  // not reached
  return 0;
}
#endif

// Value RT_make_unsigned_integer(unsigned long n)
// {
//   return make_unsigned_integer(n);
// }

#ifndef __x86_64__
Value RT_mod32(Value value)
{
  if (fixnump(value))
    {
      if (xlong(value) >= 0)
        return value;
    }
  else if (bignump(value))
    {
      Bignum * b = the_bignum(value);
      if (mpz_sgn(b->_z) >= 0)
        return make_unsigned_integer(mpz_get_ui(b->_z));
    }
  signal_type_error(value, S_unsigned_byte);
  // not reached
  return 0;
}
#endif

#ifdef __x86_64__
unsigned long RT_unsigned_bignum_to_raw_ub64(Value value)
{
  return mpz_get_ui(the_bignum(value)->_z);
}
#endif

void RT_progv_bind_vars(Thread * thread, Value symbols, Value values)
{
  for (Value list = symbols; list != NIL; list = xcdr(list))
    {
      Value name = car(list);
      if (!symbolp(name))
        {
          signal_type_error(name, S_symbol);
          return;
        }
      Value value;
      if (values != NIL)
        {
          value = car(values);
          values = xcdr(values);
        }
      else
        {
          // "If too few values are supplied, the remaining symbols are
          // bound and then made to have no value."
          value = NULL_VALUE;
        }
      thread->bind_special(name, value);
    }
}

Value RT_bad_index(long index, long length)
{
  return signal_type_error(make_fixnum(index),
                           list3(S_integer, make_fixnum(0),
                                 list1(make_fixnum(length))));
}

void initialize_runtime()
{
  ht_names = new EqualHashTable();

  ht_names->put(make_simple_string("RT_malloc"),
                make_integer((unsigned long)RT_malloc));

  ht_names->put(make_simple_string("RT_current_thread_call_symbol_0"),
                make_integer((unsigned long)RT_current_thread_call_symbol_0));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol_1"),
                make_integer((unsigned long)RT_current_thread_call_symbol_1));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol_2"),
                make_integer((unsigned long)RT_current_thread_call_symbol_2));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol_3"),
                make_integer((unsigned long)RT_current_thread_call_symbol_3));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol_4"),
                make_integer((unsigned long)RT_current_thread_call_symbol_4));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol_5"),
                make_integer((unsigned long)RT_current_thread_call_symbol_5));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol_6"),
                make_integer((unsigned long)RT_current_thread_call_symbol_6));
  ht_names->put(make_simple_string("RT_current_thread_call_symbol"),
                make_integer((unsigned long)RT_current_thread_call_symbol));

  ht_names->put(make_simple_string("RT_thread_call_symbol_0"),
                make_integer((unsigned long)RT_thread_call_symbol_0));
  ht_names->put(make_simple_string("RT_thread_call_symbol_1"),
                make_integer((unsigned long)RT_thread_call_symbol_1));
  ht_names->put(make_simple_string("RT_thread_call_symbol_2"),
                make_integer((unsigned long)RT_thread_call_symbol_2));
  ht_names->put(make_simple_string("RT_thread_call_symbol_3"),
                make_integer((unsigned long)RT_thread_call_symbol_3));
  ht_names->put(make_simple_string("RT_thread_call_symbol_4"),
                make_integer((unsigned long)RT_thread_call_symbol_4));
  ht_names->put(make_simple_string("RT_thread_call_symbol_5"),
                make_integer((unsigned long)RT_thread_call_symbol_5));
  ht_names->put(make_simple_string("RT_thread_call_symbol_6"),
                make_integer((unsigned long)RT_thread_call_symbol_6));
  ht_names->put(make_simple_string("RT_thread_call_symbol"),
                make_integer((unsigned long)RT_thread_call_symbol));

  ht_names->put(make_simple_string("RT_fast_call_symbol_0"),
                make_integer((unsigned long)RT_fast_call_symbol_0));
  ht_names->put(make_simple_string("RT_fast_call_symbol_1"),
                make_integer((unsigned long)RT_fast_call_symbol_1));
  ht_names->put(make_simple_string("RT_fast_call_symbol_2"),
                make_integer((unsigned long)RT_fast_call_symbol_2));
  ht_names->put(make_simple_string("RT_fast_call_symbol_3"),
                make_integer((unsigned long)RT_fast_call_symbol_3));
  ht_names->put(make_simple_string("RT_fast_call_symbol_4"),
                make_integer((unsigned long)RT_fast_call_symbol_4));
  ht_names->put(make_simple_string("RT_fast_call_symbol_5"),
                make_integer((unsigned long)RT_fast_call_symbol_5));
  ht_names->put(make_simple_string("RT_fast_call_symbol_6"),
                make_integer((unsigned long)RT_fast_call_symbol_6));
  ht_names->put(make_simple_string("RT_fast_call_symbol"),
                make_integer((unsigned long)RT_fast_call_symbol));

  ht_names->put(make_simple_string("RT_current_thread_call_function_0"),
                make_integer((unsigned long)RT_current_thread_call_function_0));
  ht_names->put(make_simple_string("RT_current_thread_call_function_1"),
                make_integer((unsigned long)RT_current_thread_call_function_1));
  ht_names->put(make_simple_string("RT_current_thread_call_function_2"),
                make_integer((unsigned long)RT_current_thread_call_function_2));
  ht_names->put(make_simple_string("RT_current_thread_call_function_3"),
                make_integer((unsigned long)RT_current_thread_call_function_3));
  ht_names->put(make_simple_string("RT_current_thread_call_function_4"),
                make_integer((unsigned long)RT_current_thread_call_function_4));
  ht_names->put(make_simple_string("RT_current_thread_call_function_5"),
                make_integer((unsigned long)RT_current_thread_call_function_5));
  ht_names->put(make_simple_string("RT_current_thread_call_function_6"),
                make_integer((unsigned long)RT_current_thread_call_function_6));
  ht_names->put(make_simple_string("RT_current_thread_call_function"),
                make_integer((unsigned long)RT_current_thread_call_function));

  ht_names->put(make_simple_string("RT_thread_call_function_0"),
                make_integer((unsigned long)RT_thread_call_function_0));
  ht_names->put(make_simple_string("RT_thread_call_function_1"),
                make_integer((unsigned long)RT_thread_call_function_1));
  ht_names->put(make_simple_string("RT_thread_call_function_2"),
                make_integer((unsigned long)RT_thread_call_function_2));
  ht_names->put(make_simple_string("RT_thread_call_function_3"),
                make_integer((unsigned long)RT_thread_call_function_3));
  ht_names->put(make_simple_string("RT_thread_call_function_4"),
                make_integer((unsigned long)RT_thread_call_function_4));
  ht_names->put(make_simple_string("RT_thread_call_function_5"),
                make_integer((unsigned long)RT_thread_call_function_5));
  ht_names->put(make_simple_string("RT_thread_call_function_6"),
                make_integer((unsigned long)RT_thread_call_function_6));
  ht_names->put(make_simple_string("RT_thread_call_function"),
                make_integer((unsigned long)RT_thread_call_function));

  ht_names->put(make_simple_string("RT_fast_call_function_0"),
                make_integer((unsigned long)RT_fast_call_function_0));
  ht_names->put(make_simple_string("RT_fast_call_function_1"),
                make_integer((unsigned long)RT_fast_call_function_1));
  ht_names->put(make_simple_string("RT_fast_call_function_2"),
                make_integer((unsigned long)RT_fast_call_function_2));
  ht_names->put(make_simple_string("RT_fast_call_function_3"),
                make_integer((unsigned long)RT_fast_call_function_3));
  ht_names->put(make_simple_string("RT_fast_call_function_4"),
                make_integer((unsigned long)RT_fast_call_function_4));
  ht_names->put(make_simple_string("RT_fast_call_function_5"),
                make_integer((unsigned long)RT_fast_call_function_5));
  ht_names->put(make_simple_string("RT_fast_call_function_6"),
                make_integer((unsigned long)RT_fast_call_function_6));
  ht_names->put(make_simple_string("RT_fast_call_function"),
                make_integer((unsigned long)RT_fast_call_function));

  ht_names->put(make_simple_string("RT_thread_funcall_0"),
                make_integer((unsigned long)RT_thread_funcall_0));
  ht_names->put(make_simple_string("RT_thread_funcall_1"),
                make_integer((unsigned long)RT_thread_funcall_1));
  ht_names->put(make_simple_string("RT_thread_funcall_2"),
                make_integer((unsigned long)RT_thread_funcall_2));
  ht_names->put(make_simple_string("RT_thread_funcall_3"),
                make_integer((unsigned long)RT_thread_funcall_3));
  ht_names->put(make_simple_string("RT_thread_funcall_4"),
                make_integer((unsigned long)RT_thread_funcall_4));

  ht_names->put(make_simple_string("RT_fast_funcall_0"),
                make_integer((unsigned long)RT_fast_funcall_0));
  ht_names->put(make_simple_string("RT_fast_funcall_1"),
                make_integer((unsigned long)RT_fast_funcall_1));
  ht_names->put(make_simple_string("RT_fast_funcall_2"),
                make_integer((unsigned long)RT_fast_funcall_2));
  ht_names->put(make_simple_string("RT_fast_funcall_3"),
                make_integer((unsigned long)RT_fast_funcall_3));
  ht_names->put(make_simple_string("RT_fast_funcall_4"),
                make_integer((unsigned long)RT_fast_funcall_4));

  ht_names->put(make_simple_string("RT_current_thread_symbol_value"),
                make_integer((unsigned long)RT_current_thread_symbol_value));

  ht_names->put(make_simple_string("RT_thread_symbol_value"),
                make_integer((unsigned long)RT_thread_symbol_value));

  ht_names->put(make_simple_string("RT_current_thread_get_values"),
                make_integer((unsigned long)RT_current_thread_get_values));

  ht_names->put(make_simple_string("RT_thread_get_values"),
                make_integer((unsigned long)RT_thread_get_values));

  ht_names->put(make_simple_string("RT_thread_set_values_1"),
                make_integer((unsigned long)RT_thread_set_values_1));

  ht_names->put(make_simple_string("RT_current_thread_set_values_0"),
                make_integer((unsigned long)RT_current_thread_set_values_0));

  ht_names->put(make_simple_string("RT_current_thread_set_values_2"),
                make_integer((unsigned long)RT_current_thread_set_values_2));

  ht_names->put(make_simple_string("RT_thread_set_values_2"),
                make_integer((unsigned long)RT_thread_set_values_2));

  ht_names->put(make_simple_string("RT_current_thread_set_values_3"),
                make_integer((unsigned long)RT_current_thread_set_values_3));

  ht_names->put(make_simple_string("RT_thread_set_values_3"),
                make_integer((unsigned long)RT_thread_set_values_3));

  ht_names->put(make_simple_string("RT_current_thread_set_values_4"),
                make_integer((unsigned long)RT_current_thread_set_values_4));

  ht_names->put(make_simple_string("RT_thread_set_values_4"),
                make_integer((unsigned long)RT_thread_set_values_4));

  ht_names->put(make_simple_string("RT_current_thread_clear_values"),
                make_integer((unsigned long)RT_current_thread_clear_values));

  ht_names->put(make_simple_string("RT_thread_clear_values"),
                make_integer((unsigned long)RT_thread_clear_values));

  ht_names->put(make_simple_string("RT_current_thread"),
                make_integer((unsigned long)RT_current_thread));

  ht_names->put(make_simple_string("RT_current_thread_last_special_binding"),
                make_integer((unsigned long)RT_current_thread_last_special_binding));

  ht_names->put(make_simple_string("RT_thread_last_special_binding"),
                make_integer((unsigned long)RT_thread_last_special_binding));

  ht_names->put(make_simple_string("RT_current_thread_set_last_special_binding"),
                make_integer((unsigned long)RT_current_thread_set_last_special_binding));

  ht_names->put(make_simple_string("RT_thread_set_last_special_binding"),
                make_integer((unsigned long)RT_thread_set_last_special_binding));

  ht_names->put(make_simple_string("RT_current_thread_bind_special"),
                make_integer((unsigned long)RT_current_thread_bind_special));

  ht_names->put(make_simple_string("RT_thread_bind_special"),
                make_integer((unsigned long)RT_thread_bind_special));

  ht_names->put(make_simple_string("RT_current_thread_bind_special_to_current_value"),
                make_integer((unsigned long)RT_current_thread_bind_special_to_current_value));

  ht_names->put(make_simple_string("RT_thread_bind_special_to_current_value"),
                make_integer((unsigned long)RT_thread_bind_special_to_current_value));

  ht_names->put(make_simple_string("RT_current_thread_set_symbol_value"),
                make_integer((unsigned long)RT_current_thread_set_symbol_value));

  ht_names->put(make_simple_string("RT_thread_set_symbol_value"),
                make_integer((unsigned long)RT_thread_set_symbol_value));

  ht_names->put(make_simple_string("RT_symbol_function"),
                make_integer((unsigned long)RT_symbol_function));

  ht_names->put(make_simple_string("RT_symbol_setf_function"),
                make_integer((unsigned long)RT_symbol_setf_function));

  ht_names->put(make_simple_string("RT_make_compiled_closure"),
                make_integer((unsigned long)RT_make_compiled_closure));

  ht_names->put(make_simple_string("RT_make_compiled_closure_2"),
                make_integer((unsigned long)RT_make_compiled_closure_2));

  ht_names->put(make_simple_string("RT_make_value_cell"),
                make_integer((unsigned long)RT_make_value_cell));

  ht_names->put(make_simple_string("RT_make_value_cell_1"),
                make_integer((unsigned long)RT_make_value_cell_1));

  ht_names->put(make_simple_string("RT_value_cell_value"),
                make_integer((unsigned long)RT_value_cell_value));

  ht_names->put(make_simple_string("RT_set_value_cell_value"),
                make_integer((unsigned long)RT_set_value_cell_value));

  ht_names->put(make_simple_string("RT_allocate_closure_data_vector"),
                make_integer((unsigned long)RT_allocate_closure_data_vector));

  ht_names->put(make_simple_string("RT_copy_closure_data_vector"),
                make_integer((unsigned long)RT_copy_closure_data_vector));

  ht_names->put(make_simple_string("RT_unshare_variable"),
                make_integer((unsigned long)RT_unshare_variable));

  ht_names->put(make_simple_string("RT_restify"),
                make_integer((unsigned long)RT_restify));

  ht_names->put(make_simple_string("RT_process_args"),
                make_integer((unsigned long)RT_process_args));

  ht_names->put(make_simple_string("RT_thread_multiple_value_call"),
                make_integer((unsigned long)RT_thread_multiple_value_call));

  ht_names->put(make_simple_string("RT_accumulate_values"),
                make_integer((unsigned long)RT_accumulate_values));

  ht_names->put(make_simple_string("RT_multiple_value_list"),
                make_integer((unsigned long)RT_multiple_value_list));

  ht_names->put(make_simple_string("RT_equal"),
                make_integer((unsigned long)RT_equal));

  ht_names->put(make_simple_string("RT_eql"),
                make_integer((unsigned long)RT_eql));

  ht_names->put(make_simple_string("RT_equals"),
                make_integer((unsigned long)RT_equals));

  ht_names->put(make_simple_string("RT_classp"),
                make_integer((unsigned long)RT_classp));

  ht_names->put(make_simple_string("RT_endp"),
                make_integer((unsigned long)RT_endp));

  ht_names->put(make_simple_string("RT_functionp"),
                make_integer((unsigned long)RT_functionp));

  ht_names->put(make_simple_string("RT_integerp"),
                make_integer((unsigned long)RT_integerp));

  ht_names->put(make_simple_string("RT_listp"),
                make_integer((unsigned long)RT_listp));

  ht_names->put(make_simple_string("RT_minusp"),
                make_integer((unsigned long)RT_minusp));

  ht_names->put(make_simple_string("RT_numberp"),
                make_integer((unsigned long)RT_numberp));

  ht_names->put(make_simple_string("RT_plusp"),
                make_integer((unsigned long)RT_plusp));

  ht_names->put(make_simple_string("RT_stringp"),
                make_integer((unsigned long)RT_stringp));

  ht_names->put(make_simple_string("RT_vectorp"),
                make_integer((unsigned long)RT_vectorp));

  ht_names->put(make_simple_string("RT_fast_apply_function_2"),
                make_integer((unsigned long)RT_fast_apply_function_2));

  ht_names->put(make_simple_string("RT_fast_apply_2"),
                make_integer((unsigned long)RT_fast_apply_2));

  ht_names->put(make_simple_string("RT_handle_interrupt"),
                make_integer((unsigned long)RT_handle_interrupt));

  ht_names->put(make_simple_string("interrupted"),
                make_integer((unsigned long)&interrupted));

#ifdef WIN32
  ht_names->put(make_simple_string("setjmp"),
                make_integer((unsigned long)_setjmp));
#else
  ht_names->put(make_simple_string("setjmp"),
                make_integer((unsigned long)setjmp));
#endif

  ht_names->put(make_simple_string("RT_enter_block"),
                make_integer((unsigned long)RT_enter_block));

  ht_names->put(make_simple_string("RT_frame_jmp"),
                make_integer((unsigned long)RT_frame_jmp));

  ht_names->put(make_simple_string("RT_leave_block"),
                make_integer((unsigned long)RT_leave_block));

  ht_names->put(make_simple_string("RT_return_from"),
                make_integer((unsigned long)RT_return_from));

  ht_names->put(make_simple_string("RT_block_non_local_return"),
                make_integer((unsigned long)RT_block_non_local_return));

  ht_names->put(make_simple_string("RT_enter_catch"),
                make_integer((unsigned long)RT_enter_catch));

  ht_names->put(make_simple_string("RT_leave_catch"),
                make_integer((unsigned long)RT_leave_catch));

  ht_names->put(make_simple_string("RT_caught_throw"),
                make_integer((unsigned long)RT_caught_throw));

  ht_names->put(make_simple_string("RT_throw"),
                make_integer((unsigned long)RT_throw));

  ht_names->put(make_simple_string("RT_add_tagbody"),
                make_integer((unsigned long)RT_add_tagbody));

  ht_names->put(make_simple_string("RT_add_tag"),
                make_integer((unsigned long)RT_add_tag));

  ht_names->put(make_simple_string("RT_leave_tagbody"),
                make_integer((unsigned long)RT_leave_tagbody));

  ht_names->put(make_simple_string("RT_non_local_go"),
                make_integer((unsigned long)RT_non_local_go));

  ht_names->put(make_simple_string("RT_thread_copy_values"),
                make_integer((unsigned long)RT_thread_copy_values));

  ht_names->put(make_simple_string("RT_thread_set_values"),
                make_integer((unsigned long)RT_thread_set_values));

  ht_names->put(make_simple_string("RT_enter_unwind_protect"),
                make_integer((unsigned long)RT_enter_unwind_protect));

  ht_names->put(make_simple_string("RT_leave_unwind_protect"),
                make_integer((unsigned long)RT_leave_unwind_protect));

  ht_names->put(make_simple_string("RT_unwind_to"),
                make_integer((unsigned long)RT_unwind_to));

#ifndef __x86_64__
  ht_names->put(make_simple_string("RT_unsigned_byte_to_raw_ub32"),
                make_integer((unsigned long)RT_unsigned_byte_to_raw_ub32));
#endif

#ifndef __x86_64__
  ht_names->put(make_simple_string("RT_mod32"),
                make_integer((unsigned long)RT_mod32));
#endif

#ifdef __x86_64__
  ht_names->put(make_simple_string("RT_unsigned_bignum_to_raw_ub64"),
                make_integer((unsigned long)RT_unsigned_bignum_to_raw_ub64));
#endif

  ht_names->put(make_simple_string("RT_progv_bind_vars"),
                make_integer((unsigned long)RT_progv_bind_vars));

  ht_names->put(make_simple_string("RT_bad_index"),
                make_integer((unsigned long)RT_bad_index));

  ht_names->put(make_simple_string("RT_gethash2"),
                make_integer((unsigned long)RT_gethash2));

  ht_names->put(make_simple_string("RT_gethash3"),
                make_integer((unsigned long)RT_gethash3));

  ht_names->put(make_simple_string("RT_thread_uwp_in_cleanup_p"),
                make_integer((unsigned long)RT_thread_uwp_in_cleanup_p));

  the_symbol(S_runtime_names)->initialize_constant(make_value(ht_names));
}
