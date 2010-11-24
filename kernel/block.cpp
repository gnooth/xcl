// block.cpp
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

#include "lisp.hpp"
#include "Frame.hpp"
#include "Environment.hpp"
#include "runtime.h"
#include "primitives.hpp"

// ### block
Value CL_block(Value args, Environment * env, Thread * thread)
{
  if (args == NIL)
    return wrong_number_of_arguments(S_block, 0, 1, MANY);
  Value block_name = xcar(args);
  if (!symbolp(block_name))
    signal_type_error(block_name, S_symbol);
  Value body = xcdr(args);
  Block * block = thread->add_block(block_name);
  if (setjmp(*block->jmp()) == 0)
    {
      // implicit PROGN
      Value result = NIL;
      Environment * ext = new Environment(env);
      while (body != NIL)
        {
          result = eval(car(body), ext, thread);
          body = xcdr(body);
        }
      thread->set_last_control_frame(block->last_control_frame());
      assert(thread->stack() == block->stack());
      assert(thread->call_depth() == block->call_depth());
      return result;
    }
  else
    {
      // caught RETURN-FROM
      return RT_block_non_local_return(thread, block);
    }
}

// ### return-from
Value CL_return_from(Value args, Environment * env, Thread * thread)
{
  INDEX numargs = length(args);
  if (numargs < 1 || numargs > 2)
    wrong_number_of_arguments(S_return_from, numargs, 1, 2);
  Value block_name = xcar(args);
  Value result;
  if (numargs == 2)
    result = eval(xcadr(args), env, thread);
  else
    result = NIL;
  RT_return_from(thread, block_name, result);
  // noreturn
}

// ### return
Value CL_return(Value args, Environment * env, Thread * thread)
{
  INDEX numargs = length(args);
  if (numargs > 1)
    wrong_number_of_arguments(S_return, numargs, 0, 1);
  Value block_name = NIL;
  Value result;
  if (numargs == 1)
    result = eval(xcar(args), env, thread);
  else
    result = NIL;
  RT_return_from(thread, block_name, result);
  // noreturn
}

Block * RT_enter_block(Thread * thread, Value block_name)
{
  return thread->add_block(block_name);
}

void RT_leave_block(Thread * thread, Block * block)
{
  thread->set_last_control_frame(block->last_control_frame());
}

Value RT_block_non_local_return(Thread * thread, Block * block)
{
  thread->set_stack(block->stack());
  thread->set_call_depth(block->call_depth());
  thread->set_last_control_frame(block->last_control_frame());
  thread->set_last_tag(block->last_tag());
  int values_length = thread->values_length();
  assert(values_length >= -1);
  assert(values_length < MULTIPLE_VALUES_LIMIT);
  if (values_length == 0)
    return NIL;
  Value result = thread->nth_value(0);
  if (values_length == 1)
    thread->clear_values();
  return result;
}

// This function should be called immediately after evaluating the result form
// (if any), so that the thread's values vector is set up correctly.
void RT_return_from(Thread * thread, Value block_name, Value result)
{
  if (!symbolp(block_name))
    signal_type_error(block_name, S_symbol);
  Block * block = thread->find_block(block_name);
  if (!block)
    {
      String * s = new String("No block named ");
      s->append(the_symbol(block_name)->name());
      s->append(" is currently visible.");
      signal_lisp_error(new Error(s));
    }
  if (thread->values_length() < 0)
    // single return value
    thread->set_values(result);
  // unwind stack, calling unwind-protect cleanups
  RT_unwind_to(block, thread);
  longjmp(*block->jmp(), 1);
  // noreturn
}
