// catch.cpp
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
#include "primitives.hpp"
#include "runtime.h"
#include "ControlError.hpp"
#include "Environment.hpp"

// ### catch
Value CL_catch(Value args, Environment * env, Thread * thread)
{
  if (args == NIL)
    return wrong_number_of_arguments(S_catch, 0, 1, MANY);
  Value tag = eval(xcar(args), env, thread);
  Value body = xcdr(args);
#ifndef NDEBUG
  StackFrame * saved_stack = thread->stack();
  unsigned int saved_call_depth = thread->call_depth();
#endif
  Catch * catch_frame = thread->add_catch_frame(tag); // RT_enter_catch
  assert(catch_frame->type() == CATCH);
  if (setjmp(*catch_frame->jmp()) == 0)
    {
      // implicit PROGN
      Value result = NIL;
      Environment * ext = new Environment(env);
      while (body != NIL)
        {
          result = eval(car(body), ext, thread);
          body = xcdr(body);
        }
      assert(thread->stack() == saved_stack);
      assert(thread->call_depth() == saved_call_depth);
      thread->set_last_control_frame(catch_frame->last_control_frame()); // RT_leave_catch
      return result;
    }
  else
    {
      // caught THROW
//       thread->set_stack(saved_stack);
//       thread->set_call_depth(saved_call_depth);
//       thread->set_last_control_frame(catch_frame->last_control_frame()); // added
//       thread->set_last_tag(catch_frame->last_tag());
//       int values_length = thread->values_length();
//       assert(values_length >= 0);
//       if (values_length == 0)
//         return NIL;
//       Value result = thread->nth_value(0);
//       if (values_length == 1)
//         thread->clear_values();
//       return result;
      return RT_caught_throw(thread, catch_frame);
    }
}

Catch * RT_enter_catch(Thread * thread, Value tag)
{
//   printf("RT_enter_catch called\n");
//   fflush(stdout);
  return thread->add_catch_frame(tag);
}

void RT_leave_catch(Thread * thread, Catch * catch_frame)
{
//   printf("RT_leave_catch called\n");
//   fflush(stdout);
  thread->set_last_control_frame(catch_frame->last_control_frame());
}

// ### throw
Value CL_throw(Value args, Environment * env, Thread * thread)
{
  INDEX numargs = length(args);
  if (numargs != 2)
    wrong_number_of_arguments(S_throw, numargs, 2, 2);
  Value tag = eval(xcar(args), env, thread);
  Value result = eval(xcadr(args), env, thread);
  RT_throw(thread, tag, result);
  // noreturn
}

Value RT_caught_throw(Thread * thread, Catch * catch_frame)
{
  thread->set_stack(catch_frame->stack());
  thread->set_call_depth(catch_frame->call_depth());
  thread->set_last_control_frame(catch_frame->last_control_frame());
  thread->set_last_tag(catch_frame->last_tag());
  int values_length = thread->values_length();
  assert(values_length >= 0);
  if (values_length == 0)
    return NIL;
  Value result = thread->nth_value(0);
  if (values_length == 1)
    thread->clear_values();
  return result;
}

// This function should be called immediately after evaluating the result form,
// so that the thread's values vector is set up correctly.
void RT_throw(Thread * thread, Value tag, Value result)
{
  Frame * frame = thread->find_catch_frame(tag);
  if (!frame)
    {
      String * s = new String("Attempt to throw to the nonexistent tag ");
      s->append(::prin1_to_string(tag));
      s->append(".");
      signal_lisp_error(new ControlError(s));
    }
  assert(frame->type() == CATCH);
  if (thread->values_length() < 0)
    // single return value
    thread->set_values(result);
  // unwind stack, calling unwind-protect cleanups
  RT_unwind_to(frame, thread);
  longjmp(*frame->jmp(), 1);
  // noreturn
}
