// unwind-protect.cpp
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
#include "Frame.hpp"
#include "Environment.hpp"
#include "runtime.h"
#include "UnwindProtect.hpp"

// ### unwind-protect protected-form cleanup-form* => result*
Value CL_unwind_protect(Value args, Environment * env, Thread * thread)
{
  if (args == NIL)
    return wrong_number_of_arguments(S_unwind_protect, 0, 1, MANY);

  Value protected_form = car(args);
  Value cleanup_forms = cdr(args);

#ifndef NDEBUG
  UnwindProtect * old_uwp = thread->unwind_protect();
  Frame * old_control_frame = thread->last_control_frame();
#endif

  // establish a new context
  UnwindProtect * new_uwp = new UnwindProtect(cleanup_forms,
                                              env,
                                              thread->last_control_frame());
  thread->set_unwind_protect(new_uwp);
  thread->add_frame(new_uwp);

  // evaluate protected form
  const Value result = eval(protected_form, env, thread);

  // save values
  Values * values = RT_thread_copy_values(thread, result);

  // "If a non-local exit occurs during execution of cleanup-forms, no special
  // action is taken. The cleanup-forms of UNWIND-PROTECT are not protected by
  // that UNWIND-PROTECT."
  RT_leave_unwind_protect(thread, new_uwp);
  assert(thread->unwind_protect() == old_uwp);
  assert(thread->last_control_frame() == old_control_frame);

  // run cleanup forms
  while (cleanup_forms != NIL)
    {
      eval(car(cleanup_forms), env, thread);
      cleanup_forms = xcdr(cleanup_forms);
    }

  // restore values
  RT_thread_set_values(thread, values);

  return result;
}

Values * RT_thread_copy_values(Thread * thread, Value primary_value)
{
  return thread->copy_values(primary_value);
}

Value RT_thread_set_values(Thread * thread, Values * values)
{
  return thread->set_values(values);
}

UnwindProtect * RT_enter_unwind_protect(Thread * thread, void * code, long rbp)
{
  UnwindProtect * uwp = new UnwindProtect(code,
                                          rbp,
                                          thread->last_control_frame());
  thread->set_unwind_protect(uwp);
  thread->add_frame(uwp);
  return uwp;
}

void RT_leave_unwind_protect(Thread * thread, UnwindProtect * uwp)
{
  thread->set_unwind_protect(uwp->unwind_protect());
  thread->set_last_control_frame(uwp->last_control_frame());
}
