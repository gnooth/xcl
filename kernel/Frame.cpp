// Frame.cpp
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
#include "UnwindProtect.hpp"
#include "runtime.h"

jmp_buf * RT_frame_jmp(Frame * frame)
{
  return frame->jmp();
}

inline void restore_frame_context(Frame * frame, Thread * thread)
{
  thread->set_last_special_binding(frame->last_special_binding());

  if (frame->type() == TAGBODY)
    thread->set_last_control_frame(frame);
  else
    {
      thread->set_last_control_frame(frame->last_control_frame());
      thread->set_last_tag(frame->last_tag());
    }

  thread->set_unwind_protect(frame->unwind_protect());
  thread->set_stack(frame->stack());
  thread->set_call_depth(frame->call_depth());
}

// FIXME move to Thread object
UnwindProtect * _uwp;

void RT_unwind_to(Frame * frame, Thread * thread)
{
  UnwindProtect * context = thread->unwind_protect();
  if (context && context != frame->unwind_protect())
    {
      // save thread's values
      Values * values = thread->copy_values();

      // run applicable cleanups
      Frame * f = thread->last_control_frame();
      while (f && f != frame)
        {
          if (f->type() == UNWIND_PROTECT)
            {
              if (((UnwindProtect *)f)->code() != NULL)
                {
                  _uwp = (UnwindProtect *) f;
                  Value (*code) () = (Value (*) ()) ((UnwindProtect *)f)->code();
#ifdef __x86_64__
                  long reg = ((UnwindProtect *)f)->rbp();
                  __asm__ __volatile__("push %%rbp\n\t"
                                       "push %%r12\n\t"
                                       "movq %0,%%rbp\n\t"
                                       "call *%1\n\t"
                                       "pop %%r12\n\t"
                                       "pop %%rbp\n\t"
                                       : // no output registers
                                       : "r"(reg), "r"(code) // input
                                       : "rax","rbx","rcx","rdx",
                                         "rsi","rdi","r8","r9",
                                         "r13","r14","r15",
                                         "memory" // clobber list
                                       );
#else
                  int reg = ((UnwindProtect *)f)->ebp();
                  __asm__ __volatile__("push %%ebp\n\t"
                                       "movl %0,%%ebp\n\t"
                                       "call *%1\n\t"
                                       "pop %%ebp\n\t"
                                       : // no output registers
                                       : "r"(reg), "r"(code) // input
                                       : "eax","ebx","ecx","edx" // clobber list
                                       );
#endif
                  _uwp = NULL;
                }
              else
                ((UnwindProtect *)f)->run_cleanup_forms(thread);
            }
          f = f->next();
        }

      // restore thread's values
      thread->set_values(values);
    }

  restore_frame_context(frame, thread);
}

int RT_thread_unwinding_p(Thread * thread, UnwindProtect * uwp)
{
  return (uwp == _uwp) ? 1 : 0;
}
