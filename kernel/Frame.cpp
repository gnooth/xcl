// Frame.cpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

JMP_BUF * RT_frame_jmp(Frame * frame)
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

void RT_unwind_to(Frame * frame, Thread * thread)
{
//   UnwindProtect * current_uwp = thread->unwind_protect();
//   if (current_uwp && current_uwp != frame->unwind_protect())
    {
      // save thread's values
      Values * values = thread->copy_values();
      // run applicable cleanups
      Frame * f = thread->last_control_frame();
      while (f && f != frame)
        {
          if (f->type() == UNWIND_PROTECT)
            {
              UnwindProtect * uwp = reinterpret_cast<UnwindProtect *>(f);
              if (uwp->code() != NULL)
                {
                  thread->set_uwp_in_cleanup(uwp);
                  uwp->run_cleanup_code();
                  thread->set_uwp_in_cleanup(NULL);
                }
              else
                uwp->run_cleanup_forms(thread);
            }
//           f = f->next();
          Frame * next = f->next();
          if (f->type() == BLOCK || f->type() == TAGBODY)
            {
//               printf("RT_unwind_to calling release_frame\n");
              thread->release_frame(f);
            }
          f = next;
        }
      // restore thread's values
      thread->set_values(values);
    }
  restore_frame_context(frame, thread);
}

int RT_thread_uwp_in_cleanup_p(Thread * thread, UnwindProtect * uwp)
{
  return (uwp == thread->uwp_in_cleanup()) ? 1 : 0;
}
