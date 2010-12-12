// tagbody.cpp
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

#include "lisp.hpp"
#include "Frame.hpp"
#include "Environment.hpp"
#include "runtime.h"

inline void go_to_visible_tag(Tag * tag, Thread * thread)
{
  assert(tag != NULL);
  thread->clear_values(); // REVIEW
  Tagbody * tagbody = tag->tagbody();
  // unwind stack, calling unwind-protect cleanups
  RT_unwind_to(tagbody, thread);
  assert(tag->index() > 0);
  LONGJMP(*tagbody->jmp(), tag->index());
}

// ### tagbody
Value CL_tagbody(Value args, Environment * env, Thread * thread)
{
  Environment * ext = new Environment(env);
  Tagbody * tagbody = new Tagbody(thread);
  Value body = args;
  // The tag index is 1-based so it can be used as the second argument to longjmp().
  int tag_index = 1;
  while (body != NIL)
    {
      Value current = car(body);
      body = xcdr(body);
      if (consp(current))
        continue;
      if (symbolp(current) || integerp(current))
        // it's a tag
        thread->add_tag(current, tagbody, body, tag_index++);
    }
  Tag * last_tag = thread->last_tag();
  Value remaining;
  int retval = SETJMP(*tagbody->jmp());
  if (retval == 0)
    {
      assert(thread->stack() == tagbody->stack());
      assert(thread->call_depth() == tagbody->call_depth());
      remaining = args;
    }
  else
    {
      // reached here because of a (possibly non-local) GO to a tag in this tagbody
      assert(thread->stack() == tagbody->stack());
      assert(thread->call_depth() == tagbody->call_depth());
      thread->set_last_tag(last_tag);
      Tag * tag = thread->find_tag(tagbody, retval);
      assert(tag != NULL);
      remaining = tag->continuation();
    }
  while (remaining != NIL)
    {
      Value current = xcar(remaining);
      if (consp(current))
        {
          if (xcar(current) == S_go)
            {
              if (interrupted)
                RT_handle_interrupt();
              Tag * tag = thread->find_tag(car(xcdr(current)));
              if (!tag)
                {
                  String * string = new String("No tag named ");
                  string->append(prin1_to_string(car(xcdr(current))));
                  string->append(" is currently visible.");
                  return signal_lisp_error(string);
                }
              if (tag->tagbody() == tagbody)
                {
                  remaining = tag->continuation();
                  continue;
                }
              go_to_visible_tag(tag, thread);
            }
          eval(current, ext, thread);
        }
      remaining = xcdr(remaining);
    }
  thread->set_last_tag(tagbody->last_tag());
  thread->clear_values();
  return NIL;
}

// ### go
Value CL_go(Value args, Environment * env, Thread * thread)
{
  if (interrupted)
    RT_handle_interrupt();
  Tag * tag = thread->find_tag(xcar(args));
  if (!tag)
    {
      String * string = new String("No tag named ");
      string->append(prin1_to_string(xcar(args)));
      string->append(" is currently visible.");
      return signal_lisp_error(string);
    }
  go_to_visible_tag(tag, thread);
  // not reached
  return NIL;
}

Tagbody * RT_add_tagbody(Thread * thread)
{
  Tagbody * tagbody = new Tagbody(thread);
  thread->add_frame(tagbody);
  return tagbody;
}

void RT_add_tag(Thread * thread, Value tag_name, Tagbody * tagbody, int index)
{
  thread->add_tag(tag_name, tagbody, index);
}

void RT_leave_tagbody(Thread * thread, Tagbody * tagbody)
{
  thread->set_last_tag(tagbody->last_tag());
  thread->set_last_control_frame(tagbody->last_control_frame());
}

void RT_non_local_go(Thread * thread, Value tag_name)
{
  if (interrupted)
    RT_handle_interrupt();
  Tag * tag = thread->find_tag(tag_name);
  if (!tag)
    {
      String * string = new String("No tag named ");
      string->append(prin1_to_string(tag_name));
      string->append(" is currently visible.");
      signal_lisp_error(string);
    }
  thread->clear_values(); // REVIEW
  Tagbody * tagbody = tag->tagbody();
  // unwind stack, calling unwind-protect cleanups
  RT_unwind_to(tagbody, thread);
  assert(thread->last_control_frame() == tagbody);
  LONGJMP(*tagbody->jmp(),
          // "If longjmp is invoked with a second argument of 0, 1 will be returned instead."
          tag->index() + 1);
}
