// dolist.cpp
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
#include "runtime.h"
#include "primitives.hpp"
#include "Environment.hpp"

// ### dolist
Value CL_dolist(Value args, Environment * env, Thread * thread)
{
  Value body_form = cdr(args);
  args = car(args);
  Value var = car(args);
  if (!symbolp(var))
    return signal_type_error(var, S_symbol);
  Value list_form = CL_cadr(args);
  Value result_form = CL_caddr(args);
  Environment * ext = new Environment(env);
  void * last_special_binding = NULL;
  // process declarations
  Value specials = NIL;
  while (body_form != NIL)
    {
      Value obj = car(body_form);
      if (consp(obj) && xcar(obj) == S_declare)
        {
          Value decls = xcdr(obj);
          while (decls != NIL)
            {
              Value decl = car(decls);
              if (consp(decl) && xcar(decl) == S_special)
                {
                  Value names = xcdr(decl);
                  while (names != NIL)
                    {
                      specials = make_cons(car(names), specials);
                      names = xcdr(names);
                    }
                }
              decls = xcdr(decls);
            }
          body_form = xcdr(body_form);
        }
      else
        break;
    }
  // "An implicit block named NIL surrounds DOLIST."
  Block * block = thread->add_block(NIL);
  if (SETJMP(*block->jmp()) == 0)
    {
      // evaluate the list form
      Value list = eval(list_form, ext, thread);
      // tagbody
      Tagbody * tagbody = new Tagbody(thread);
      // look for tags
      Value body = body_form;
      // The tag index is 1-based so it can be used as the second argument to longjmp().
      int tag_index = 1;
      while (body != NIL)
        {
          Value current = car(body);
          body = xcdr(body);
          if (consp(current))
            continue;
          // it's a tag
          thread->add_tag(current, tagbody, body, tag_index++);
        }
      // establish a reusable binding
      Binding * binding = NULL;
      if (specials != NIL && memq(var, specials))
        {
          last_special_binding = thread->last_special_binding();
          thread->bind_special(var, NULL_VALUE);
          ext->declare_special(var);
        }
      else if (the_symbol(var)->is_special_variable())
        {
          last_special_binding = thread->last_special_binding();
          thread->bind_special(var, NULL_VALUE);
        }
      else
        {
          ext->bind(var, NULL_VALUE);
          binding = ext->get_binding(var);
        }

      while (specials != NIL)
        {
          ext->declare_special(xcar(specials));
          specials = xcdr(specials);
        }
      // process the list
      while (list != NIL)
        {
          if (binding)
            binding->set_value(car(list));
          else
            thread->set_symbol_value(var, car(list));
          Value remaining;
          int retval = SETJMP(*tagbody->jmp());
          if (retval == 0)
            remaining = body_form;
          else
            {
              // reached here because of a (possibly non-local) GO to a tag in
              // this tagbody
              Tag * tag = thread->find_tag(tagbody, retval);
              assert(tag != NULL);
              remaining = tag->continuation();
            }
          while (remaining != NIL)
            {
              Value current = xcar(remaining);
              if (consp(current))
                eval(current, ext, thread);
              remaining = xcdr(remaining);
            }
          list = xcdr(list);
          if (interrupted)
            RT_handle_interrupt();
        }
      thread->set_last_tag(tagbody->last_tag());
      if (binding)
        binding->set_value(NIL);
      else
        thread->set_symbol_value(var, NIL);
      Value result = eval(result_form, ext, thread);
      thread->set_last_control_frame(block->last_control_frame());
      if (last_special_binding)
        thread->set_last_special_binding(last_special_binding);
      return result;
    }
  else
    {
      // caught RETURN
      if (last_special_binding)
        thread->set_last_special_binding(last_special_binding);
      return RT_block_non_local_return(thread, block);
    }
}
