// do.cpp
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
#include "runtime.h"
#include "primitives.hpp"
#include "Environment.hpp"

static Value do_internal(Value args, Environment * env, Thread * thread, bool sequential)
{
  Value varlist = car(args);
  Value second = CL_cadr(args);
  Value end_test_form = car(second);
  Value result_forms = cdr(second);
  Value body = CL_cddr(args);
  // process variable specifications
  const INDEX numvars = length(varlist);
  Value * vars      = new (GC) Value[numvars];
  Value * initforms = new (GC) Value[numvars];
  Value * stepforms = new (GC) Value[numvars];
  for (INDEX i = 0; i < numvars; i++)
    {
      Value varspec = xcar(varlist);
      if (consp(varspec))
        {
          Value var = xcar(varspec);
          if (symbolp(var))
            vars[i] = var;
          else
            return signal_type_error(var, S_symbol);
          initforms[i] = CL_cadr(varspec);
          // is there a step form?
          if (CL_cddr(varspec) != NIL)
            stepforms[i] = CL_caddr(varspec);
          else
            stepforms[i] = NULL_VALUE;
        }
      else
        {
          // not a cons, must be a symbol
          if (symbolp(varspec))
            vars[i] = varspec;
          else
            return signal_type_error(varspec, S_symbol);
          initforms[i] = NIL;
          stepforms[i] = NULL_VALUE;
        }
      varlist = xcdr(varlist);
    }
  StackFrame * saved_stack = thread->stack();
  int saved_call_depth = thread->call_depth();
  void * last_special_binding = thread->last_special_binding();
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
                  Value names = xcdr(decl);
                  while (names != NIL)
                    {
                      specials = make_cons(car(names), specials);
                      names = xcdr(names);
                    }
                }
              decls = xcdr(decls);
            }
          body = xcdr(body);
        }
      else
        break;
    }
  Environment * ext = new Environment(env);
  for (INDEX i = 0; i < numvars; i++)
    {
      Value var = vars[i];
      Value value = eval(initforms[i], (sequential ? ext : env), thread);
      if (specials != NIL && memq(var, specials))
        thread->bind_special(var, value);
      else if (the_symbol(var)->is_special_variable())
        thread->bind_special(var, value);
      else
        ext->bind(var, value);
    }
  Value list = specials;
  while (list != NIL)
    {
      ext->declare_special(xcar(list));
      list = xcdr(list);
    }
  // "An implicit block named NIL surrounds the entire DO (or DO*) form."
  Block * block = thread->add_block(NIL);
  Value result;
  if (SETJMP(*block->jmp()) == 0)
    {
      // tagbody
      Tagbody * tagbody = thread->get_tagbody();
      // look for tags
      Value remaining = body;
      // The tag index is 1-based so it can be used as the second argument to longjmp().
      int tag_index = 1;
      while (remaining != NIL)
        {
          Value current = car(remaining);
          remaining = xcdr(remaining);
          if (consp(current))
            continue;
          // it's a tag
          thread->add_tag(current, tagbody, remaining, tag_index++);
        }
      while (true)
        {
          // execute body
          // test for termination
          if (eval(end_test_form, ext, thread) != NIL)
            break;
          Value remaining;
          int retval = SETJMP(*tagbody->jmp());
          if (retval == 0)
            remaining = body;
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
              Value current = car(remaining);
              if (consp(current))
                eval(current, ext, thread);
              remaining = xcdr(remaining);
            }
          // update variables
          if (sequential)
            {
              for (INDEX i = 0; i < numvars; i++)
                {
                  Value stepform = stepforms[i];
                  if (stepform != NULL_VALUE)
                    {
                      Value var = vars[i];
                      Value value = eval(stepform, ext, thread);
                      if (the_symbol(var)->is_special_variable()
                          || ext->is_declared_special(var))
                        thread->set_symbol_value(var, value);
                      else
                        ext->rebind(var, value);
                    }
                }
            }
          else
            {
              // evaluate step forms
              Value * results = new (GC) Value[numvars];
              for (INDEX i = 0; i < numvars; i++)
                {
                  Value stepform = stepforms[i];
                  if (stepform != NULL_VALUE)
                    {
                      results[i] = eval(stepform, ext, thread);
                    }
                  else
                    results[i] = NULL_VALUE;
                }
              // update variables
              for (INDEX i = 0; i < numvars; i++)
                {
                  if (results[i] != NULL_VALUE)
                    {
                      Value var = vars[i];
                      Value value = results[i];
                      if (the_symbol(var)->is_special_variable()
                          || ext->is_declared_special(var))
                        thread->set_symbol_value(var, value);
                      else
                        ext->rebind(var, value);
                    }
                }
            }
          if (interrupted)
            RT_handle_interrupt();
        }
      thread->set_last_tag(tagbody->last_tag());
      thread->set_last_control_frame(tagbody->last_control_frame());
      thread->release_frame(tagbody);
      // process result forms
      result = progn(result_forms, ext, thread);
      thread->set_stack(saved_stack);
      thread->set_call_depth(saved_call_depth);
    }
  else
    {
      // caught RETURN
      result = RT_block_non_local_return(thread, block);
    }
  thread->set_last_special_binding(last_special_binding);
  thread->set_last_control_frame(block->last_control_frame());
  thread->release_frame(block);
  return result;
}

// ### do
Value CL_do(Value args, Environment * env, Thread * thread)
{
  return do_internal(args, env, thread, false);
}

// ### do*
Value CL_do_star(Value args, Environment * env, Thread * thread)
{
  return do_internal(args, env, thread, true);
}
