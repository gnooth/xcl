// Function.cpp
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
#include "primitives.hpp"
#include "PrintNotReadable.hpp"

Value Function::type_of() const
{
  long widetag = this->widetag();
  if (widetag == WIDETAG_PRIMITIVE || widetag == WIDETAG_COMPILED_CLOSURE
      || widetag == WIDETAG_COMPILED_FUNCTION)
    return S_compiled_function;
  return S_function;
}

Value Function::class_of() const
{
  return C_function;
}

bool Function::typep(Value type) const
{
  if (type == S_compiled_function)
    {
      long widetag = this->widetag();
      return (widetag == WIDETAG_PRIMITIVE || widetag == WIDETAG_COMPILED_CLOSURE
              || widetag == WIDETAG_COMPILED_FUNCTION);
    }
  return (type == S_function || type == S_atom || type == T || type == C_function
          || type == C_t);
}

Value Function::parts()
{
  String * description = new String(prin1_to_string());
  description->append_char('\n');
  Value elements = NIL;
  Value name = operator_name();
  elements = make_cons(make_cons(make_simple_string("NAME"),
                                 name != NULL_VALUE ? name : NIL),
                       elements);
  elements = make_cons(make_cons(make_simple_string("ARITY"),
                                 make_fixnum(arity())),
                       elements);
  elements = make_cons(make_cons(make_simple_string("MINARGS"),
                                 make_fixnum(minargs())),
                       elements);
  elements = make_cons(make_cons(make_simple_string("MAXARGS"),
                                 make_fixnum(maxargs())),
                       elements);
  return current_thread()->set_values(make_value(description), T, CL_nreverse(elements));
}

AbstractString * Function::write_to_string()
{
  Value name = operator_name();
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_readably) != NIL)
    {
      if (symbolp(name) || is_valid_setf_function_name(name))
        {
          String * s = new String();
          s->append("#.(");
          s->append(the_symbol(S_coerce_to_function)->prin1_to_string());
          s->append(" '");
          s->append(::prin1_to_string(name));
          s->append_char(')');
          return s;
        }
      signal_lisp_error(new PrintNotReadable(make_value(this)));
      // not reached
      return NULL;
    }
  String * s = new String();
  s->append(the_symbol(S_function)->write_to_string());
  if (name != NULL_VALUE)
    {
      s->append_char(' ');
      void* last_special_binding = thread->last_special_binding();
      thread->bind_special(S_print_length, NIL);
      thread->bind_special(S_print_level, NIL);
      s->append(::prin1_to_string(name));
      thread->set_last_special_binding(last_special_binding);
    }
  return unreadable_string(s);
}

// ### function-plist
Value SYS_function_plist(Value arg)
{
  return check_function(arg)->plist();
}

// ### set-function-plist
Value SYS_set_function_plist(Value function, Value plist)
{
  check_function(function)->set_plist(plist);
  return plist;
}

// ### function-call-count
Value SYS_function_call_count(Value arg)
{
  if (functionp(arg))
    return make_unsigned_integer(the_function(arg)->call_count());
  return signal_type_error(arg, S_function);
}

// ### set-function-call-count
Value SYS_set_function_call_count(Value arg1, Value arg2)
{
  if (functionp(arg1))
    the_function(arg1)->set_call_count(check_index(arg2));
  return signal_type_error(arg1, S_function);
}
