// PrintNotReadable.cpp
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
#include "runtime.h"
#include "PrintNotReadable.hpp"

Layout * PrintNotReadable::get_layout_for_class()
{
  static Layout * layout;
  if (layout == NULL)
    layout = new Layout(the_class(C_print_not_readable),
                        list3(S_format_control,
                              S_format_arguments,
                              S_object),
                        NIL);
  return layout;
}

void PrintNotReadable::initialize(Value initargs)
{
  Value format_control = NULL_VALUE;
  Value format_arguments = NULL_VALUE;
  Value object = NULL_VALUE;
  Value first, second;
  while (consp(initargs))
    {
      first = xcar(initargs);
      initargs = xcdr(initargs);
      second = car(initargs);
      initargs = cdr(initargs);
      if (first == K_format_control)
        {
          if (format_control == NULL_VALUE)
            format_control = second;
        }
      else if (first == K_format_arguments)
        {
          if (format_arguments == NULL_VALUE)
            format_arguments = second;
        }
      else if (first == K_object)
        {
          if (object == NULL_VALUE)
            object = second;
        }
    }
  if (format_control != NULL_VALUE)
    set_slot_value(S_format_control, format_control);
  if (format_arguments != NULL_VALUE)
    set_slot_value(S_format_arguments, format_arguments);
  set_slot_value(S_object, object != NULL_VALUE ? object : NIL);
}

bool PrintNotReadable::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_print_not_readable || type == S_error || type == S_serious_condition
            || type == S_condition || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_print_not_readable || type == C_error || type == C_serious_condition
            || type == C_condition || type == C_standard_object || type == T);
}

AbstractString * PrintNotReadable::write_to_string()
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_readably) != NIL)
    {
      signal_lisp_error(new PrintNotReadable(make_value(this)));
      return NULL; // not reached
    }
  if (thread->symbol_value(S_print_escape) != NIL)
    return unreadable_string();
  Value format_control = slot_value(S_format_control);
  if (format_control != NIL)
    return check_string(RT_thread_call_symbol_4(thread, S_apply, S_format, NIL, format_control,
                                                slot_value(S_format_arguments)));
  Value object = slot_value(S_object);
  String * s = new String("The value ");
  s->append(::prin1_to_string(object));
  s->append(" cannot be printed readably.");
  return s;
}

// ### print_not_readable_object
Value CL_print_not_readable_object(Value arg)
{
  return check_standard_object(arg)->slot_value(S_object);
}
