// UnboundSlot.cpp
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
#include "UnboundSlot.hpp"

Layout * UnboundSlot::get_layout_for_class()
{
  static Layout * layout;
  if (layout == NULL)
    layout = new Layout(the_class(C_unbound_slot),
                        list4(S_format_control,
                              S_format_arguments,
                              S_name,
                              S_instance),
                        NIL);
  return layout;
}

void UnboundSlot::initialize(Value initargs)
{
  Value format_control = NULL_VALUE;
  Value format_arguments = NULL_VALUE;
  Value name = NULL_VALUE;
  Value instance = NULL_VALUE;
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
      else if (first == K_name)
        {
          if (name == NULL_VALUE)
            name = second;
        }
      else if (first == K_instance)
        {
          if (instance == NULL_VALUE)
            instance = second;
        }
    }
  if (format_control != NULL_VALUE)
    set_format_control(format_control);
  if (format_arguments != NULL_VALUE)
    set_format_arguments(format_arguments);
  set_slot_value(S_name, name != NULL_VALUE ? name : NIL);
  set_slot_value(S_instance, instance != NULL_VALUE ? instance : NIL);
}

bool UnboundSlot::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_unbound_slot || type == S_cell_error || type == S_error
            || type == S_serious_condition || type == S_condition
            || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_unbound_slot || type == C_cell_error || type == C_error
            || type == C_serious_condition || type == C_condition
            || type == C_standard_object || type == C_t);
}

AbstractString * UnboundSlot::write_to_string()
{
  String * s = new String();
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_escape) != NIL)
    {
      s->append(the_symbol(S_unbound_slot)->write_to_string());
      Value name = iref(2); // REVIEW
      if (name != UNBOUND_VALUE)
        {
          s->append_char(' ');
          s->append(::write_to_string(name));
        }
      return unreadable_string(s);
    }
  else
    {
      s->append("The slot ");
      s->append(::prin1_to_string(slot_value(S_name)));
      s->append(" is unbound in the object ");
      s->append(::prin1_to_string(slot_value(S_instance)));
      s->append_char('.');
      return s;
    }
}

// ### unbound-slot-instance condition => instance
Value CL_unbound_slot_instance(Value arg)
{
  return check_standard_object(arg)->slot_value(S_instance);
}
