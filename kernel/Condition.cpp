// Condition.cpp
//
// Copyright (C) 2006-2008 Peter Graves <peter@armedbear.org>
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
#include "Condition.hpp"
#include "StringOutputStream.hpp"

Layout * Condition::get_layout_for_class()
{
  static Layout * layout;
  if (layout == NULL)
    layout = new Layout(the_class(C_condition),
                        list2(S_format_control, S_format_arguments),
                        NIL);
  return layout;
}

void Condition::set_format_control(Value format_control)
{
//   iset(0, format_control);
//   SYS_set_std_instance_slot_value(make_value(this), S_format_control, format_control);
  set_slot_value(S_format_control, format_control);
}

void Condition::set_format_arguments(Value format_arguments)
{
//   iset(1, format_arguments);
//   SYS_set_std_instance_slot_value(make_value(this), S_format_arguments, format_arguments);
  set_slot_value(S_format_arguments, format_arguments);
}

Value Condition::format_control()
{
//   return iref(0);
//   return SYS_std_instance_slot_value(make_value(this), S_format_control);
  return slot_value(S_format_control);
}

Value Condition::format_arguments()
{
//   return iref(1);
//   return SYS_std_instance_slot_value(make_value(this), S_format_arguments);
  return slot_value(S_format_arguments);
}

void Condition::initialize(Value initargs)
{
  Value format_control = NULL_VALUE;
  Value format_arguments = NULL_VALUE;
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
    }
  if (format_control != NULL_VALUE)
    set_format_control(format_control);
  if (format_arguments != NULL_VALUE)
    set_format_arguments(format_arguments);
}

bool Condition::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_condition || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_condition || type == C_standard_object || type == C_t);
}

AbstractString * Condition::write_to_string()
{
  Thread * const thread = current_thread();
  if (CL_fboundp(S_print_object) != NIL)
    {
      StringOutputStream * stream = new StringOutputStream(S_character);
      thread->execute(the_symbol(S_print_object)->function(),
                      make_value(this),
                      make_value(stream));
      AbstractString * s = stream->get_string();
      return s;
    }
  if (thread->symbol_value(S_print_escape) == NIL && thread->symbol_value(S_print_readably) == NIL)
    {
      if (stringp(format_control()))
        {
          if (format_arguments() != NIL)
            return format_to_string(format_control(), format_arguments());
          else
            return the_string(format_control());
        }
    }
  return unreadable_string();
}

// ### conditionp
Value SYS_conditionp(Value arg)
{
  if (conditionp(arg))
    return T;
  // REVIEW
  return typep(arg, S_condition) ? T : NIL;
}
