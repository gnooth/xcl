// UnboundVariable.cpp
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
#include "UnboundVariable.hpp"

bool UnboundVariable::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_unbound_variable || type == S_cell_error || type == S_error
            || type == S_serious_condition || type == S_condition
            || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_unbound_variable || type == C_cell_error || type == C_error
            || type == C_serious_condition || type == C_condition
            || type == C_standard_object || type == C_t);
}

AbstractString * UnboundVariable::write_to_string()
{
  String * s = new String();
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_escape) != NIL)
    {
      s->append(the_symbol(S_unbound_variable)->write_to_string());
      s->append_char(' ');
      s->append(::write_to_string(slot_value(S_name)));
      return unreadable_string(s);
    }
  else
    {
      s->append("The variable ");
      s->append(::prin1_to_string(slot_value(S_name)));
      s->append(" is unbound.");
      return s;
    }
}
