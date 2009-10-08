// BuiltInClass.cpp
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

Value BuiltInClass::type_of() const
{
  return S_built_in_class;
}

bool BuiltInClass::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_built_in_class || type == S_class
            || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_built_in_class || type == C_class
            || type == C_standard_object || type == C_t);
}

AbstractString * BuiltInClass::write_to_string()
{
  String * s = new String(the_symbol(S_built_in_class)->prin1_to_string());
  Value name = this->name();
  if (name != NIL)
    {
      s->append_char(' ');
      s->append(::prin1_to_string(name));
    }
  return unreadable_string(s);
}
