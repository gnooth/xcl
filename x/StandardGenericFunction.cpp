// StandardGenericFunction.cpp
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

Value StandardGenericFunction::type_of() const
{
  return S_standard_generic_function;
}

Value StandardGenericFunction::class_of() const
{
  return C_standard_generic_function;
}

bool StandardGenericFunction::typep(Value type) const
{
  return (type == S_standard_generic_function || type == S_generic_function
          || type == S_metaobject
          || type == S_funcallable_standard_object || type == S_function
          || type == S_standard_object || type == S_atom || type == T
          || type == C_standard_generic_function || type == C_generic_function
          || type == C_metaobject || type == C_funcallable_standard_object
          || type == C_standard_object || type == C_function || type == C_t);
}

AbstractString * StandardGenericFunction::write_to_string()
{
  String * string = new String("#<");
  string->append(the_symbol(S_standard_generic_function)->write_to_string());
  if (name() != NIL)
    {
      string->append_char(' ');
      string->append(::write_to_string(name()));
    }
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  string->append(buf);
  return string;
}

// ### allocate-instance-standard-generic-function layout => instance
Value SYS_allocate_instance_standard_generic_function(Value arg)
{
  return make_value(new StandardGenericFunction(check_layout(arg)));
}
