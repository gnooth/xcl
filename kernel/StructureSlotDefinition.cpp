// StructureSlotDefinition.cpp
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
#include "StructureSlotDefinition.hpp"

AbstractString * StructureSlotDefinition::write_to_string()
{
  String * string = new String();
  if (current_thread()->symbol_value(S_print_fasl) != NIL)
    {
      string->append("#.(");
      string->append(the_symbol(S_make_structure_slot_definition_internal)->write_to_string());
      string->append(" '");
      string->append(::write_to_string(_name));
      string->append_char(' ');
      string->append(::write_to_string(_index));
      string->append(" '");
      string->append(::write_to_string(_reader));
      string->append(" '");
      string->append(::write_to_string(_initform));
      string->append(" '");
      string->append(::write_to_string(_type));
      string->append_char(' ');
      string->append(::write_to_string(_read_only_p));
      string->append_char(')');
    }
  else
    {
      string->append("#<");
      string->append(::write_to_string(type_of()));
      string->append_char(' ');
      string->append(::write_to_string(name()));
      char buf[256];
      SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
      string->append(buf);
    }
  return string;
}

// ### %make-structure-slot-definition
Value SYS_make_structure_slot_definition_internal(Value name, Value index, Value reader,
                                                  Value initform, Value type, Value read_only_p)
{
  return make_value(new StructureSlotDefinition(name, index, reader, initform, type, read_only_p));
}

// ### copy-structure-slot-definition
Value SYS_copy_structure_slot_definition(Value arg)
{
  StructureSlotDefinition * def = check_structure_slot_definition(arg);
  return make_value(new StructureSlotDefinition(def));
}

// ### slot-name
Value SYS_slot_name(Value slotdef)
{
  return check_structure_slot_definition(slotdef)->name();
}

// ### slot-index
Value SYS_slot_index(Value slotdef)
{
  return check_structure_slot_definition(slotdef)->index();
}

// ### slot-reader
Value SYS_slot_reader(Value slotdef)
{
  return check_structure_slot_definition(slotdef)->reader();
}

// ### slot-initform
Value SYS_slot_initform(Value slotdef)
{
  return check_structure_slot_definition(slotdef)->initform();
}

// ### slot-type
Value SYS_slot_type(Value slotdef)
{
  return check_structure_slot_definition(slotdef)->type();
}

// ### slot-read-only-p
Value SYS_slot_read_only_p(Value slotdef)
{
  return check_structure_slot_definition(slotdef)->read_only_p();
}

// ### set-slot-index
Value SYS_set_slot_index(Value slotdef, Value index)
{
  check_structure_slot_definition(slotdef)->set_index(index);
  return index;
}

// ### set-slot-reader
Value SYS_set_slot_reader(Value slotdef, Value reader)
{
  check_structure_slot_definition(slotdef)->set_reader(reader);
  return reader;
}

// ### set-slot-initform
Value SYS_set_slot_initform(Value slotdef, Value initform)
{
  check_structure_slot_definition(slotdef)->set_initform(initform);
  return initform;
}
