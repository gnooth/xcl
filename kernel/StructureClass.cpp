// StructureClass.cpp
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
#include "primitives.hpp"       // CL_nreverse()

// returns a list
static Value structure_class_instance_slots()
{
  Value instance_slots = NIL;
  instance_slots = make_cons(S_direct_methods, instance_slots);
  instance_slots = make_cons(S_prototype, instance_slots);
  instance_slots = make_cons(S_name, instance_slots);
  instance_slots = make_cons(S_layout, instance_slots);
  instance_slots = make_cons(S_precedence_list, instance_slots);
  instance_slots = make_cons(S_direct_superclasses, instance_slots);
  instance_slots = make_cons(S_direct_subclasses, instance_slots);
  instance_slots = make_cons(S_direct_slots, instance_slots);
  instance_slots = make_cons(S_slots, instance_slots);
  return CL_nreverse(instance_slots);
}
// REVIEW
static Layout * structure_class_layout()
{
//   Value instance_slots = NIL;
//   instance_slots = make_cons(S_direct_methods, instance_slots);
//   instance_slots = make_cons(S_class_prototype, instance_slots);
//   instance_slots = make_cons(S_name, instance_slots);
//   instance_slots = make_cons(S_layout, instance_slots);
//   instance_slots = make_cons(S_precedence_list, instance_slots);
//   instance_slots = make_cons(S_direct_superclasses, instance_slots);
//   instance_slots = make_cons(S_direct_subclasses, instance_slots);
//   instance_slots = make_cons(S_direct_slots, instance_slots);
//   instance_slots = make_cons(S_slots, instance_slots);
  return new Layout(check_class(find_class(S_structure_class)),
//                     CL_nreverse(instance_slots),
                    structure_class_instance_slots(),
                    NIL // no shared slots
                    );
}

StructureClass::StructureClass(Value name)
  : LispClass(WIDETAG_STRUCTURE_CLASS, 9)
{
  set_layout(structure_class_layout());
  iset(3, make_value(structure_class_layout()));
  set_name(name);
//   set_slots(structure_class_instance_slots());
  set_slots(NIL);
}

StructureClass::StructureClass(Value name, Value slots)
  : LispClass(WIDETAG_STRUCTURE_CLASS, 9)
{
  set_layout(structure_class_layout());
//   iset(3, make_value(structure_class_layout()));
  set_name(name);
  set_slots(slots);
}

Value StructureClass::type_of() const
{
  return S_structure_class;
}

bool StructureClass::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_structure_class || type == S_class
            || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_structure_class || type == C_class
            || type == C_standard_object || type == C_t);
}

// ### make-structure-class name include slots => class
Value SYS_make_structure_class(Value name, Value slots, Value include)
{
  if (!symbolp(name))
    return signal_type_error(name, S_symbol);
  if (!listp(slots))
    return signal_type_error(name, S_list);
  StructureClass * c = new StructureClass(name, slots);
  if (include != NIL)
    {
      Value included_class = find_class(include);
      if (included_class == NULL_VALUE)
        {
          String * message = new String(::prin1_to_string(include));
          message->append(" does not name a class.");
          return signal_lisp_error(message);
        }
      c->set_cpl(make_cons(make_value(c), the_class(included_class)->cpl()));
    }
  else
    c->set_cpl(make_cons(make_value(c), the_class(C_structure_object)->cpl()));

  return add_class(name, make_value(c));
}
