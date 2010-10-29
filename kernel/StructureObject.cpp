// StructureObject.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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
#include "StructureObject.hpp"
#include "StructureSlotDefinition.hpp"

StructureObject::StructureObject(Value types, Value values_list, INDEX numslots)
  : TypedObject(WIDETAG_STRUCTURE_OBJECT), _types(types),
//     _numslots(length(values_list))
    _numslots(numslots)
{
  assert(_slots != NULL);
  if (numslots != length(values_list))
    {
      printf("types = %s numslots = %lu length(values_list) = %lu\n",
             ::prin1_to_string(types)->as_c_string(),
             numslots, length(values_list));
      assert(0);
    }
//   for (INDEX i = 0; i < _numslots; i++)
  for (INDEX i = 0; i < numslots; i++)
    {
      set_slot_value(i, xcar(values_list));
      values_list = xcdr(values_list);
    }
}

StructureObject::StructureObject(StructureObject * structure)
  : TypedObject(WIDETAG_STRUCTURE_OBJECT), _types(structure->_types),
    _numslots(structure->_numslots)
{
  for (INDEX i = 0; i < _numslots; i++)
    set_slot_value(i, structure->slot_value(i));
}

long StructureObject::slots_offset()
{
  return ((long)(&(this->_slots))) - ((long)this);
}

bool StructureObject::typep(Value type) const
{
  if (symbolp(type))
    {
      if (memq(type, _types))
        return true;
      if (type == S_structure_object || type == S_atom || type == T)
        return true;
    }
  if (classp(type))
    return memq(the_class(type)->name(), _types);
  return false;
}

bool StructureObject::equalp(Value value) const
{
  if (structure_object_p(value))
    {
      StructureObject * obj = the_structure_object(value);
      // REVIEW if (::equal(_types, obj->_types)
      if (car(_types) == car(obj->_types))
        {
          for (unsigned long i = 0; i < _numslots; i++)
            {
              if (!::equalp(_slots[i], obj->_slots[i]))
                return false;
            }
          return true;
        }
    }
  return false;
}

AbstractString * StructureObject::write_to_string()
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_structure) != NIL || thread->symbol_value(S_print_readably) != NIL)
    {
      INDEX max_length = MOST_POSITIVE_FIXNUM;
      if (thread->symbol_value(S_print_readably) == NIL)
        {
          Value print_length = thread->symbol_value(S_print_length);
          if (print_length != NIL)
            max_length = check_index(print_length);
        }
      String * string = new String("#S(");
      string->append(::write_to_string(xcar(_types)));
      Value cls = find_class(car(_types));
      Value slots = ((StructureClass *)check_class(cls))->slots();
      for (INDEX i = 0; i < _numslots; i++)
        {
          if (i >= max_length)
            {
              string->append(" ...");
              break;
            }
          string->append_char(' ');
          string->append_char(':');
          string->append(the_symbol(check_structure_slot_definition(car(slots))->name())->name());
          slots = xcdr(slots);
          string->append_char(' ');
          string->append(::write_to_string(slot_value(i)));
        }
      string->append_char(')');
      return string;
    }
  else
    return unreadable_string();
}

long structure_slots_offset()
{
  StructureObject obj;
  return obj.slots_offset();
}

// ### %%make-structure types values numslots
Value SYS_xxmake_structure(Value types, Value numslots)
{
#ifndef NDEBUG
  if (!listp(types))
    return signal_type_error(types, S_list);
#endif
  long n = fixnum_value(numslots);
  return make_value(new(n) StructureObject(types, n));
}

// ### %make-structure types values numslots
Value SYS_xmake_structure(Value types, Value values_list, Value numslots)
{
#ifndef NDEBUG
  if (!listp(types))
    return signal_type_error(types, S_list);
#endif
  long n = fixnum_value(numslots);
  return make_value(new(n) StructureObject(types, values_list, n));
}

// ### structure-object-p
Value SYS_structure_object_p(Value arg)
{
  return structure_object_p(arg) ? T : NIL;
}

// ### structure-typep object type-name
// type-name is a symbol that names a structure type
Value SYS_structure_typep(Value object, Value type_name)
{
  return (structure_object_p(object)
          && the_structure_object(object)->structure_typep(type_name)) ? T : NIL;
}

// ### require-structure-type object type-name => object
Value SYS_require_structure_type(Value object, Value type_name)
{
  if (structure_object_p(object) && the_structure_object(object)->structure_typep(type_name))
    return object;
  return signal_type_error(object, type_name);
}

// ### copy-structure
Value CL_copy_structure(Value object)
{
  StructureObject * structure = check_structure_object(object);
  return make_value(new(structure->numslots()) StructureObject(structure));
}

// ### structure-ref structure index => value
Value SYS_structure_ref(Value structure, Value index)
{
  StructureObject * s = check_structure_object(structure);
  return s->slot_value(check_index(index, 0, s->numslots() - 1));
}

// ### structure-set structure index new-value => new-value
Value SYS_structure_set(Value structure, Value index, Value new_value)
{
  StructureObject * s = check_structure_object(structure);
  s->set_slot_value(check_index(index, 0, s->numslots() - 1), new_value);
  return new_value;
}
