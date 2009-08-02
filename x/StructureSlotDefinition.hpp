// StructureSlotDefinition.hpp
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

#ifndef __STRUCTURE_SLOT_DEFINITION_HPP
#define __STRUCTURE_SLOT_DEFINITION_HPP

class StructureSlotDefinition : public TypedObject
{
private:
  Value _name;
  Value _index;
  Value _reader;
  Value _initform;
  Value _type;
  Value _read_only_p;

public:
  StructureSlotDefinition(Value name, Value index, Value reader,
                          Value initform, Value type, Value read_only_p)
    : TypedObject(WIDETAG_STRUCTURE_SLOT_DEFINITION), _name(name),
      _index(index), _reader(reader), _initform(initform), _type(type),
      _read_only_p(read_only_p)
  {
  }

  StructureSlotDefinition(StructureSlotDefinition * def)
    : TypedObject(WIDETAG_STRUCTURE_SLOT_DEFINITION), _name(def->_name),
      _index(def->_index), _reader(def->_reader), _initform(def->_initform),
      _type(def->_type), _read_only_p(def->_read_only_p)
  {
  }

  Value name()        const { return _name; }
  Value index()       const { return _index; }
  Value reader()      const { return _reader; }
  Value initform()    const { return _initform; }
  Value type()        const { return _type; }
  Value read_only_p() const { return _read_only_p; }

  void set_index(Value index)       { _index = index; }
  void set_reader(Value reader)     { _reader = reader; }
  void set_initform(Value initform) { _initform = initform; }

  virtual Value type_of() const
  {
    return S_structure_slot_definition;
  }

  AbstractString * write_to_string();
};

inline bool structure_slot_definition_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_STRUCTURE_SLOT_DEFINITION);
}

inline StructureSlotDefinition * the_structure_slot_definition(Value value)
{
  assert(structure_slot_definition_p(value));
  return reinterpret_cast<StructureSlotDefinition *>(value - LOWTAG_TYPED_OBJECT);
}

inline StructureSlotDefinition * check_structure_slot_definition(Value value)
{
  if (structure_slot_definition_p(value))
    return the_structure_slot_definition(value);
  signal_type_error(value, S_structure_slot_definition);
  // Not reached.
  return NULL;
}

#endif // StructureSlotDefinition.hpp
