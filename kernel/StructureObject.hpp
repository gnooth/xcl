// StructureObject.hpp
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

#ifndef __STRUCTURE_OBJECT_HPP
#define __STRUCTURE_OBJECT_HPP

class StructureObject : public TypedObject
{
private:
  Value _types;
  const INDEX _numslots;
  Value _slots[0];

public:
  void * operator new(size_t size, INDEX numslots)
  {
    return GC_malloc_ignore_off_page(sizeof(StructureObject) + numslots * sizeof(Value));
  }

  // used only by structure_slots_offset()
  StructureObject()
    : TypedObject(WIDETAG_STRUCTURE_OBJECT), _numslots(0)
  {
  }

  StructureObject(Value types, INDEX numslots)
    : TypedObject(WIDETAG_STRUCTURE_OBJECT), _types(types), _numslots(numslots)
  {
  }

  StructureObject(Value types, Value values_list, INDEX numslots);

  StructureObject(StructureObject * structure);

  Value types() const
  {
    return _types;
  }

  INDEX numslots() const
  {
    return _numslots;
  }

  virtual Value type_of() const
  {
    return xcar(_types);
  }

  virtual Value class_of() const
  {
    return find_class(xcar(_types));
  }

  long slots_offset();

  virtual bool typep(Value type) const;

  bool structure_typep(Value type) const
  {
    return memq(type, _types);
  }

  virtual bool equalp(Value value) const;

  Value slot_value(INDEX i) const
  {
    return _slots[i];
  }

  void set_slot_value(INDEX i, Value value)
  {
    _slots[i] = value;
  }

  virtual AbstractString * write_to_string();
};

inline bool structure_object_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_STRUCTURE_OBJECT);
}

inline StructureObject * the_structure_object(Value value)
{
  assert(structure_object_p(value));
  return reinterpret_cast<StructureObject *>(value - LOWTAG_TYPED_OBJECT);
}

inline StructureObject * check_structure_object(Value value)
{
  if (structure_object_p(value))
    return the_structure_object(value);
  signal_type_error(value, S_structure_object);
  // not reached
  return NULL;
}

extern long structure_slots_offset();

#endif
