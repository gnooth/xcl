// StandardObject.hpp
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

#ifndef __STANDARD_OBJECT_HPP
#define __STANDARD_OBJECT_HPP

#include "Layout.hpp"

class StandardObject : public TypedObject
{
private:
  Layout * _layout;
  INDEX _numslots; // REVIEW
  Value * _slots;

protected:
  StandardObject(long widetag, INDEX numslots)
    : TypedObject(widetag), _layout(NULL), _numslots(numslots)
  {
    _slots = (Value *) GC_malloc(numslots * sizeof(Value));
    for (INDEX i = 0; i < numslots; i++)
      xiset(i, UNBOUND_VALUE);
  }

public:
  StandardObject(long widetag, Layout * layout)
    : TypedObject(widetag), _layout(layout)
  {
    INDEX numslots = layout->numslots();
    _slots = (Value *) GC_malloc(numslots * sizeof(Value));
    for (INDEX i = 0; i < numslots; i++)
      xiset(i, UNBOUND_VALUE);
    _numslots = numslots;
  }

  StandardObject(long widetag, Layout * layout, INDEX numslots)
    : TypedObject(widetag), _layout(layout), _numslots(numslots)
  {
    _slots = (Value *) GC_malloc(numslots * sizeof(Value));
    for (INDEX i = 0; i < numslots; i++)
      xiset(i, UNBOUND_VALUE);
  }

  Layout * layout() const
  {
    return _layout;
  }

  void set_layout(Layout * layout)
  {
    assert((((long)layout) & LOWTAG_MASK) == 0);
    _layout = layout;
  }

  INDEX numslots() const
  {
    return _numslots;
  }

  void set_numslots(long n)
  {
    _numslots = n;
  }

  Value * slots() const
  {
    return _slots;
  }

  void set_slots(Value * slots)
  {
    _slots = slots;
  }

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  Value xiref(INDEX index) const
  {
    return _slots[index];
  }

  void xiset(INDEX index, Value value)
  {
    _slots[index] = value;
  }

  Value iref(INDEX index) const
  {
    if (index >= _numslots)
      {
        return signal_type_error(make_number((long)index),
                                 list3(S_integer,
                                       FIXNUM_ZERO,
                                       list1(make_number((long)_numslots))));
      }
    return _slots[index];
  }

  void iset(INDEX index, Value value)
  {
    if (index >= _numslots)
      {
        signal_type_error(make_number((long)index),
                          list3(S_integer,
                                FIXNUM_ZERO,
                                list1(make_number((long)_numslots))));
      }
    _slots[index] = value;
  }

  Value slot_value(Value arg);

  void set_slot_value(Value arg1, Value arg2);

  Layout * update_layout();

  AbstractString * write_to_string();
};

inline bool standard_object_p(Value value)
{
  if (typed_object_p(value))
    {
      if (the_typed_object(value)->widetag() & WIDETAG_INSTANCE_BIT)
        return true;
    }
  return false;
}

inline StandardObject * the_standard_object(Value value)
{
  assert(standard_object_p(value));
  return reinterpret_cast<StandardObject *>(value - LOWTAG_TYPED_OBJECT);
}

inline StandardObject * check_standard_object(Value value)
{
  if (standard_object_p(value))
    return the_standard_object(value);
  signal_type_error(value, S_standard_object);
  // Not reached.
  return NULL;
}

#endif // StandardObject.hpp
