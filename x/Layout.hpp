// Layout.hpp
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

#ifndef __LAYOUT_HPP
#define __LAYOUT_HPP

class LispClass;

class Layout : public TypedObject
{
public:
  LispClass * _class;

private:
  INDEX _numslots;
  Value * _slot_names;
  Value _shared_slots;
  bool _invalid;

public:
  // for MAKE-LAYOUT
  Layout(LispClass * lisp_class, Value instance_slots, Value shared_slots);

  // Copy constructor.
  Layout(Layout * old_layout);

  LispClass * lisp_class() const
  {
    return _class;
  }

  INDEX numslots() const
  {
    return _numslots;
  }

  Value * slot_names() const
  {
    return _slot_names;
  }

  Value shared_slots() const
  {
    return _shared_slots;
  }

  bool is_invalid() const
  {
    return _invalid;
  }

  void invalidate()
  {
    _invalid = true;
  }

  virtual bool typep(Value type) const;

  virtual Value type_of() const
  {
    return S_layout;
  }

  virtual Value class_of() const;

  // Ignores shared slots.
  long slot_index(Value slot_name)
  {
    for (INDEX i = 0; i < _numslots; i++)
      {
        if (_slot_names[i] == slot_name)
          return i;
      }
    // Not found.
    return -1;
  }

  // Ignores instance slots.
  Value shared_slot_location(Value slot_name)
  {
    if (_shared_slots != NIL)
      {
        Value shared_slots = _shared_slots;
        do
          {
            Value location = car(shared_slots);
            if (car(location) == slot_name)
              return location;
            shared_slots = xcdr(shared_slots);
          }
        while (shared_slots != NIL);
      }
    return NIL;
  }

  Value slot_location(Value slot_name)
  {
    for (INDEX i = 0; i < _numslots; i++)
      {
        if (_slot_names[i] == slot_name)
          return make_fixnum(i);
      }
    // Reaching here, it's not an instance slot.
    if (_shared_slots != NIL)
      {
        Value shared_slots = _shared_slots;
        do
          {
            Value location = car(shared_slots);
            if (car(location) == slot_name)
              return location;
            shared_slots = xcdr(shared_slots);
          }
        while (shared_slots != NIL);
      }
    return NIL;
  }
};

inline bool layout_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_LAYOUT);
}

inline Layout * the_layout(Value value)
{
  assert(layout_p(value));
  return reinterpret_cast<Layout *>(value - LOWTAG_TYPED_OBJECT);
}

inline Layout * check_layout(Value value)
{
  if (layout_p(value))
    return the_layout(value);
  signal_type_error(value, S_layout);
  // Not reached.
  return NULL;
}

#endif // Layout.hpp
