// Condition.hpp
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

#ifndef __CONDITION_HPP
#define __CONDITION_HPP

#include "StandardObject.hpp"

class Condition : public StandardObject
{
private:
  static Layout * get_layout_for_class();

protected:
  void set_format_control(Value format_control);

  void set_format_arguments(Value format_arguments);

  Condition(long widetag, Layout * layout)
    : StandardObject(widetag, layout)
  {
    set_format_control(NIL);
    set_format_arguments(NIL);
  }

public:
  Condition()
    : StandardObject(WIDETAG_CONDITION, get_layout_for_class())
  {
    set_format_control(NIL);
    set_format_arguments(NIL);
  }

  Condition(AbstractString * s)
    : StandardObject(WIDETAG_CONDITION, get_layout_for_class())
  {
    set_format_control(make_value(s));
    set_format_arguments(NIL);
  }

  Condition(const char * s)
    : StandardObject(WIDETAG_CONDITION, get_layout_for_class())
  {
    set_format_control(make_value(new_simple_string(s)));
    set_format_arguments(NIL);
  }

  Value format_control();

  Value format_arguments();

  virtual void initialize(Value initargs);

  virtual Value type_of() const
  {
    return S_condition;
  }

  virtual Value class_of() const
  {
    return C_condition;
  }

  virtual bool typep(Value type) const;

  virtual AbstractString * write_to_string();
};

inline bool conditionp(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_CONDITION);
}

inline Condition * the_condition(Value value)
{
  assert(conditionp(value));
  return reinterpret_cast<Condition *>(value - LOWTAG_TYPED_OBJECT);
}

inline Condition * check_condition(Value value)
{
  if (conditionp(value))
    return the_condition(value);
  signal_type_error(value, S_condition);
  // Not reached.
  return NULL;
}

extern Value signal_lisp_error(Condition * error);
extern Value signal_lisp_error(AbstractString * s);

#endif // Condition.hpp
