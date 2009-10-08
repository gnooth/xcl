// Operator.hpp
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

#ifndef __OPERATOR_HPP
#define __OPERATOR_HPP

class Operator : public TypedObject
{
private:
  Value _name;

protected:
  Operator(long widetag, Value name) : TypedObject(widetag), _name(name)
  {
    assert(widetag == WIDETAG_FUNCTION || WIDETAG_PRIMITIVE || widetag == WIDETAG_CLOSURE
           || widetag == WIDETAG_SPECIAL_OPERATOR
           || widetag == WIDETAG_MACRO
           || widetag == WIDETAG_AUTOLOAD);
  }

public:
  // REVIEW
  virtual bool is_special_operator() = 0;

  Value operator_name() const
  {
    return _name;
  }

  void set_operator_name(Value name)
  {
    _name = name;
  }
};

inline bool operatorp(Value value)
{
  if (typed_object_p(value))
    {
      switch (the_typed_object(value)->widetag())
        {
        case WIDETAG_FUNCTION:
        case WIDETAG_PRIMITIVE:
        case WIDETAG_CLOSURE:
        case WIDETAG_SPECIAL_OPERATOR:
        case WIDETAG_MACRO:
        case WIDETAG_AUTOLOAD:
          return true;
        }
    }
  return false;
}

inline Operator * the_operator(Value value)
{
  assert(operatorp(value));
  return reinterpret_cast<Operator *>(value - LOWTAG_TYPED_OBJECT);
}

#endif
