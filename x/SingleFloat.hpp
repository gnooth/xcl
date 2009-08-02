// SingleFloat.hpp
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

#ifndef __SINGLE_FLOAT_HPP
#define __SINGLE_FLOAT_HPP

class DoubleFloat;

class SingleFloat : public TypedObject
{
public:
  float _f;

  SingleFloat(double d);

  SingleFloat(long n) : TypedObject(WIDETAG_SINGLE_FLOAT), _f(n) {}

  virtual Value type_of() const
  {
    return S_single_float;
  }

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual bool eql(Value value) const;

  virtual bool equal(Value value) const;

  Value add(long n) const
  {
    return make_value(new SingleFloat(_f + n));
  }

  Value add(SingleFloat * sf) const
  {
    return make_value(new SingleFloat(_f + sf->_f));
  }

  Value add(DoubleFloat * df) const;

  Value subtract(long n) const
  {
    return make_value(new SingleFloat(_f - n));
  }

  Value subtract(SingleFloat * sf) const
  {
    return make_value(new SingleFloat(_f - sf->_f));
  }

  Value subtract(DoubleFloat * df) const;

  Value negate() const
  {
    return make_value(new SingleFloat(-_f));
  }

  int bits() const
  {
    int n;
    memcpy(&n, &_f, sizeof(int));
    return n;
  }

  int sign() const
  {
    return bits() < 0 ? -1 : 1;
  }

  Value rational() const;

  virtual unsigned long hash();

  virtual unsigned long equalp_hash();

  virtual AbstractString * write_to_string();
};

inline bool single_float_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SINGLE_FLOAT);
}

inline SingleFloat * the_single_float(Value value)
{
  assert(single_float_p(value));
  return reinterpret_cast<SingleFloat *>(value - LOWTAG_TYPED_OBJECT);
}

inline Value make_single_float(float f)
{
  return make_value(new SingleFloat(f));
}

#endif // SingleFloat.hpp
