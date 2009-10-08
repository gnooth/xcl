// DoubleFloat.hpp
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

#ifndef __DOUBLE_FLOAT_HPP
#define __DOUBLE_FLOAT_HPP

class SingleFloat;

class DoubleFloat : public TypedObject
{
public:
  double _d;

  DoubleFloat(double d);

  DoubleFloat(long n) : TypedObject(WIDETAG_DOUBLE_FLOAT), _d(n) {}

  virtual Value type_of() const
  {
    return S_double_float;
  }

  virtual Value class_of() const;
  virtual bool typep(Value type) const;

  virtual bool eql(Value value) const;
  virtual bool equal(Value value) const;

  Value add(long n) const
  {
    return make_value(new DoubleFloat(_d + n));
  }

  Value add(SingleFloat * sf) const;

  Value add(DoubleFloat * df) const
  {
    return make_value(new DoubleFloat(_d + df->_d));
  }

  Value subtract(long n) const
  {
    return make_value(new DoubleFloat(_d - n));
  }

  Value subtract(SingleFloat * sf) const;

  Value subtract(DoubleFloat * df) const
  {
    return make_value(new DoubleFloat(_d - df->_d));
  }

  Value negate() const
  {
    return make_value(new DoubleFloat(-_d));
  }

  int low_bits() const
  {
    int n;
    memcpy(&n, &_d, sizeof(int));
    return n;
  }

  int high_bits() const
  {
#ifdef __x86_64__
    long n;
    memcpy(&n, &_d, sizeof(long));
#else
    long long n;
    memcpy(&n, &_d, sizeof(long long));
#endif
    return (int) ((n >> 32) & 0xffffffff);
  }

  int sign() const
  {
    return high_bits() < 0 ? -1 : 1;
  }

  Value rational() const;

  virtual unsigned long hash();

  virtual unsigned long equalp_hash();

  virtual AbstractString * write_to_string();
};

inline bool double_float_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_DOUBLE_FLOAT);
}

inline DoubleFloat * the_double_float(Value value)
{
  assert(double_float_p(value));
  return reinterpret_cast<DoubleFloat *>(value - LOWTAG_TYPED_OBJECT);
}

inline Value make_double_float(double d)
{
  return make_value(new DoubleFloat(d));
}

#endif // DoubleFloat.hpp
