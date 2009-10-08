// Complex.hpp
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

#ifndef __COMPLEX_HPP
#define __COMPLEX_HPP

class Complex : public TypedObject
{
private:
  Value _realpart;
  Value _imagpart;
public:
  Complex(Value realpart, Value imagpart)
    : TypedObject(WIDETAG_COMPLEX), _realpart(realpart), _imagpart(imagpart)
  {}

  Value realpart() const
  {
    return _realpart;
  }

  Value imagpart() const
  {
    return _imagpart;
  }

  virtual Value type_of() const
  {
    return S_complex;
  }

  virtual Value class_of() const
  {
    return C_complex;
  }

  virtual bool typep(Value type) const;

  virtual unsigned long hash()
  {
    // REVIEW
    return ::hash(_realpart) ^ ::hash(_imagpart);
  }

  Value add(long n) const;
  Value add(Complex * c) const;

  Value subtract(long n) const;
  Value subtract(Complex * c) const;

  Value negate() const;

  virtual bool eql(Value value) const;
  virtual bool equal(Value value) const;
  virtual bool equalp(Value value) const;

  virtual AbstractString * write_to_string();
};

inline bool complexp(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_COMPLEX);
}

inline Complex * the_complex(Value value)
{
  assert(complexp(value));
  return reinterpret_cast<Complex *>(value - LOWTAG_TYPED_OBJECT);
}

extern Value make_complex(Value realpart, Value imagpart);

#endif // Complex.hpp
