// Ratio.hpp
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

#ifndef __RATIO_HPP
#define __RATIO_HPP

class Ratio : public TypedObject
{
public:
  mpq_t _q;

  Ratio(mpq_t q) : TypedObject(WIDETAG_RATIO)
  {
    mpq_set(_q, q);
    mpq_canonicalize(_q);
  }

  virtual Value type_of() const
  {
    return S_ratio;
  }

  virtual Value class_of() const
  {
    return C_ratio;
  }

  virtual bool typep(Value type) const;

  Value numerator();
  Value denominator();

  Value add(long n) const;
  Value add(Bignum * b);
  Value add(Ratio * r);

  Value subtract(long n) const;
  Value subtract(Ratio * r) const;

  Value negate() const;

  bool eql(Ratio * r) const
  {
    return mpq_equal(_q, r->_q);
  }

  virtual bool eql(Value value) const;

  virtual bool equal(Value value) const;

  virtual unsigned long hash();

  virtual AbstractString * write_to_string();
};

inline bool ratiop(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_RATIO);
}

inline Ratio * the_ratio(Value value)
{
  assert(ratiop(value));
  return reinterpret_cast<Ratio *>(value - LOWTAG_TYPED_OBJECT);
}

extern Value make_ratio(const char *s, long base);

#endif // Ratio.hpp
