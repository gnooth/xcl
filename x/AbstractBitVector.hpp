// AbstractBitVector.hpp
//
// Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

#ifndef __ABSTRACT_BIT_VECTOR_HPP
#define __ABSTRACT_BIT_VECTOR_HPP

#define BIT_VECTOR_SHIFT 5
#define BIT_VECTOR_MASK 31

class AbstractBitVector : public AbstractVector
{
protected:
  AbstractBitVector(long widetag)
    : AbstractVector(widetag)
  {
  }

  AbstractBitVector(long widetag, unsigned long capacity)
    : AbstractVector(widetag, capacity)
  {
  }

public:
  virtual void setbit(INDEX index) = 0;
  virtual void clearbit(INDEX index) = 0;
  virtual BIT getbit(INDEX index) const = 0;
  virtual void setbit(INDEX index, BIT bit) = 0;

  virtual Value class_of() const;

  virtual Value element_type() const
  {
    return S_bit;
  }

  virtual unsigned long hash();

  virtual Value subseq(unsigned long start, unsigned long end) const;

  virtual bool equal(Value value) const;

  virtual AbstractString * write_to_string();
};

inline bool bit_vector_p(Value value)
{
  if (typed_object_p(value))
    {
      switch (the_typed_object(value)->widetag())
        {
        case WIDETAG_SIMPLE_BIT_VECTOR:
        case WIDETAG_BIT_VECTOR:
          return true;
        }
    }
  return false;
}

inline AbstractBitVector * the_bit_vector(Value value)
{
  assert(bit_vector_p(value));
  return reinterpret_cast<AbstractBitVector *>(value - LOWTAG_TYPED_OBJECT);
}

inline AbstractBitVector * check_bit_vector(Value value)
{
  if (bit_vector_p(value))
    return the_bit_vector(value);
  signal_type_error(value, S_bit_vector);
  // not reached
  return NULL;
}

#endif // AbstractBitVector.hpp
