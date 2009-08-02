// SimpleBitVector.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"

void SimpleBitVector::setbit(INDEX index)
{
  inline_setbit(index);
}

void SimpleBitVector::clearbit(INDEX index)
{
  inline_clearbit(index);
}

BIT SimpleBitVector::getbit(INDEX index) const
{
  return inline_getbit(index);
}

void SimpleBitVector::setbit(INDEX index, BIT bit)
{
  inline_setbit(index, bit);
}

SimpleBitVector::SimpleBitVector(INDEX length)
  : AbstractBitVector(WIDETAG_SIMPLE_BIT_VECTOR, length)
{
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  _data = (unsigned int *) GC_malloc_atomic(size * sizeof(unsigned int));
  for (INDEX i = size; i-- > 0;)
    _data[i] = 0;
}

SimpleBitVector::SimpleBitVector(INDEX length, Value bits[])
  : AbstractBitVector(WIDETAG_SIMPLE_BIT_VECTOR, length)
{
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  _data = (unsigned int *) GC_malloc_atomic(size * sizeof(unsigned int));
  for (INDEX i = length; i-- > 0;)
    inline_setbit(i, check_bit(bits[i]));
}

SimpleBitVector::SimpleBitVector(AbstractString * string)
  : AbstractBitVector(WIDETAG_SIMPLE_BIT_VECTOR)
{
  const INDEX len = string->length();
  _capacity = len;
  INDEX size = len >> BIT_VECTOR_SHIFT;
  if ((len & BIT_VECTOR_MASK) != 0)
    ++size;
  _data = (unsigned int *) GC_malloc_atomic(size * sizeof(unsigned int));
  for (INDEX i = len; i-- > 0;)
    {
      char c = string->fast_char_at(i);
      if (c == '1')
        inline_setbit(i);
      else
        inline_clearbit(i);
    }
}

bool SimpleBitVector::typep(Value type) const
{
  if (consp(type))
    {
      Value type_specifier_atom = xcar(type);
      Value tail = xcdr(type);
      if (type_specifier_atom == S_array || type_specifier_atom == S_simple_array)
        {
          if (consp(tail))
            {
              Value element_type = xcar(tail);
              tail = xcdr(tail);
              if (element_type == UNSPECIFIED || element_type == S_bit
                  || ::equal(element_type, list3(S_integer, FIXNUM_ZERO, FIXNUM_ONE)))
                {
                  if (tail == NIL)
                    return true;
                  if (cdr(tail) == NIL) // i.e. length(tail) == 1
                    {
                      Value dimensions = xcar(tail);
                      if (dimensions == UNSPECIFIED)
                        return true;
                      if (dimensions == FIXNUM_ONE)
                        return true;
                      if (consp(dimensions))
                        {
                          if (::length(dimensions) == 1)
                            {
                              Value dimension = xcar(dimensions);
                              if (dimension == UNSPECIFIED
                                  || dimension == make_fixnum(_capacity))
                                return true;
                            }
                        }
                    }
                }
            }
        }
      else if (type_specifier_atom == S_bit_vector || type_specifier_atom == S_simple_bit_vector)
        {
          if (::equal(tail, list1(UNSPECIFIED)))
            return true;
          if (::equal(tail, list1(make_fixnum(_capacity))))
            return true;
        }
    }
  else if (symbolp(type))
    {
      if (type == S_simple_bit_vector || type == S_bit_vector || type == S_vector
          || type == S_sequence || type == S_simple_array || type == S_array
          || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_bit_vector || type == C_vector || type == C_sequence
          || type == C_array || type == C_t)
        return true;
    }
  return false;
}

void SimpleBitVector::fill(Value value)
{
  const BIT bit = check_bit(value);
  INDEX size = _capacity >> BIT_VECTOR_SHIFT;
  if ((_capacity & BIT_VECTOR_MASK) != 0)
    ++size;
  if (bit == 0)
    for (INDEX i = size; i-- > 0;)
      _data[i] = 0;
  else
    for (INDEX i = size; i-- > 0;)
      _data[i] = 0xffffffff;
}

inline Value SimpleBitVector::inline_aref(INDEX i) const
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  return make_fixnum(inline_getbit(i));
}

Value SimpleBitVector::aref(INDEX i) const
{
  return inline_aref(i);
}

inline Value SimpleBitVector::inline_aset(INDEX i, Value new_value)
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  inline_setbit(i, check_bit(new_value));
  return new_value;
}

Value SimpleBitVector::aset(INDEX i, Value new_value)
{
  return inline_aset(i, new_value);
}

Value SimpleBitVector::elt(INDEX i) const
{
  return inline_aref(i);
}

Value SimpleBitVector::reverse() const
{
  const INDEX capacity = _capacity;
  SimpleBitVector * result = new SimpleBitVector(capacity);
  INDEX i, j;
  for (i = 0, j = capacity - 1; i < capacity; i++, j--)
    result->inline_setbit(i, inline_getbit(j));
  return make_value(result);
}

Value SimpleBitVector::nreverse()
{
  if (_capacity > 0)
    {
      INDEX i = 0;
      INDEX j = _capacity - 1;
      while (i < j)
        {
          INDEX temp = inline_getbit(i);
          inline_setbit(i, inline_getbit(j));
          inline_setbit(j, temp);
          ++i;
          --j;
        }
    }
  return make_value(this);
}

AbstractVector * SimpleBitVector::adjust_vector(INDEX new_capacity,
                                                Value initial_element,
                                                Value initial_contents)
{
  if (initial_contents != NIL)
    {
      SimpleBitVector * bv = new SimpleBitVector(new_capacity);
      if (listp(initial_contents))
        {
          Value list = initial_contents;
          for (INDEX i = 0; i < new_capacity; i++)
            {
              bv->inline_setbit(i, check_bit(car(list)));
              list = xcdr(list);
            }
        }
      else if (vectorp(initial_contents))
        {
          AbstractVector * v = the_vector(initial_contents);
          for (INDEX i = 0; i < new_capacity; i++)
            bv->inline_setbit(i, check_bit(v->aref(i)));
        }
      else
        signal_type_error(initial_contents, S_sequence);
      return bv;
    }
  if (_capacity != new_capacity)
    {
      SimpleBitVector * bv = new SimpleBitVector(new_capacity);
      INDEX limit = (_capacity < new_capacity) ? _capacity : new_capacity;
      for (INDEX i = 0; i < limit; i++)
        bv->inline_setbit(i, inline_getbit(i));
      if (_capacity < new_capacity)
        {
          BIT bit = check_bit(initial_element);
          for (INDEX i = _capacity; i < new_capacity; i++)
            bv->inline_setbit(i, bit);
        }
      return bv;
    }
  // No change.
  return this;
}

AbstractVector * SimpleBitVector::displace_vector(INDEX new_capacity,
                                                  AbstractArray * displaced_to,
                                                  INDEX offset)
{
  return new BitVector(new_capacity, displaced_to, offset, NIL);
}

// ### simple-bit-vector-p object => generalized-boolean
Value CL_simple_bit_vector_p(Value object)
{
  return simple_bit_vector_p(object) ? T : NIL;
}

// ### sbit1 simple-bit-vector index => bit
Value SYS_sbit1(Value arg1, Value arg2)
{
  return check_simple_bit_vector(arg1)->inline_aref(check_index(arg2));
}

// ### set-sbit1 simple-bit-vector index new-value => new-value
Value SYS_set_sbit1(Value arg1, Value arg2, Value arg3)
{
  return check_simple_bit_vector(arg1)->inline_aset(check_index(arg2), arg3);
}

// ### simple-bit-vector-fill simple-bit-vector value => simple-bit-vector
Value SYS_simple_bit_vector_fill(Value arg1, Value arg2)
{
  check_simple_bit_vector(arg1)->fill(arg2);
  return arg1;
}
