// SimpleBitVector.cpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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
#include "ProgramError.hpp"

void SimpleBitVector::set_bit(INDEX index)
{
  inline_setbit(index);
}

void SimpleBitVector::clear_bit(INDEX index)
{
  inline_clearbit(index);
}

BIT SimpleBitVector::get_bit(INDEX index) const
{
  return inline_getbit(index);
}

void SimpleBitVector::set_bit(INDEX index, BIT bit)
{
  inline_setbit(index, bit);
}

SimpleBitVector::SimpleBitVector(AbstractString * string)
  : AbstractBitVector(WIDETAG_SIMPLE_BIT_VECTOR)
{
  const INDEX len = string->length();
  _capacity = len;
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
  SimpleBitVector * result = new_simple_bit_vector(capacity);
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
      SimpleBitVector * bv = new_simple_bit_vector(new_capacity);
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
      SimpleBitVector * bv = new_simple_bit_vector(new_capacity);
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
  // no change
  return this;
}

AbstractVector * SimpleBitVector::displace_vector(INDEX new_capacity,
                                                  AbstractArray * displaced_to,
                                                  INDEX offset)
{
  return new BitVector(new_capacity, displaced_to, offset, NIL);
}

long simple_bit_vector_data_offset()
{
  SimpleBitVector * bv = new_simple_bit_vector(1);
  return bv->data_offset();
}

// ### simple-bit-vector-p object => generalized-boolean
Value CL_simple_bit_vector_p(Value object)
{
  return simple_bit_vector_p(object) ? T : NIL;
}

// ### make-simple-bit-vector size => simple-bit-vector
Value SYS_make_simple_bit_vector(Value arg)
{
  return make_value(new_simple_bit_vector(check_index(arg, 0, ARRAY_DIMENSION_LIMIT - 1)));
}

// ### %sbit1 simple-bit-vector index => bit
Value SYS_xsbit1(Value arg1, Value arg2)
{
  return make_fixnum(the_simple_bit_vector(arg1)->inline_getbit(xlong(arg2)));
}

// ### sbit1 simple-bit-vector index => bit
Value SYS_sbit1(Value arg1, Value arg2)
{
  return check_simple_bit_vector(arg1)->inline_aref(check_index(arg2));
}

// ### %set-sbit1 simple-bit-vector index new-value => new-value
Value SYS_xset_sbit1(Value arg1, Value arg2, Value arg3)
{
  return the_simple_bit_vector(arg1)->inline_aset(xlong(arg2), arg3);
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

static Value vector_length_mismatch_error()
{
  return signal_lisp_error(new ProgramError("Vector length mismatch."));
}

// ### simple-bit-vector-bit-and vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_and(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (p1[i] & p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-andc1 vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_andc1(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (~p1[i] & p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-andc2 vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_andc2(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (p1[i] & ~p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-eqv vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_eqv(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = ~(p1[i] ^ p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-ior vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_ior(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (p1[i] | p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-nand vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_nand(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = ~(p1[i] & p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-nor vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_nor(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = ~(p1[i] | p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-orc1 vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_orc1(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (~p1[i] | p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-orc2 vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_orc2(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (p1[i] | ~p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-xor vector-1 vector-2 vector-3
Value SYS_simple_bit_vector_bit_xor(Value arg1, Value arg2, Value arg3)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  SimpleBitVector * v3 = check_simple_bit_vector(arg3);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  unsigned int * p3 = v3->data();
  INDEX length = v3->length();
  if (v1->length() != length || v2->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p3[i] = (p1[i] ^ p2[i]);
  return arg3;
}

// ### simple-bit-vector-bit-not vector-1 vector-2
Value SYS_simple_bit_vector_bit_not(Value arg1, Value arg2)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  INDEX length = v2->length();
  if (v1->length() != length)
    return vector_length_mismatch_error();
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  for (INDEX i = 0; i < size; i++)
    p2[i] = ~p1[i];
  return arg2;
}

// ### simple-bit-vector-equal vector-1 vector-2
Value SYS_simple_bit_vector_equal(Value arg1, Value arg2)
{
  SimpleBitVector * v1 = check_simple_bit_vector(arg1);
  SimpleBitVector * v2 = check_simple_bit_vector(arg2);
  if (v1 == v2)
    return T;
  INDEX length = v1->length();
  if (v2->length() != length)
    return NIL;
  INDEX size = length >> BIT_VECTOR_SHIFT;
  if ((length & BIT_VECTOR_MASK) != 0)
    ++size;
  unsigned int * p1 = v1->data();
  unsigned int * p2 = v2->data();
  for (INDEX i = 0; i < size; i++)
    if (p1[i] != p2[i])
      return NIL;
  return T;
}
