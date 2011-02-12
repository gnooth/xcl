// BitVector.cpp
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

inline void BitVector::set_bit(INDEX index)
{
  if (_data)
    {
      INDEX offset = index >> BIT_VECTOR_SHIFT;
      _data[offset] |= 1L << (index & BIT_VECTOR_MASK);
    }
  else
    _array->aset(index + _offset, FIXNUM_ONE);
}

inline void BitVector::clear_bit(INDEX index)
{
  if (_data)
    {
      INDEX offset = index >> BIT_VECTOR_SHIFT;
      _data[offset] &= ~(1L << (index & BIT_VECTOR_MASK));
    }
  else
    _array->aset(index + _offset, FIXNUM_ZERO);
}

inline BIT BitVector::get_bit(INDEX index) const
{
  if (_data)
    {
      INDEX offset = index >> BIT_VECTOR_SHIFT;
      return (_data[offset] & (1L << (index & BIT_VECTOR_MASK))) != 0 ? 1 : 0;
    }
  else
    return check_bit(_array->aref(index + _offset));
}

inline void BitVector::set_bit(INDEX index, BIT bit)
{
  assert(bit == 0 || bit == 1);
  if (bit == 0)
    clear_bit(index);
  else
    set_bit(index);
}

BitVector::BitVector(INDEX capacity, bool has_fill_pointer)
  : AbstractBitVector(WIDETAG_BIT_VECTOR, capacity), _has_fill_pointer(has_fill_pointer),
    _fill_pointer(0), _array(NULL), _offset(0)
{
  INDEX size = capacity >> BIT_VECTOR_SHIFT;
  if ((capacity & BIT_VECTOR_MASK) != 0)
    ++size;
  _data = (unsigned int *) GC_malloc_atomic(size * sizeof(unsigned int));
  for (INDEX i = 0; i < size; i++)
    _data[i] = 0;
}

// displaced
BitVector::BitVector(INDEX capacity, AbstractArray * array, INDEX offset, Value fill_pointer)
  : AbstractBitVector(WIDETAG_BIT_VECTOR, capacity), _data(NULL), _array(array), _offset(offset)
{
  if (fill_pointer == NIL)
    {
      _has_fill_pointer = false;
      _fill_pointer = 0;
    }
  else
    {
      _has_fill_pointer = true;
      _fill_pointer = (fill_pointer == T) ? capacity : check_index(fill_pointer, 0, capacity);
    }
}

Value BitVector::displacement() const
{
  return current_thread()->set_values(_array != NULL ? make_value(_array) : NIL,
                                      make_fixnum(_offset));
}

bool BitVector::typep(Value type) const
{
  if (consp(type))
    {
      Value type_specifier_atom = xcar(type);
      Value tail = xcdr(type);
      if (type_specifier_atom == S_array)
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
      else if (type_specifier_atom == S_bit_vector)
        {
          if (::equal(tail, list1(UNSPECIFIED)))
            return true;
          if (::equal(tail, list1(make_fixnum(_capacity))))
            return true;
        }
    }
  else if (symbolp(type))
    {
      if (type == S_bit_vector || type == S_vector || type == S_sequence
          || type == S_simple_array || type == S_array || type == S_atom || type == T)
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

// FIXME enforce array-dimension-limit
// REVIEW not thread-safe
void BitVector::ensure_capacity(INDEX required_capacity)
{
  if (_data)
    {
      if (_capacity < required_capacity)
        {
          INDEX old_size = _capacity >> BIT_VECTOR_SHIFT;
          if ((_capacity & BIT_VECTOR_MASK) != 0)
            ++old_size;

          // FIXME check for overflow
          INDEX new_capacity = _capacity * 2;
          if (new_capacity < required_capacity)
            new_capacity = required_capacity;

          INDEX new_size = new_capacity >> BIT_VECTOR_SHIFT;
          if ((new_capacity & BIT_VECTOR_MASK) != 0)
            ++new_size;
          unsigned int * new_data =
            (unsigned int *) GC_malloc_atomic(new_size * sizeof(unsigned int));
          INDEX i;
          for (i = 0; i < old_size; i++)
            new_data[i] = _data[i];
          while (i < new_size)
            new_data[i++] = 0;
          _data = new_data;
          _capacity = new_capacity;
        }
    }
  else
    {
      assert(_array != NULL);
      if (_capacity < required_capacity || _array->total_size() - _offset < required_capacity)
        {
          // Copy array.
          unsigned long size = required_capacity >> BIT_VECTOR_SHIFT;
          if ((size & BIT_VECTOR_MASK) != 0)
            ++size;
          _data =
            (unsigned int *) GC_malloc_atomic(size * sizeof(unsigned int));
          unsigned long limit = _array->total_size() - _offset;
          if (limit > _capacity)
            limit = _capacity;
          for (unsigned long i = 0; i < limit; i++)
            {
              BIT bit = check_bit(_array->aref(i + _offset));
              if (bit)
                set_bit(i);
              else
                clear_bit(i);
            }
          _capacity = required_capacity;
          _array = NULL;
          _offset = 0;
        }
    }
}

void BitVector::fill(Value value)
{
  const BIT bit = check_bit(value);
  for (INDEX i = _capacity; i-- > 0;)
    set_bit(i, bit);
}

// "AREF ignores fill pointers. It is permissible to use AREF to access any
// array element, whether active or not."
Value BitVector::aref(unsigned long i) const
{
  if (i >= _capacity)
    return bad_index(i);
  return make_fixnum(get_bit(i));
}

Value BitVector::aset(INDEX i, Value new_value)
{
  if (i >= _capacity)
    return bad_index(i);
  set_bit(i, check_bit(new_value));
  return new_value;
}

Value BitVector::elt(INDEX i) const
{
  if (i >= length())
    return bad_index(i);
  return make_fixnum(get_bit(i));
}

Value BitVector::push(Value new_element)
{
  check_fill_pointer();
  if (_fill_pointer < _capacity)
    {
      unsigned long old_length = _fill_pointer;
      set_bit(_fill_pointer++, check_bit(new_element));
      return make_fixnum(old_length);
    }
  else
    return NIL;
}

Value BitVector::push_extend(Value new_element, unsigned long extension)
{
  check_fill_pointer();
  if (_fill_pointer >= _capacity)
    ensure_capacity(_fill_pointer + extension);
  assert(_fill_pointer < _capacity);
  unsigned long old_length = _fill_pointer;
  set_bit(_fill_pointer++, check_bit(new_element));
  return make_fixnum(old_length);
}

Value BitVector::pop()
{
  check_fill_pointer();
  if (_fill_pointer > 0)
    return make_fixnum(get_bit(--_fill_pointer));
  else
    return signal_lisp_error("There is nothing left to pop.");
}

Value BitVector::reverse() const
{
  const unsigned long len = length();
  SimpleBitVector * result = new_simple_bit_vector(len);
  unsigned long i, j;
  for (i = 0, j = len - 1; i < len; i++, j--)
    result->set_bit(i, get_bit(j));
  return make_value(result);
}

Value BitVector::nreverse()
{
  const unsigned long len = length();
  if (len > 0)
    {
      unsigned long i = 0;
      unsigned long j = len - 1;
      while (i < j)
        {
          unsigned long temp = get_bit(i);
          set_bit(i, get_bit(j));
          set_bit(j, temp);
          ++i;
          --j;
        }
    }
  return make_value(this);
}

AbstractVector * BitVector::adjust_vector(unsigned long new_capacity,
                                          Value initial_element,
                                          Value initial_contents)
{
  if (_data == NULL)
    {
      // Copy array.
      unsigned long size = _capacity >> BIT_VECTOR_SHIFT;
      if ((_capacity & BIT_VECTOR_MASK) != 0)
        ++size;
      _data = (unsigned int *) GC_malloc_atomic(size * sizeof(unsigned int));
      for (unsigned long i = 0; i < _capacity; i++)
        set_bit(i, check_bit(_array->aref(_offset + i)));
      _array = NULL;
      _offset = 0;
    }
  unsigned long new_size = new_capacity >> BIT_VECTOR_SHIFT;
  if ((new_capacity & BIT_VECTOR_MASK) != 0)
    ++new_size;
  if (initial_contents != NIL)
    {
      _data = (unsigned int *) GC_malloc_atomic(new_size * sizeof(unsigned int));
      if (consp(initial_contents))
        {
          Value list = initial_contents;
          for (unsigned long i = 0; i < new_capacity; i++)
            {
              set_bit(i, check_bit(car(list)));
              list = xcdr(list);
            }
        }
      else if (vectorp(initial_contents))
        {
          AbstractVector * v = the_vector(initial_contents);
          for (unsigned long i = 0; i < new_capacity; i++)
            set_bit(i, check_bit(v->aref(i)));
        }
      else
        signal_type_error(initial_contents, S_sequence);
    }
  else if (_capacity != new_capacity)
    {
      unsigned int * new_data =
        (unsigned int *) GC_malloc_atomic(new_size * sizeof(unsigned int));
      unsigned long old_size = new_capacity >> BIT_VECTOR_SHIFT;
      if ((new_capacity & BIT_VECTOR_MASK) != 0)
        ++old_size;
      unsigned long limit = new_size < old_size ? new_size : old_size;
      for (unsigned long i = 0; i < limit; i++)
        new_data[i] = _data[i];
      _data = new_data;
      if (new_capacity > _capacity)
        {
          unsigned long bit = check_bit(initial_element);
          if (bit)
            for (unsigned long i = _capacity; i < new_capacity; i++)
              set_bit(i);
          else
            for (unsigned long i = _capacity; i < new_capacity; i++)
              clear_bit(i);
        }
    }
  _capacity = new_capacity;
  // "The consequences are unspecified if array is adjusted to a size smaller
  // than its fill pointer without supplying the fill-pointer argument so that
  // its fill-pointer is properly adjusted in the process."
  if (_fill_pointer > _capacity)
    _fill_pointer = _capacity;
  return this;
}

AbstractVector * BitVector::displace_vector(unsigned long new_capacity,
                                            AbstractArray * displaced_to,
                                            unsigned long offset)
{
  _capacity = new_capacity;
  // "The consequences are unspecified if array is adjusted to a size smaller
  // than its fill pointer without supplying the fill-pointer argument so that
  // its fill-pointer is properly adjusted in the process."
  if (_fill_pointer > _capacity)
    _fill_pointer = _capacity;
  _array = displaced_to;
  _offset = offset;
  _data = NULL;
  return this;
}
