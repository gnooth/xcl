// SimpleBitVector.hpp
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

#ifndef __SIMPLE_BIT_VECTOR_HPP
#define __SIMPLE_BIT_VECTOR_HPP

class SimpleBitVector : public AbstractBitVector
{
private:
  unsigned int _data[0];
public:
  void * operator new(size_t size, INDEX capacity)
  {
    INDEX data_length = capacity >> BIT_VECTOR_SHIFT;
    if ((capacity & BIT_VECTOR_MASK) != 0)
      ++data_length;
    return GC_malloc_ignore_off_page(sizeof(SimpleBitVector) + data_length * sizeof(unsigned int));
  }

  SimpleBitVector(INDEX capacity)
    : AbstractBitVector(WIDETAG_SIMPLE_BIT_VECTOR, capacity)
  {
    INDEX data_length = capacity >> BIT_VECTOR_SHIFT;
    if ((capacity & BIT_VECTOR_MASK) != 0)
      ++data_length;
    for (INDEX i = data_length; i-- > 0;)
      _data[i] = 0;
  }

  SimpleBitVector(AbstractString * string);

  long data_offset()
  {
    return ((long)(&(this->_data))) - ((long)this);
  }

  unsigned int * data()
  {
    return _data;
  }

  // not virtual
  inline void inline_setbit(INDEX index)
  {
    INDEX offset = index >> BIT_VECTOR_SHIFT;
    _data[offset] |= 1 << (index & BIT_VECTOR_MASK);
  }

  // not virtual
  inline void inline_clearbit(INDEX index)
  {
    INDEX offset = index >> BIT_VECTOR_SHIFT;
    _data[offset] &= ~(1 << (index & BIT_VECTOR_MASK));
  }

  // not virtual
  inline BIT inline_getbit(INDEX index) const
  {
    INDEX offset = index >> BIT_VECTOR_SHIFT;
    return (_data[offset] & (1 << (index & BIT_VECTOR_MASK))) != 0 ? 1 : 0;
  }

  // not virtual
  inline void inline_setbit(INDEX index, BIT bit)
  {
    assert(bit == 0 || bit == 1);
    if (bit == 0)
      clear_bit(index);
    else
      set_bit(index);
  }

  virtual void set_bit(INDEX index);

  virtual void clear_bit(INDEX index);

  virtual BIT get_bit(INDEX index) const;

  virtual void set_bit(INDEX index, BIT bit);

  virtual bool has_fill_pointer() const
  {
    return false;
  }

  virtual bool is_adjustable() const
  {
    return false;
  }

  virtual INDEX length() const
  {
    return _capacity;
  }

  virtual void set_length(INDEX length)
  {
    no_fill_pointer();
  }

  virtual bool is_displaced() const
  {
    return false;
  }

  virtual Value displacement() const
  {
    return current_thread()->set_values(NIL, FIXNUM_ZERO);
  }

  virtual Value type_of() const
  {
    return list2(S_simple_bit_vector, make_fixnum(_capacity));
  }

  virtual bool typep(Value type) const;

  virtual void fill(Value value);

  // not virtual
  inline Value inline_aref(INDEX i) const;

  virtual Value aref(INDEX i) const;

  // not virtual
  Value inline_aset(INDEX i, Value new_value);

  virtual Value aset(INDEX i, Value new_value);

  virtual Value elt(INDEX i) const;

  virtual Value push(Value new_element)
  {
    return no_fill_pointer();
  }

  virtual Value push_extend(Value new_element, INDEX extension)
  {
    return no_fill_pointer();
  }

  virtual Value pop()
  {
    return no_fill_pointer();
  }

  virtual Value reverse() const;

  virtual Value nreverse();

  virtual AbstractVector * adjust_vector(INDEX new_capacity,
                                         Value initial_element,
                                         Value initial_contents);

  virtual AbstractVector * displace_vector(INDEX new_capacity,
                                           AbstractArray * displaced_to,
                                           INDEX offset);
};

inline SimpleBitVector * new_simple_bit_vector(INDEX capacity)
{
  return new(capacity) SimpleBitVector(capacity);
}

inline SimpleBitVector * new_simple_bit_vector(AbstractString * string)
{
  return new(string->length()) SimpleBitVector(string);
}

inline bool simple_bit_vector_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SIMPLE_BIT_VECTOR);
}

inline SimpleBitVector * the_simple_bit_vector(Value value)
{
  assert(simple_bit_vector_p(value));
  return reinterpret_cast<SimpleBitVector *>(value - LOWTAG_TYPED_OBJECT);
}

inline SimpleBitVector * check_simple_bit_vector(Value value)
{
  if (simple_bit_vector_p(value))
    return the_simple_bit_vector(value);
  signal_type_error(value, S_simple_bit_vector);
  // not reached
  return NULL;
}

extern long simple_bit_vector_data_offset();

#endif // SimpleBitVector.hpp
