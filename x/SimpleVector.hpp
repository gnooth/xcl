// SimpleVector.hpp
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

#ifndef __SIMPLE_VECTOR_HPP
#define __SIMPLE_VECTOR_HPP

class SimpleVector : public AbstractVector
{
private:
  Value _data[0];

public:
  void * operator new(size_t size, INDEX capacity)
  {
    return GC_malloc_ignore_off_page(sizeof(SimpleVector) + capacity * sizeof(Value));
  }

  SimpleVector(INDEX capacity)
    : AbstractVector(WIDETAG_SIMPLE_VECTOR, capacity)
  {
    memset(_data, 0, capacity * sizeof(Value));
  }

  SimpleVector(INDEX length, Value data[]);

  SimpleVector(Value list);

  long data_offset()
  {
    return ((long)(&(this->_data))) - ((long)this);
  }

  Value * data()
  {
    return _data;
  }

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

  virtual Value element_type() const
  {
    return T;
  }

  virtual Value type_of() const;

  virtual bool typep(Value type) const;

  virtual void fill(Value value);

  // not virtual
  inline Value inline_aref(INDEX i) const;

  virtual Value aref(INDEX i) const;

  // not virtual
  inline Value inline_aset(INDEX i, Value new_value);

  // not virtual
  inline Value inline_xaset(INDEX i, Value new_value);

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

  virtual Value subseq(INDEX start, INDEX end) const;

  virtual AbstractVector * adjust_vector(INDEX new_capacity,
                                         Value initial_element,
                                         Value initial_contents);

  virtual AbstractVector * displace_vector(INDEX new_capacity,
                                           AbstractArray * displaced_to,
                                           INDEX offset);
};

inline SimpleVector * new_simple_vector(INDEX capacity)
{
  return new(capacity) SimpleVector(capacity);
}

inline SimpleVector * new_simple_vector(INDEX length, Value data[])
{
  return new(length) SimpleVector(length, data);
}

inline SimpleVector * new_simple_vector(Value list)
{
  return new(::length(list)) SimpleVector(list);
}

inline bool simple_vector_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SIMPLE_VECTOR);
}

inline SimpleVector * the_simple_vector(Value value)
{
  assert(simple_vector_p(value));
  return reinterpret_cast<SimpleVector *>(value - LOWTAG_TYPED_OBJECT);
}

inline SimpleVector * check_simple_vector(Value value)
{
  if (simple_vector_p(value))
    return the_simple_vector(value);
  signal_type_error(value, S_simple_vector);
  // not reached
  return NULL;
}

extern long vector_capacity_offset();

extern long simple_vector_data_offset();

#endif // SimpleVector.hpp
