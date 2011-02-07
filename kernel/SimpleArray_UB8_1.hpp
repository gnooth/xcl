// SimpleArray_UB8_1.hpp
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

#ifndef __SIMPLEARRAY_UB8_1_HPP
#define __SIMPLEARRAY_UB8_1_HPP

class SimpleArray_UB8_1 : public AbstractVector
{
private:
  BYTE _data[0];

public:
  void * operator new(size_t size, INDEX capacity)
  {
    return GC_malloc_atomic_ignore_off_page(sizeof(SimpleArray_UB8_1) + capacity * sizeof(BYTE));
  }

  SimpleArray_UB8_1(INDEX capacity)
    : AbstractVector(WIDETAG_SIMPLE_ARRAY_UB8_1, capacity)
  {
    memset(_data, 0, capacity);
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

  virtual Value element_type() const;

  virtual Value type_of() const;

  virtual bool typep(Value type) const;

  unsigned char * data()
  {
    return _data;
  }

  virtual void fill(Value value);

  virtual Value aref(INDEX i) const;
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
};

inline SimpleArray_UB8_1 * new_simple_array_ub8_1(INDEX capacity)
{
  return new(capacity) SimpleArray_UB8_1(capacity);
}

inline bool simple_array_ub8_1_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SIMPLE_ARRAY_UB8_1);
}

inline SimpleArray_UB8_1 * the_simple_array_ub8_1(Value value)
{
  assert(simple_array_ub8_1_p(value));
  return reinterpret_cast<SimpleArray_UB8_1 *>(value - LOWTAG_TYPED_OBJECT);
}

inline SimpleArray_UB8_1 * check_simple_array_ub8_1(Value value)
{
  if (simple_array_ub8_1_p(value))
    return the_simple_array_ub8_1(value);
  Value expected_type = list3(S_simple_array, S_unsigned_byte, list1(make_fixnum(8)));
  signal_type_error(value, expected_type);
  // not reached
  return NULL;
}

#endif // SimpleArray_UB8_1.hpp
