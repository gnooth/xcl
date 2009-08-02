// BitVector.hpp
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

#ifndef __BIT_VECTOR_HPP
#define __BIT_VECTOR_HPP

class BitVector : public AbstractBitVector
{
private:
  bool _has_fill_pointer;
  unsigned long _fill_pointer;
  unsigned int * _data;

  // displaced
  AbstractArray * _array;
  unsigned long _offset;

public:
  BitVector(INDEX capacity, bool has_fill_pointer);

  // displaced
  BitVector(INDEX capacity, AbstractArray * array, INDEX offset, Value fill_pointer);

  void ensure_capacity(INDEX required_capacity);

  virtual void setbit(INDEX index);
  virtual void clearbit(INDEX index);
  virtual BIT getbit(INDEX index) const;
  virtual void setbit(INDEX index, BIT bit);

  virtual bool has_fill_pointer() const
  {
    return _has_fill_pointer;
  }

  virtual bool is_adjustable() const
  {
    return true;
  }

  virtual INDEX length() const
  {
    return _has_fill_pointer ? _fill_pointer : _capacity;
  }

  virtual void set_length(INDEX length)
  {
    if (_has_fill_pointer)
      {
        assert(length <= _capacity);
        _fill_pointer = length;
      }
    else
      no_fill_pointer();
  }

  virtual bool is_displaced() const
  {
    return _array != NULL;
  }

  virtual Value displacement() const;

  virtual Value type_of() const
  {
    return list2(S_bit_vector, make_fixnum(_capacity));
  }

  virtual bool typep(Value type) const;

  virtual void fill(Value value);

  virtual Value aref(unsigned long i) const;
  virtual Value aset(unsigned long i, Value new_value);

  virtual Value elt(unsigned long i) const;

  virtual Value push(Value new_element);
  virtual Value push_extend(Value new_element, unsigned long extension);

  virtual Value pop();

  virtual Value reverse() const;
  virtual Value nreverse();

  virtual AbstractVector * adjust_vector(unsigned long new_capacity,
                                         Value initial_element,
                                         Value initial_contents);

  virtual AbstractVector * displace_vector(unsigned long new_capacity,
                                           AbstractArray * displaced_to,
                                           unsigned long offset);
};

#endif // BitVector.hpp
