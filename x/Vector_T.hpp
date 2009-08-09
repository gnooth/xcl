// Vector_T.hpp
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

#ifndef __VECTOR_T_HPP
#define __VECTOR_T_HPP

class Vector_T : public AbstractVector
{
private:
  bool _has_fill_pointer;
  INDEX _fill_pointer;
  Value * _data;

  // displaced
  AbstractArray * _array;
  INDEX _offset;

  void ensure_capacity(INDEX n);

  Value _push(Value new_element);

public:
  Vector_T(INDEX capacity, Value fill_pointer);

  // displaced
  Vector_T(INDEX capacity, AbstractArray * array, INDEX offset, Value fill_pointer);

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

  virtual Value element_type() const
  {
    return T;
  }

  virtual Value type_of() const;

  virtual bool typep(Value type) const;

  virtual void fill(Value value);

  virtual Value aref(INDEX i) const;
  virtual Value aset(INDEX i, Value new_value);

  virtual Value elt(INDEX i) const;

  virtual Value push(Value new_element);
  virtual Value push_extend(Value new_element, INDEX extension);

  virtual Value push_extend(Value new_element);

  virtual Value pop();

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

#endif // Vector_T.hpp
