// Array_T.hpp
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

#ifndef __ARRAY_T_HPP
#define __ARRAY_T_HPP

class Array_T : public AbstractArray
{
private:
  // non-displaced arrays
  Value * _data;

  // displaced arrays
  AbstractArray * _array;
  INDEX _offset;

  unsigned int _rank;
  INDEX * _dimensions;
  INDEX _total_size;
  Value _element_type;

  unsigned long set_initial_contents(unsigned long axis, unsigned long ndims,
                                     unsigned long dims[], Value contents,
                                     unsigned long index);

public:
  Array_T(unsigned long rank, unsigned long dimensions[], Value element_type,
          Value initial_contents, Value initial_element);

  // displaced
  Array_T(unsigned long rank, unsigned long dimensions[], AbstractArray * array,
          unsigned long offset);

  virtual bool is_adjustable() const
  {
    return true;
  }

  virtual Value type_of() const
  {
    return list3(S_array, _element_type, dimensions());
  }

  virtual bool typep(Value type) const;

  virtual Value element_type() const
  {
    return _element_type;
  }

  virtual unsigned int rank() const
  {
    return _rank;
  }

  virtual Value dimensions() const;

  virtual INDEX dimension(unsigned int n) const;

  virtual unsigned long total_size() const
  {
    return _total_size;
  }

  virtual bool is_displaced() const
  {
    return _array != NULL;
  }

  virtual Value displacement() const;

  // row-major-aref
  virtual Value aref(unsigned long i) const;
  virtual Value aset(unsigned long i, Value new_value);

  AbstractString * write_to_string();
};

#endif // Array_T.hpp
