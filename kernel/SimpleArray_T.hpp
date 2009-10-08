// SimpleArray_T.hpp
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

#ifndef __SIMPLE_ARRAY_T_HPP
#define __SIMPLE_ARRAY_T_HPP

class SimpleArray_T : public AbstractArray
{
private:
  Value * _data;

  unsigned int _rank;
  unsigned long * _dimensions;
  unsigned long _total_size;
  Value _element_type;

  unsigned long set_initial_contents(unsigned long axis, unsigned long ndims,
                                     unsigned long dims[], Value contents,
                                     unsigned long index);

public:
  SimpleArray_T(unsigned long rank, unsigned long dimensions[],
                Value element_type, Value initial_contents,
                Value initial_element);

  SimpleArray_T(unsigned long rank, Value initial_contents);

  virtual bool is_adjustable() const
  {
    return false;
  }

  virtual Value type_of() const
  {
    return list3(S_simple_array, _element_type, dimensions());
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
    return false;
  }

  virtual Value displacement() const
  {
    return current_thread()->set_values(NIL, FIXNUM_ZERO);
  }

  // row-major-aref
  virtual Value aref(unsigned long i) const;
  virtual Value aset(unsigned long i, Value new_value);

  virtual AbstractArray * adjust_array(unsigned long rank,
                                       unsigned long dimensions[],
                                       Value initial_element,
                                       Value initial_contents);

  virtual AbstractArray * displace_array(unsigned long rank,
                                         unsigned long dimensions[],
                                         AbstractArray * displaced_to,
                                         unsigned long offset);

  virtual AbstractString * write_to_string();
};

#endif // SimpleArray_T.hpp
