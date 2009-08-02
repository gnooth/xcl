// ZeroRankArray.hpp
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

#ifndef __ZERO_RANK_ARRAY_HPP
#define __ZERO_RANK_ARRAY_HPP

class ZeroRankArray : public AbstractArray
{
private:
  Value _element_type;
  Value _data;
  AbstractArray * _array;
  INDEX _offset;

public:
  ZeroRankArray(Value element_type, Value data)
    : AbstractArray(WIDETAG_SIMPLE_ARRAY_T), _element_type(element_type), _data(data),
      _array(NULL), _offset(0)
  {
  }

  ZeroRankArray(AbstractArray * array, INDEX offset)
    : AbstractArray(WIDETAG_SIMPLE_ARRAY_T), _element_type(array->element_type()), _data(NULL_VALUE),
      _array(array), _offset(offset)
  {
  }

  virtual bool is_adjustable() const
  {
    return true; // REVIEW
  }

  virtual Value type_of() const;

  virtual bool typep(Value type) const;

  virtual Value element_type() const
  {
    return _element_type;
  }

  virtual unsigned int rank() const
  {
    return 0;
  }

  virtual Value dimensions() const
  {
    return NIL;
  }

  virtual INDEX dimension(unsigned int n) const;

  virtual INDEX total_size() const
  {
    return 1;
  }

  // row-major-aref
  virtual Value aref(INDEX i) const;
  virtual Value aset(INDEX i, Value new_value);

  virtual AbstractString * write_to_string();
};

#endif // ZeroRankArray.hpp
