// SimpleArray_UB16_1.cpp
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
#include "SimpleArray_UB16_1.hpp"

// SimpleArray_UB16_1::SimpleArray_UB16_1(INDEX capacity)
//   : AbstractVector(WIDETAG_SIMPLE_ARRAY_UB16_1, capacity)
// {
//   _data = (unsigned short *) GC_malloc_atomic(capacity * sizeof(unsigned short));
//   memset(_data, 0, capacity * sizeof(unsigned short));
// }

Value SimpleArray_UB16_1::element_type() const
{
  return UB16_TYPE;
}

Value SimpleArray_UB16_1::type_of() const
{
  return list3(S_simple_array, UB16_TYPE, list1(make_fixnum(_capacity)));
}

bool SimpleArray_UB16_1::typep(Value type) const
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
              if (element_type == UNSPECIFIED)
                ; // ok
              else
                {
                  Value upgraded_element_type = upgraded_array_element_type(element_type);
                  if (::equal(upgraded_element_type, UB16_TYPE))
                    ; // ok
                  else if (::equal(upgraded_element_type,
                                   list3(S_integer, FIXNUM_ZERO, make_fixnum(65535))))
                    ; // ok
                  else if (::equal(upgraded_element_type,
                                   list3(S_integer, FIXNUM_ZERO, list1(make_fixnum(65536)))))
                    ; // ok
                  else
                    return false;
                }
              tail = xcdr(tail);
              if (tail == NIL)
                return true;
              if (cdr(tail) == NIL) // i.e. length(tail) == 1
                {
                  Value dimensions = xcar(tail);
                  if (dimensions == UNSPECIFIED)
                    return true;
                  if (dimensions == FIXNUM_ONE)
                    return true;
                  if (::equal(dimensions, list1(UNSPECIFIED)))
                    return true;
                  if (::equal(dimensions, list1(make_fixnum(_capacity))))
                    return true;
                }
            }
        }
    }
  else if (symbolp(type))
    {
      if (type == S_vector || type == S_sequence || type == S_simple_array
          || type == S_array || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_vector || type == C_array || type == C_sequence || type == C_t)
        return true;
    }
  return false;
}

void SimpleArray_UB16_1::fill(Value value)
{
  long n = fixnum_value(value);
  if (n >= 0 && n < 65536)
    {
      for (INDEX i = 0; i < _capacity; i++)
        _data[i] = n;
    }
  else
    signal_type_error(value, UB16_TYPE);
}

// "AREF ignores fill pointers. It is permissible to use AREF to access any
// array element, whether active or not."
Value SimpleArray_UB16_1::aref(INDEX i) const
{
  if (i >= _capacity)
    return bad_index(i);
  return make_unsigned_fixnum(_data[i]);
}

Value SimpleArray_UB16_1::aset(INDEX i, Value new_value)
{
  if (i >= _capacity)
    return bad_index(i);
  if (fixnump(new_value))
    {
      long n = xlong(new_value);
      if (n >= 0 && n < 65536)
        {
          _data[i] = (unsigned short) n;
          return new_value;
        }
    }
  return signal_type_error(new_value, UB16_TYPE);
}

Value SimpleArray_UB16_1::elt(INDEX i) const
{
  if (i >= _capacity)
    return bad_index(i);
  return make_unsigned_fixnum(_data[i]);
}

Value SimpleArray_UB16_1::reverse() const
{
  SimpleArray_UB16_1 * result = new_simple_array_ub16_1(_capacity);
  INDEX i, j;
  for (i = 0, j = _capacity - 1; i < _capacity; i++, j--)
    result->_data[i] = _data[j];
  return make_value(result);
}

Value SimpleArray_UB16_1::nreverse()
{
  if (_capacity > 0)
    {
      INDEX i = 0;
      INDEX j = _capacity - 1;
      while (i < j)
        {
          unsigned short temp = _data[i];
          _data[i] = _data[j];
          _data[j] = temp;
          ++i;
          --j;
        }
    }
  return make_value(this);
}

Value SimpleArray_UB16_1::subseq(INDEX start, INDEX end) const
{
  assert(end >= start);
  const INDEX len = end - start;
  SimpleArray_UB16_1* vector = new_simple_array_ub16_1(len);
  INDEX i = start, j = 0;
  while (i < end)
    vector->_data[j++] = _data[i++];
  return make_value(vector);
}
