// SimpleArray_UB32_1.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "SimpleArray_UB32_1.hpp"
#include "check_ub32.hpp"

Value SimpleArray_UB32_1::element_type() const
{
  return UB32_TYPE;
}

Value SimpleArray_UB32_1::type_of() const
{
  return list3(S_simple_array, UB32_TYPE, list1(make_fixnum(_capacity)));
}

bool SimpleArray_UB32_1::typep(Value type) const
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
                  if (::equal(upgraded_element_type, UB32_TYPE))
                    ; // ok
#ifdef __x86_64__
                  else if (::equal(upgraded_element_type,
                                   list3(S_integer, FIXNUM_ZERO,
                                         make_unsigned_integer(4294967295))))
                    ; // ok
                  else if (::equal(upgraded_element_type,
                                   list3(S_integer, FIXNUM_ZERO,
                                         list1(make_unsigned_integer(4294967296)))))
                    ; // ok
#else
                  // 32-bit Lisp
                  // REVIEW
                  else if (::equal(upgraded_element_type,
                                   list3(S_integer, FIXNUM_ZERO,
                                         CL_one_minus(CL_expt(FIXNUM_TWO, make_fixnum(32))))))
                    ; // ok
                  else if (::equal(upgraded_element_type,
                                   list3(S_integer, FIXNUM_ZERO,
                                         list1(CL_expt(FIXNUM_TWO, make_fixnum(32))))))
                    ; // ok
#endif
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

void SimpleArray_UB32_1::fill(Value value)
{
  unsigned int n = check_ub32(value);
  for (INDEX i = 0; i < _capacity; i++)
    _data[i] = n;
}

inline Value SimpleArray_UB32_1::inline_aref(INDEX i) const
{
  if (i < _capacity)
    return make_ub32(_data[i]);
  return bad_index(i, 0, _capacity);
}

// "AREF ignores fill pointers. It is permissible to use AREF to access any
// array element, whether active or not."
Value SimpleArray_UB32_1::aref(INDEX i) const
{
  return inline_aref(i);
}

Value SimpleArray_UB32_1::inline_aset(INDEX i, Value new_value)
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  _data[i] = check_ub32(new_value);
  return new_value;
}

Value SimpleArray_UB32_1::aset(INDEX i, Value new_value)
{
  return inline_aset(i, new_value);
}

Value SimpleArray_UB32_1::elt(INDEX i) const
{
  return inline_aref(i);
}

Value SimpleArray_UB32_1::reverse() const
{
  SimpleArray_UB32_1 * result = new_simple_array_ub32_1(_capacity);
  INDEX i, j;
  for (i = 0, j = _capacity - 1; i < _capacity; i++, j--)
    result->_data[i] = _data[j];
  return make_value(result);
}

Value SimpleArray_UB32_1::nreverse()
{
  if (_capacity > 0)
    {
      INDEX i = 0;
      INDEX j = _capacity - 1;
      while (i < j)
        {
          unsigned int temp = _data[i];
          _data[i] = _data[j];
          _data[j] = temp;
          ++i;
          --j;
        }
    }
  return make_value(this);
}

Value SimpleArray_UB32_1::subseq(INDEX start, INDEX end) const
{
  assert(end >= start);
  const INDEX len = end - start;
  SimpleArray_UB32_1 * vector = new_simple_array_ub32_1(len);
  INDEX i = start, j = 0;
  while (i < end)
    vector->_data[j++] = _data[i++];
  return make_value(vector);
}
