// SimpleVector.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "SimpleVector.hpp"

SimpleVector::SimpleVector(INDEX capacity, Value data[])
  : AbstractVector(WIDETAG_SIMPLE_VECTOR, capacity)
{
  for (INDEX i = capacity; i-- > 0;)
    _data[i] = data[i];
}

SimpleVector::SimpleVector(Value list)
  : AbstractVector(WIDETAG_SIMPLE_VECTOR)
{
  _capacity = ::length(check_list(list));
  INDEX i = 0;
  while (list != NIL)
    {
      assert(i < _capacity);
      _data[i++] = xcar(list);
      list = xcdr(list);
    }
}

Value SimpleVector::type_of() const
{
  return list2(S_simple_vector, make_fixnum(_capacity));
}

bool SimpleVector::typep(Value type) const
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
              tail = xcdr(tail);
              if (element_type == UNSPECIFIED
                  || upgraded_array_element_type(element_type) == T)
                {
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
    }
  else if (symbolp(type))
    {
      if (type == S_simple_vector || type == S_vector || type == S_sequence
          || type == S_simple_array || type == S_array || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_vector || type == C_array || type == C_sequence)
        return true;
    }
  return false;
}

void SimpleVector::fill(Value value)
{
  for (INDEX i = _capacity; i-- > 0;)
    _data[i] = value;
}

inline Value SimpleVector::inline_aref(INDEX i) const
{
  if (i < _capacity)
    return _data[i];
  return bad_index(i, 0, _capacity);
}

// "AREF ignores fill pointers. It is permissible to use AREF to access any
// array element, whether active or not."
Value SimpleVector::aref(INDEX i) const
{
  if (i >= _capacity)
    {
      Value datum = make_number(i);
      Value expected_type =
        list3(S_integer, FIXNUM_ZERO,
              _capacity > 0 ? make_number(_capacity - 1) : list1(FIXNUM_ZERO));
      return bad_index(datum, expected_type);
    }
  return _data[i];
}

inline Value SimpleVector::inline_aset(INDEX i, Value new_value)
{
  if (i < _capacity)
    return (_data[i] = new_value);
  return bad_index(i, 0, _capacity);
}

Value SimpleVector::aset(INDEX i, Value new_value)
{
  if (i >= _capacity)
    {
      Value datum = make_number(i);
      Value expected_type =
        list3(S_integer, FIXNUM_ZERO,
              _capacity > 0 ? make_number(_capacity - 1) : list1(FIXNUM_ZERO));
      return bad_index(datum, expected_type);
    }
  _data[i] = new_value;
  return new_value;
}

Value SimpleVector::elt(INDEX i) const
{
  if (i >= _capacity)
    {
      Value datum = make_number(i);
      Value expected_type =
        list3(S_integer, FIXNUM_ZERO,
              _capacity > 0 ? make_number(_capacity - 1) : list1(FIXNUM_ZERO));
      return bad_index(datum, expected_type);
    }
  return _data[i];
}

Value SimpleVector::reverse() const
{
  SimpleVector * result = new_simple_vector(_capacity);
  INDEX i, j;
  for (i = 0, j = _capacity - 1; i < _capacity; i++, j--)
    result->_data[i] = _data[j];
  return make_value(result);
}

Value SimpleVector::nreverse()
{
  if (_capacity > 0)
    {
      INDEX i = 0;
      INDEX j = _capacity - 1;
      while (i < j)
        {
          Value temp = _data[i];
          _data[i] = _data[j];
          _data[j] = temp;
          ++i;
          --j;
        }
    }
  return make_value(this);
}

Value SimpleVector::subseq(unsigned long start, unsigned long end) const
{
  assert(end >= start);
  SimpleVector * vector = new_simple_vector(end - start);
  unsigned long i = start, j = 0;
  while (i < end)
    vector->_data[j++] = _data[i++];
  return make_value(vector);
}

AbstractVector * SimpleVector::adjust_vector(INDEX new_capacity,
                                             Value initial_element,
                                             Value initial_contents)
{
  if (initial_contents != NIL)
    {
      Value * new_data = new (GC) Value[new_capacity];
      if (listp(initial_contents))
        {
          Value list = initial_contents;
          for (INDEX i = 0; i < new_capacity; i++)
            {
              new_data[i] = car(list);
              list = xcdr(list);
            }
        }
      else if (vectorp(initial_contents))
        {
          AbstractVector * v = the_vector(initial_contents);
          for (INDEX i = 0; i < new_capacity; i++)
            new_data[i] = v->aref(i);
        }
      else
        signal_type_error(initial_contents, S_sequence);

      return new_simple_vector(new_capacity, new_data);
    }
  if (_capacity != new_capacity)
    {
      Value * new_data = (Value *) GC_malloc_ignore_off_page(new_capacity * sizeof(Value));
      INDEX limit = (_capacity < new_capacity) ? _capacity : new_capacity;
      for (INDEX i = 0; i < limit; i++)
        new_data[i] = _data[i];
      for (INDEX i = _capacity; i < new_capacity; i++)
        new_data[i] = initial_element;
      return new_simple_vector(new_capacity, new_data);
    }
  // no change
  return this;
}

AbstractVector * SimpleVector::displace_vector(INDEX new_capacity,
                                               AbstractArray * displaced_to,
                                               INDEX offset)
{
  return new Vector_T(new_capacity, displaced_to, offset, NIL);
}

long vector_capacity_offset()
{
  // We can't do this on the stack because we need our operator new to run.
  SimpleVector * vector = new_simple_vector((INDEX)1);
  return vector->capacity_offset();
}

long simple_vector_data_offset()
{
  // We can't do this on the stack because we need our operator new to run.
  SimpleVector * vector = new_simple_vector((INDEX)1);
  return vector->data_offset();
}

// ### simple-vector-p object => generalized-boolean
Value CL_simple_vector_p(Value arg)
{
  return simple_vector_p(arg) ? T : NIL;
}

// ### make-simple-vector size => simple-vector
Value SYS_make_simple_vector(Value arg)
{
  return make_value(new_simple_vector(check_index(arg)));
}

// ### vector
Value CL_vector(unsigned int numargs, Value args[])
{
  return make_value(new_simple_vector(numargs, args));
}

// ### vector2
Value SYS_vector2(Value arg1, Value arg2)
{
  SimpleVector * vector = new_simple_vector((INDEX)2);
  vector->inline_xaset(0, arg1);
  vector->inline_xaset(1, arg2);
  return make_value(vector);
}

// ### vector3
Value SYS_vector3(Value arg1, Value arg2, Value arg3)
{
  SimpleVector * vector = new_simple_vector((INDEX)3);
  vector->inline_xaset(0, arg1);
  vector->inline_xaset(1, arg2);
  vector->inline_xaset(2, arg3);
  return make_value(vector);
}

// ### svref simple-vector index => element
Value CL_svref(Value arg1, Value arg2)
{
  // "SVREF is identical to AREF except that it requires its first argument to
  // be a simple vector."
  return check_simple_vector(arg1)->inline_aref(check_index(arg2));
}

// ### %svref simple-vector index => element
Value SYS_xsvref(Value arg1, Value arg2)
{
  return the_simple_vector(arg1)->inline_aref(check_index(arg2));
}

// ### svset simple-vector index new-element => new-element
Value SYS_svset(Value arg1, Value arg2, Value arg3)
{
  return check_simple_vector(arg1)->inline_aset(check_index(arg2), arg3);
}

// ### %svset simple-vector index new-element => new-element
Value SYS_xsvset(Value arg1, Value arg2, Value arg3)
{
  return the_simple_vector(arg1)->inline_aset(check_index(arg2), arg3);
}
