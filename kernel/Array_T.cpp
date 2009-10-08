// Array_T.cpp
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
#include "Array_T.hpp"
#include "primitives.hpp"

Array_T::Array_T(unsigned long rank, unsigned long dimensions[], Value element_type,
                 Value initial_contents, Value initial_element)
  : AbstractArray(WIDETAG_ARRAY_T), _array(NULL), _offset(0), _rank(rank),
    _dimensions(dimensions), _element_type(element_type)
{
  _total_size = compute_total_size(rank, dimensions);
  _data = (Value *) GC_malloc_ignore_off_page(_total_size * sizeof(Value));
  if (initial_contents != NIL)
    set_initial_contents(0, rank, dimensions, initial_contents, 0);
  else
    for (unsigned long i = _total_size; i-- > 0;)
      _data[i] = initial_element;
}

// displaced
Array_T::Array_T(unsigned long rank, unsigned long dimensions[],
                 AbstractArray * array, unsigned long offset)
  : AbstractArray(WIDETAG_ARRAY_T), _data(NULL), _array(array), _offset(offset),
    _rank(rank), _dimensions(dimensions)
{
  _total_size = compute_total_size(rank, dimensions);
  _element_type = array->element_type();
}

unsigned long Array_T::set_initial_contents(unsigned long axis, unsigned long rank,
                                            unsigned long dims[], Value contents,
                                            unsigned long index)
{
  if (rank == 0)
    {
      if (index < _total_size)
        _data[index] = contents;
      else
        {
          signal_lisp_error("Bad initial contents for array.");
          // not reached
          return 0;
        }
      ++index;
    }
  else
    {
      unsigned long dim = dims[0];
      if (dim != length(contents))
        {
          signal_lisp_error("Bad initial contents for array.");
          // Not reached.
          return 0;
        }
      unsigned long * new_dims =
        (unsigned long *) GC_malloc_atomic((rank - 1) * sizeof(unsigned long));
      for (unsigned long i = 1; i < rank; i++)
        new_dims[i - 1] = dims[i];
      if (listp(contents))
        {
          for (unsigned long i = length(contents); i-- > 0;)
            {
              Value content = car(contents);
              index = set_initial_contents(axis + 1, rank - 1, new_dims, content, index);
              contents = xcdr(contents);
            }
        }
      else
        {
          AbstractVector * v = check_vector(contents);
          const unsigned long length = v->length();
          for (unsigned long i = 0; i < length; i++)
            {
              Value content = v->aref(i);
              index = set_initial_contents(axis + 1, rank - 1, new_dims, content, index);
            }
        }
    }
  return index;
}

bool Array_T::typep(Value type) const
{
  if (consp(type))
    {
      Value type_specifier_atom = xcar(type);
      Value tail = xcdr(type);
      if (type_specifier_atom == S_array)
        {
          if (consp(tail))
            {
              Value element_type = xcar(tail);
              tail = xcdr(tail);
              if (element_type == UNSPECIFIED || ::equal(element_type, _element_type)
                  || (_element_type == S_bit && ::equal(element_type, BIT_TYPE)))
                {
                  if (tail == NIL)
                    return true;
                  if (::length(tail) == 1)
                    {
                      Value dimensions = xcar(tail);
                      if (dimensions == UNSPECIFIED)
                        return true;
                      if (dimensions == make_fixnum(_rank))
                        return true;
                      if (consp(dimensions))
                        {
                          if (::length(dimensions) == _rank)
                            {
                              unsigned long i = 0;
                              while (dimensions != NIL)
                                {
                                  Value dim = xcar(dimensions);
                                  if (dim == UNSPECIFIED || dim == make_fixnum(_dimensions[i]))
                                    ; // ok
                                  else
                                    return false;
                                  dimensions = xcdr(dimensions);
                                  ++i;
                                }
                              return true;
                            }
                        }
                    }
                }
            }
        }
    }
  else if (symbolp(type))
    {
      if (type == S_array || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_array || type == C_t)
        return true;
    }
  return false;
}

Value Array_T::dimensions() const
{
    Value result = NIL;
    for (unsigned long i = _rank; i-- > 0;)
      result = make_cons(make_fixnum(_dimensions[i]), result);
    return result;
}

INDEX Array_T::dimension(unsigned int n) const
{
  if (n < _rank)
    return _dimensions[n];

  signal_type_error(make_number(n),
                    list3(S_integer, FIXNUM_ZERO, make_number(_rank - 1)));
  // not reached
  return 0;
}

Value Array_T::displacement() const
{
  return current_thread()->set_values(_array != NULL ? make_value(_array) : NIL,
                                      make_fixnum(_offset));
}

Value Array_T::aref(unsigned long index) const
{
    if (_data)
      {
        if (index < _total_size)
          return _data[index];
        else
          return signal_type_error(make_number(index),
                                   list3(S_integer, FIXNUM_ZERO, make_number(_total_size)));
      }
    else
      {
        // displaced
        return _array->aref(index + _offset);
      }
}

Value Array_T::aset(unsigned long index, Value new_value)
{
    if (_data)
      {
        if (index < _total_size)
          {
            _data[index] = new_value;
            return new_value;
          }
        else
          return signal_type_error(make_number(index),
                                   list3(S_integer, FIXNUM_ZERO, make_number(_total_size)));
      }
    else
      {
        // displaced
        return _array->aset(index + _offset, new_value);
      }
}

AbstractString * Array_T::write_to_string()
{
  return write_to_string_internal((int)_rank, _dimensions);
}
