// SimpleArray_T.cpp
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
#include "Array_T.hpp"
#include "SimpleArray_T.hpp"

SimpleArray_T::SimpleArray_T(unsigned long rank, unsigned long dimensions[],
                             Value element_type, Value initial_contents,
                             Value initial_element)
  : AbstractArray(WIDETAG_SIMPLE_ARRAY_T), _rank(rank), _dimensions(dimensions),
    _element_type(element_type)
{
  _total_size = compute_total_size(rank, dimensions);
  _data = (Value *) GC_malloc_ignore_off_page(_total_size * sizeof(Value));
  if (initial_contents != NIL)
    set_initial_contents(0, rank, dimensions, initial_contents, 0);
  else
    for (unsigned long i = _total_size; i-- > 0;)
      _data[i] = initial_element;
}

SimpleArray_T::SimpleArray_T(unsigned long rank, Value initial_contents)
  : AbstractArray(WIDETAG_SIMPLE_ARRAY_T), _rank(rank), _element_type(T)
{
  assert(rank > 1);
  _rank = rank;
  _dimensions = (unsigned long *) GC_malloc_atomic(rank * sizeof(unsigned long));
  for (unsigned long i = 0; i < rank; i++)
    _dimensions[i] = 0;
//   Value list = check_list(initial_contents);
  Value rest = initial_contents;
  for (unsigned long i = 0; i < rank; i++)
    {
      _dimensions[i] = ::length(rest);
      if (rest == NIL || ::length(rest) == 0)
        break;
      rest = CL_elt(rest, 0);
    }
  _total_size = compute_total_size(_rank, _dimensions);
  _data = (Value *) GC_malloc_ignore_off_page(_total_size * sizeof(Value));
  set_initial_contents(0, rank, _dimensions, initial_contents, 0);
}

unsigned long SimpleArray_T::set_initial_contents(unsigned long axis, unsigned long ndims,
                                                  unsigned long dims[], Value contents,
                                                  unsigned long index)
{
  if (ndims == 0)
    {
      if (index < _total_size)
        _data[index] = contents;
      else
        {
          signal_lisp_error("Bad initial contents for array.");
          // Not reached.
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
        (unsigned long *) GC_malloc_atomic((ndims - 1) * sizeof(unsigned long));
      for (unsigned long i = 1; i < ndims; i++)
        new_dims[i - 1] = dims[i];
      if (listp(contents))
        {
          for (unsigned long i = length(contents); i-- > 0;)
            {
              Value content = car(contents);
              index = set_initial_contents(axis + 1, ndims - 1, new_dims, content, index);
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
              index = set_initial_contents(axis + 1, ndims - 1, new_dims, content, index);
            }
        }
    }
  return index;
}

bool SimpleArray_T::typep(Value type) const
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
              if (element_type == UNSPECIFIED || ::equal(element_type, _element_type))
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
      if (type == S_simple_array || type == S_array || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_array || type == C_t)
        return true;
    }
  return false;
}

Value SimpleArray_T::dimensions() const
{
    Value result = NIL;
    for (unsigned long i = _rank; i-- > 0;)
      result = make_cons(make_fixnum(_dimensions[i]), result);
    return result;
}

INDEX SimpleArray_T::dimension(unsigned int n) const
{
  if (n < _rank)
    return _dimensions[n];

  signal_type_error(make_number(n),
                    list3(S_integer, FIXNUM_ZERO, make_number(_rank - 1)));
  // Not reached.
  return 0;
}

Value SimpleArray_T::aref(unsigned long index) const
{
  assert(_data != NULL);
  if (index < _total_size)
    return _data[index];
  else
    return signal_type_error(make_number(index),
                             list3(S_integer, FIXNUM_ZERO, make_number(_total_size)));
}

Value SimpleArray_T::aset(unsigned long index, Value new_value)
{
  assert(_data != NULL);
  if (index < _total_size)
    {
      _data[index] = new_value;
      return new_value;
    }
  else
    return signal_type_error(make_number(index),
                             list3(S_integer, FIXNUM_ZERO, make_number(_total_size)));
}

AbstractArray * SimpleArray_T::adjust_array(unsigned long rank, unsigned long dimensions[],
                                            Value initial_element, Value initial_contents)
{
  if (initial_contents != NIL)
    return new SimpleArray_T(rank, dimensions, _element_type, initial_contents, NIL);

  for (unsigned long i = 0; i < rank; i++)
    {
      if (dimensions[i] != _dimensions[i])
        {
          SimpleArray_T * array =
            new SimpleArray_T(rank, dimensions, _element_type, NIL, initial_element);
          copy_array(this, array);
          return array;
        }
    }

  // New dimensions are identical to old dimensions.
  return this;
}

AbstractArray * SimpleArray_T::displace_array(unsigned long rank,
                                              unsigned long dimensions[],
                                              AbstractArray * displaced_to,
                                              unsigned long offset)
{
  return new Array_T(rank, dimensions, displaced_to, offset);
}

AbstractString * SimpleArray_T::write_to_string()
{
  return write_to_string_internal((int)_rank, _dimensions);
}
