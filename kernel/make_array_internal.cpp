// make_array_internal.cpp
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
#include "NilVector.hpp"
#include "SimpleArray_T.hpp"
#include "SimpleArray_UB8_1.hpp"
#include "SimpleArray_UB16_1.hpp"
#include "SimpleArray_UB32_1.hpp"
#include "SimpleVector.hpp"
#include "Vector_UB32.hpp"
#include "ZeroRankArray.hpp"

static Value make_displaced_array(unsigned long rank, unsigned long * dims,
                                  Value adjustable, Value fill_pointer,
                                  Value displaced_to, Value displaced_index_offset)
{
  AbstractArray * array = check_array(displaced_to);
  unsigned long offset;
  if (displaced_index_offset != NIL)
    offset = check_index(displaced_index_offset);
  else
    offset = 0;
  if (rank == 0)
    return make_value(new ZeroRankArray(array, offset));
  if (rank == 1)
    {
      Value array_element_type = array->element_type();
      AbstractVector * vector;
      if (array_element_type == S_character)
        vector = new String(dims[0], array, offset, fill_pointer);
      else if (array_element_type == S_bit || equal(array_element_type, BIT_TYPE))
        vector = new BitVector(dims[0], array, offset, fill_pointer);
      else if (equal(array_element_type, UB8_TYPE))
        vector = new Vector_UB8(dims[0], array, offset, fill_pointer);
      else if (equal(array_element_type, UB16_TYPE))
        vector = new Vector_UB16(dims[0], array, offset, fill_pointer);
      else if (equal(array_element_type, UB32_TYPE))
        vector = new Vector_UB32(dims[0], array, offset, fill_pointer);
      else
        vector = new Vector_T(dims[0], array, offset, fill_pointer);
      return make_value(vector);
    }
  return make_value(new Array_T(rank, dims, array, offset));
}

// ### %make-array dimensions element-type initial-element initial-element-p
// initial-contents adjustable fill-pointer displaced-to displaced-index-offset
// => new-array
Value SYS_make_array_internal(unsigned int numargs, Value args[])
{
  if (numargs != 9)
    return wrong_number_of_arguments(S_make_array_internal, numargs, 9, 9);
  Value dimensions = args[0];
  Value element_type = args[1];
  Value initial_element = args[2];
  Value initial_element_provided = args[3];
  Value initial_contents = args[4];
  Value adjustable = args[5];
  Value fill_pointer = args[6];
  Value displaced_to = args[7];
  Value displaced_index_offset = args[8];

  if (initial_element_provided != NIL && initial_contents != NIL)
    return signal_lisp_error("Cannot specify both initial element and initial contents.");

  const unsigned long rank = listp(dimensions) ? length(dimensions) : 1;
  unsigned long * dims = (unsigned long *) GC_malloc_atomic(rank * sizeof(unsigned long));
  if (listp(dimensions))
    {
      for (unsigned long i = 0; i < rank; i++)
        {
          Value dimension = car(dimensions);
          dims[i] = check_index(dimension);
          dimensions = xcdr(dimensions);
        }
    }
  else
    dims[0] = check_index(dimensions);

  if (displaced_to != NIL)
    {
      if (initial_element_provided != NIL)
        return signal_lisp_error("Initial element may not be specified for a displaced array.");
      if (initial_contents != NIL)
        return signal_lisp_error("Initial contents may not be specified for a displaced array.");
      // FIXME Make sure element type (if specified) is compatible with
      // displaced-to array.
      return make_displaced_array(rank, dims, adjustable, fill_pointer, displaced_to, displaced_index_offset);
    }

  Value upgraded_type = upgraded_array_element_type(element_type);

  if (rank == 0)
    {
      Value data;
      if (initial_element_provided != NIL)
        data = initial_element;
      else
        data = initial_contents;
      return make_value(new ZeroRankArray(upgraded_type, data));
    }

  if (rank == 1)
    {
      const INDEX size = dims[0];
      if (size >= ARRAY_DIMENSION_LIMIT)
        {
          String * string = new String("The size specified for this array (");
          string->append_unsigned_long(size);
          string->append(") is >= ARRAY-DIMENSION-LIMIT (");
          string->append_unsigned_long(ARRAY_DIMENSION_LIMIT);
          string->append(").");
          return signal_lisp_error(string);
        }
      AbstractVector * vector;
      if (upgraded_type == S_character)
        {
          if (fill_pointer != NIL || adjustable != NIL)
            vector = new String(size, fill_pointer);
          else
            vector = new_simple_string(size);
        }
      else if (upgraded_type == S_bit || equal(upgraded_type, BIT_TYPE))
        {
          if (fill_pointer != NIL || adjustable != NIL)
            vector = new BitVector(size, fill_pointer != NIL);
          else
            vector = new_simple_bit_vector(size);
        }
      else if (upgraded_type == NIL)
        vector = new NilVector(size, fill_pointer);
      else if (equal(upgraded_type, UB8_TYPE))
        {
          if (fill_pointer != NIL || adjustable != NIL)
            vector = new Vector_UB8(size, fill_pointer);
          else
            vector = new_simple_array_ub8_1(size);
        }
      else if (equal(upgraded_type, UB16_TYPE))
        {
          if (fill_pointer != NIL || adjustable != NIL)
            vector = new Vector_UB16(size, fill_pointer);
          else
            vector = new_simple_array_ub16_1(size);
        }
      else if (equal(upgraded_type, UB32_TYPE))
        {
          if (fill_pointer != NIL || adjustable != NIL)
            vector = new Vector_UB32(size, fill_pointer);
          else
            vector = new_simple_array_ub32_1(size);
        }
      else
        {
          if (fill_pointer != NIL || adjustable != NIL)
            vector = new Vector_T(size, fill_pointer);
          else
            vector = new_simple_vector(size);
        }

      if (initial_element_provided != NIL)
        {
          // initial element was specified
          vector->fill(initial_element);
        }
      else if (initial_contents != NIL)
        {
          if (listp(initial_contents))
            {
              Value list = initial_contents;
              for (INDEX i = 0; i < size; i++)
                {
                  vector->aset(i, car(list));
                  list = cdr(list);
                }
            }
          else if (vectorp(initial_contents))
            {
              AbstractVector * v = the_vector(initial_contents);
              for (INDEX i = 0; i < size; i++)
                vector->aset(i, v->aref(i));
            }
          else
            return signal_type_error(initial_contents, S_sequence);
        }

      if (fill_pointer == T)
        vector->set_length(size);
      else if (fill_pointer != NIL)
        vector->set_length(check_index(fill_pointer, 0, size));

      return make_value(vector);
    }

  // rank > 1
  if (initial_contents != NIL)
    {
      if (adjustable != NIL)
        return make_value(new Array_T(rank, dims, element_type, initial_contents, NIL));
      else
        return make_value(new SimpleArray_T(rank, dims, element_type, initial_contents, NIL));
    }
  else
    {
      if (initial_element_provided == NIL)
        {
          if (element_type == S_bit || equal(element_type, BIT_TYPE))
            initial_element = FIXNUM_ZERO;
          else
            initial_element = NIL;
        }
      if (adjustable != NIL)
        return make_value(new Array_T(rank, dims, element_type, NIL, initial_element));
      else
        return make_value(new SimpleArray_T(rank, dims, element_type, NIL, initial_element));
    }

  return signal_lisp_error("MAKE-ARRAY: unsupported case.");
}
