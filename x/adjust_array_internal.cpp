// adjust_array_internal.cpp
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
#include "Array_T.hpp"
#include "NilVector.hpp"
#include "ZeroRankArray.hpp"
#include "SimpleVector.hpp"

// ### %adjust-array array new-dimensions element-type initial-element initial-element-p
// initial-contents initial-contents-p fill-pointer displaced-to displaced-index-offset
// => adjusted-array
Value SYS_adjust_array_internal(unsigned int numargs, Value args[])
{
  if (numargs != 10)
    return wrong_number_of_arguments(S_make_array_internal, numargs, 10, 10);

  AbstractArray * array = check_array(args[0]);
  Value dimensions = args[1];
  Value element_type = args[2];
  Value initial_element = args[3];
  Value initial_element_p = args[4];
  Value initial_contents = args[5];
  Value initial_contents_p = args[6];
  Value fill_pointer = args[7];
  Value displaced_to = args[8];
  Value displaced_index_offset = args[9];

  if (initial_element_p != NIL && initial_contents_p != NIL)
    return signal_lisp_error("ADJUST-ARRAY: cannot specify both initial element and initial contents.");

  // REVIEW the element type of multi-dimensional arrays is always T
  if (array->rank() <= 1)
    {
      if (element_type != array->element_type()
          && upgraded_array_element_type(element_type) != array->element_type())
        return signal_lisp_error("ADJUST-ARRAY: incompatible element type.");
    }

  if (array->rank() == 0)
    {
      if (initial_contents_p != NIL)
        array->aset(0, initial_contents);
      return make_value(array);
    }

  if (array->rank() == 1)
    {
      unsigned long new_size;
      if (consp(dimensions) && length(dimensions) == 1)
        new_size = check_index(xcar(dimensions));
      else
        new_size = check_index(dimensions);
      AbstractVector * v = reinterpret_cast<AbstractVector *>(array);
      AbstractVector * v2;
      if (displaced_to != NIL)
        {
          unsigned long offset;
          if (displaced_index_offset == NIL)
            offset = 0;
          else
            offset = check_index(displaced_index_offset);
          v2 = v->displace_vector(new_size, check_array(displaced_to), offset);
        }
      else
        {
          v2 = v->adjust_vector(new_size,
                                initial_element,
                                initial_contents);
        }
      if (fill_pointer != NIL)
        {
          if (fill_pointer == T)
            v2->set_length(v2->capacity());
          else
            v2->set_length(check_index(fill_pointer, 0, v2->capacity()));
        }
      return make_value(v2);
    }

  // rank > 1
//   if (array instanceof SimpleArray_T) {
//     SimpleArray_T a = (SimpleArray_T) array;
  const unsigned int rank = listp(dimensions) ? length(dimensions) : 1;
//   int[] dimv = new int[rank];
  unsigned long * dims =
    (unsigned long *) GC_malloc_atomic(rank * sizeof(unsigned long *));
  if (listp(dimensions))
    {
      for (unsigned long i = 0; i < rank; i++)
        {
          Value dim = car(dimensions);
          dims[i] = check_index(dim);
          dimensions = xcdr(dimensions);
        }
    }
  else
      dims[0] = check_index(dimensions);
  AbstractArray * a2;
  if (displaced_to != NIL)
    {
      unsigned int offset;
      if (displaced_index_offset == NIL)
        offset = 0;
      else
        offset = check_index(displaced_index_offset);
      a2 = array->displace_array(rank, dims,
                                 check_array(displaced_to),
                                 offset);
    }
  else
    {
      a2 = array->adjust_array(rank, dims, initial_element, initial_contents);
    }

//   }


//   return NIL;
  return make_value(a2);
}
