// arrays.cpp
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

#if defined(WIN32)
#include <malloc.h>
#elif defined(__NetBSD__) || defined(__FreeBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

#include "lisp.hpp"
#include "primitives.hpp"
#include "runtime.h"
#include "ZeroRankArray.hpp"
#include "ProgramError.hpp"

// ### arrayp object => boolean
Value CL_arrayp(Value arg)
{
  return arrayp(arg) ? T : NIL;
}

// ### simple-array-p object => boolean
Value SYS_simple_array_p(Value arg)
{
  if (typed_object_p(arg))
    {
      if (the_typed_object(arg)->widetag() & WIDETAG_SIMPLE_ARRAY_BIT)
        return T;
    }
  return NIL;
}

// ### array-has-fill-pointer-p array => boolean
Value CL_array_has_fill_pointer_p(Value arg)
{
  if (vectorp(arg))
    return the_vector(arg)->has_fill_pointer() ? T : NIL;
  if (arrayp(arg))
    return NIL;
  return signal_type_error(arg, S_array);
}

// ### array-rank array => rank
Value CL_array_rank(Value arg)
{
  return make_fixnum(check_array(arg)->rank());
}

// ### array-displacement array => displaced-to, displaced-index-offset
Value CL_array_displacement(Value arg)
{
  return check_array(arg)->displacement();
}

// ### adjustable-array-p array => generalized-boolean
Value CL_adjustable_array_p(Value arg)
{
  return check_array(arg)->is_adjustable() ? T : NIL;
}

// ### array-dimension array axis-number => dimension
Value CL_array_dimension(Value arg1, Value arg2)
{
  return make_number(check_array(arg1)->dimension(check_index(arg2)));
}

// ### array-dimensions array => dimensions
Value CL_array_dimensions(Value arg)
{
  return check_array(arg)->dimensions();
}

// ### array-total-size array => size
Value CL_array_total_size(Value arg)
{
  // REVIEW make_fixnum
  return make_number(check_array(arg)->total_size());
}

// ### array-row-major-index array &rest subscripts => index
Value CL_array_row_major_index(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_array_row_major_index, numargs, 1, MANY);
  AbstractArray * array = check_array(args[0]);
  int nsubs = numargs - 1;
  unsigned long * subscripts =
    (unsigned long *) GC_malloc_atomic(nsubs * sizeof(unsigned long));
  for (unsigned long i = nsubs; i-- > 0;)
    subscripts[i] = fixnum_value(args[i + 1]);
  return make_number(array->row_major_index(nsubs, subscripts));
}

// ### array-in-bounds-p array &rest subscripts => generalized-boolean
Value CL_array_in_bounds_p(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_array_in_bounds_p, numargs, 1, MANY);
  AbstractArray * array = check_array(args[0]);
  unsigned int rank = array->rank();
  if (rank != (unsigned int) numargs - 1)
    {
      String * s = new String("ARRAY-IN-BOUNDS-P: ");
      s->append("wrong number of subscripts (");
      s->append_unsigned_long(numargs - 1);
      s->append(") for array of rank ");
      s->append_unsigned_long(rank);
      s->append_char('.');
      return signal_lisp_error(new ProgramError(s));
    }
  for (unsigned long i = 0; i < rank; i++)
    {
      Value arg = args[i + 1];
      if (fixnump(arg))
        {
          long subscript = xlong(arg);
          if (subscript < 0 || ((unsigned long)subscript) >= array->dimension(i))
            return NIL;
        }
      else if (bignump(arg))
        return NIL;
      else
        return signal_type_error(arg, S_integer);
    }
  return T;
}

// ### array-element-type array => typespec
Value CL_array_element_type(Value arg)
{
  return check_array(arg)->element_type();
}

static Value upgrade_integer_type(Value type)
{
  // See if one of the UNSIGNED-BYTE types will work.
  Value lower = CL_second(type);
  Value upper = CL_third(type);
  // Convert to inclusive bounds.
  long l = MOST_NEGATIVE_FIXNUM;
  long u = MOST_POSITIVE_FIXNUM;
  if (consp(lower))
    {
      lower = xcar(lower);
      if (fixnump(lower))
        {
          l = xlong(lower);
          if (l < MOST_POSITIVE_FIXNUM)
            ++l;
          else
            return T;
        }
    }
  else if (fixnump(lower))
    l = xlong(lower);
  else
    return T;
  if (consp(upper))
    {
      upper = xcar(upper);
      if (fixnump(upper))
        {
          u = xlong(upper);
          if (u > MOST_NEGATIVE_FIXNUM)
            --u;
          else
            return T;
        }
    }
  else if (fixnump(upper))
    u = xlong(upper);
  else
    return T;
  if (l >= 0)
    {
      if (u <= 1)
        return S_bit;
      if (u <= 255)
        return UB8_TYPE;
      if (u <= 65535)
        return UB16_TYPE;
#ifdef __x86_64__
      if (u <= 4294967295)
        return UB32_TYPE;
#endif
    }
  return T;
}

Value upgraded_array_element_type(Value type)
{
  if (symbolp(type))
    {
      if (type == S_character || type == S_base_char || type == S_standard_char)
        return S_character;
      if (type == S_bit)
        return S_bit;
      if (type == NIL)
        return NIL;
    }
  if (consp(type))
    {
      if (xcar(type) == S_unsigned_byte)
        {
          Value cadr = CL_cadr(type);
          if (fixnump(cadr))
            {
              long size = xlong(cadr);
              if (size == 1)
                return S_bit;
              if (size <= 8)
                return UB8_TYPE;
              if (size <= 16)
                return UB16_TYPE;
              if (size <= 32)
                return UB32_TYPE;
            }
        }
      if (xcar(type) == S_integer)
        {
#ifdef __x86_64__
          return upgrade_integer_type(type);
#else
          // 32-bit Lisp
          Value upgraded_type = upgrade_integer_type(type);
          if (upgraded_type != T)
            return upgraded_type;
          // REVIEW
          // It's not easy to do this in C on a 32-bit platform.
          if (CL_fboundp(S_subtypep) != NIL)
            {
              Thread * thread = current_thread();
              Value ok = RT_thread_call_symbol_2(thread, S_subtypep, type, UB32_TYPE);
              thread->clear_values();
              if (ok != NIL)
                return UB32_TYPE;
            }
          // fall through...
#endif
        }
    }
  return T;
}

// ### %upgraded-array-element-type typespec => upgraded-typespec
Value SYS_upgraded_array_element_type_internal(Value arg)
{
  return upgraded_array_element_type(arg);
}

// ### fill-pointer
Value CL_fill_pointer(Value arg)
{
  if (vectorp(arg))
    {
      AbstractVector * v = the_vector(arg);
      if (v->has_fill_pointer())
        return make_fixnum(v->length());
    }
  return no_fill_pointer(arg);
}

Value SYS_set_fill_pointer(Value arg1, Value arg2)
{
  if (vectorp(arg1))
    {
      AbstractVector * v = the_vector(arg1);
      if (v->has_fill_pointer())
        {
          v->set_length(check_index(arg2, 0, v->capacity()));
          return arg2;
        }
    }
  return no_fill_pointer(arg1);
}

// ### row-major-aref array index => element
Value CL_row_major_aref(Value array, Value index)
{
  return check_array(array)->aref(check_index(index));
}

// ### row-major-aset array index new-element => new-element
Value SYS_row_major_aset(Value array, Value index, Value new_element)
{
  return check_array(array)->aset(check_index(index), new_element);
}

// ### vector-ref vector index => element
Value SYS_vector_ref(Value vector, Value index)
{
  return check_vector(vector)->aref(check_index(index));
}

// ### %vector-ref vector index => element
Value SYS_xvector_ref(Value vector, Value index)
{
  return the_vector(vector)->aref(check_index(index));
}

// ### vector-set vector index new-element => new-element
Value SYS_vector_set(Value vector, Value index, Value new_element)
{
  return check_vector(vector)->aset(check_index(index), new_element);
}

// ### %vector-set vector index new-element => new-element
Value SYS_xvector_set(Value vector, Value index, Value new_element)
{
  return the_vector(vector)->aset(check_index(index), new_element);
}

// ### aref array &rest subscripts => element
Value CL_aref(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_aref, numargs, 1, MANY);
    case 1:
      {
        AbstractArray * array = check_array(args[0]);
        if (array->rank() == 0)
          return reinterpret_cast<ZeroRankArray *>(array)->aref(0);
        String * message = new String("Wrong number of subscripts (0) for array of rank ");
        message->append_unsigned_long(array->rank());
        message->append_char('.');
        return signal_lisp_error(new ProgramError(message));
      }
    case 2:
      return check_vector(args[0])->aref(check_index(args[1]));
    default:
      {
        AbstractArray * array = check_array(args[0]);
        int nsubs = numargs - 1;
        unsigned long * subscripts =
          (unsigned long *) alloca(nsubs * sizeof(unsigned long));
        for (int i = nsubs; i-- > 0;)
          subscripts[i] = fixnum_value(args[i + 1]);
        return array->aref(array->row_major_index(nsubs, subscripts));
      }
    }
}

// ### aset array subscripts new-element => new-element
Value SYS_aset(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
    case 1:
      return wrong_number_of_arguments(S_aset, numargs, 2, MANY);
    case 2:
      {
        AbstractArray * array = check_array(args[0]);
        if (array->rank() == 0)
          return reinterpret_cast<ZeroRankArray *>(array)->aset(0, args[1]);
        String * message = new String("Wrong number of subscripts (0) for array of rank ");
        message->append_unsigned_long(array->rank());
        message->append_char('.');
        return signal_lisp_error(new ProgramError(message));
      }
    case 3:
      return check_vector(args[0])->aset(check_index(args[1]), args[2]);
    default:
      {
        AbstractArray * array = check_array(args[0]);
        int nsubs = numargs - 2;
        unsigned long * subscripts =
          (unsigned long *) alloca(nsubs * sizeof(unsigned long));
        for (int i = nsubs; i-- > 0;)
          subscripts[i] = fixnum_value(args[i + 1]);
        return array->aset(array->row_major_index(nsubs, subscripts),
                           args[numargs - 1]);
      }
    }
}
