// AbstractArray.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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
#include "PrintNotReadable.hpp"
#include "ProgramError.hpp"
#include "StringOutputStream.hpp"

Value AbstractArray::class_of() const
{
  return C_array;
}

bool AbstractArray::is_displaced() const
{
  return false;
}

Value AbstractArray::displacement() const
{
  return current_thread()->set_values(NIL, FIXNUM_ZERO);
}

INDEX AbstractArray::row_major_index(unsigned int nsubs, INDEX subscripts[])
{
  if (nsubs != rank())
    {
      String * message = new String("Wrong number of subscripts (");
      message->append_unsigned_long(nsubs);
      message->append(") for array of rank ");
      message->append_unsigned_long(rank());
      message->append_char('.');
      signal_lisp_error(new ProgramError(message));
      // Not reached.
      return 0;
    }
  unsigned long sum = 0;
  unsigned long size = 1;
  for (unsigned long i = rank(); i-- > 0;)
    {
      unsigned long dim = dimension(i);
      unsigned long last_size = size;
      size *= dim;
      unsigned long n = subscripts[i];
      if (n >= dim)
        {
          String * message = new String("Invalid index ");
          message->append_unsigned_long(n);
          message->append(" for array ");
          message->append(prin1_to_string());
          message->append_char('.');
          signal_lisp_error(new ProgramError(message));
          // Not reached.
          return 0;
        }
      sum += n * last_size;
    }
  return sum;
}

bool AbstractArray::equalp(Value value) const
{
    if (arrayp(value))
      {
        AbstractArray * array = the_array(value);
        if (rank() != array->rank())
          return false;
        for (unsigned long i = rank(); i-- > 0;)
          {
            if (dimension(i) != array->dimension(i))
              return false;
          }
        for (unsigned long i = total_size(); i--> 0;)
          {
            if (!::equalp(aref(i), array->aref(i)))
              return false;
          }
        return true;
      }
    return false;
}

// FIXME Detect overflow!
unsigned long AbstractArray::compute_total_size(unsigned long rank,
                                                unsigned long dimensions[])
{
  unsigned long size = 1;
  for (long i = rank; i-- > 0;)
    size *= dimensions[i];
  return size;
}

// Copy a1 to a2 for index tuples that are valid for both arrays.
void AbstractArray::copy_array(AbstractArray * a1, AbstractArray * a2)
{
  assert(a1->rank() == a2->rank());
  const unsigned long rank = a1->rank();
  unsigned long * subscripts =
    (unsigned long *) GC_malloc_atomic(a1->rank() * sizeof(unsigned long *));
  for (unsigned int i = rank; i-- > 0;)
    subscripts[i] = 0;
  int axis = 0;
  copy_array_1(a1, a2, subscripts, axis);
}

void AbstractArray::copy_array_1(AbstractArray * a1, AbstractArray * a2,
                                 INDEX subscripts[], int axis)
{
  const int rank = a1->rank();
  if (axis < rank)
    {
      const unsigned long limit =
        a1->dimension(axis) < a2->dimension(axis) ? a1->dimension(axis) : a2->dimension(axis);
      for (unsigned long i = 0; i < limit; i++) {
        subscripts[axis] = i;
        copy_array_1(a1, a2, subscripts, axis + 1);
      }
    }
  else
    {
      INDEX i1 = a1->row_major_index(rank, subscripts);
      INDEX i2 = a2->row_major_index(rank, subscripts);
      a2->aset(i2, a1->aref(i1));
    }
}

AbstractString * AbstractArray::write_to_string_internal(int ndims, INDEX dimv[])
{
  Thread * thread = current_thread();
  bool print_readably = (thread->symbol_value(S_print_readably) != NIL);
  if (print_readably && thread->symbol_value(S_read_eval) == NIL)
    {
      // "If *READ-EVAL* is false and *PRINT-READABLY* is true, any method for
      // PRINT-OBJECT that would output a reference to the #. reader macro
      // either outputs something different or signals an error of type PRINT-
      // NOT-READABLE."
        if (element_type() != T)
          signal_lisp_error(new PrintNotReadable(make_value(this)));
    }
  if (print_readably || thread->symbol_value(S_print_array) != NIL)
    {
      long max_level = MOST_POSITIVE_FIXNUM;
      void * last_special_binding = NULL;
      if (print_readably)
        {
          for (int i = 0; i < ndims - 1; i++)
            {
              if (dimv[i] == 0)
                {
                  for (int j = i + 1; j < ndims; j++)
                    {
                      if (dimv[j] != 0)
                        {
                          signal_lisp_error(new PrintNotReadable(make_value(this)));
                          return NULL; // not reached
                        }
                    }
                }
            }
          last_special_binding = thread->last_special_binding();
          thread->bind_special(S_print_length, NIL);
          thread->bind_special(S_print_level, NIL);
          thread->bind_special(S_print_lines, NIL);
        }
      else
        {
          Value print_level = thread->symbol_value(S_print_level);
          if (print_level != NIL)
            max_level = fixnum_value(print_level);
        }
      long current_level = fixnum_value(thread->symbol_value(S_current_print_level));
      String * s = new String();
      if (print_readably && element_type() != T)
        {
          s->append("#.(CL:MAKE-ARRAY '");
          s->append(::write_to_string(dimensions()));
          s->append(" :ELEMENT-TYPE '");
          s->append(::write_to_string(element_type()));
          s->append(" :INITIAL-CONTENTS '");
          append_contents(ndims, dimv, 0, s, thread);
          s->append_char(')');
        }
      else
        {
          s->append_char('#');
          if (current_level < max_level)
            {
              s->append_long(ndims);
              s->append_char('A');
              append_contents(ndims, dimv, 0, s, thread);
            }
        }
      if (print_readably)
        thread->set_last_special_binding(last_special_binding);
      return s;
    }
  return unreadable_string();
}

// helper for write_to_string_internal()
void AbstractArray::append_contents(int ndims, INDEX dimensions[],
                                    int index, String * s, Thread * thread)
{
  if (ndims == 0)
    {
      if (thread->symbol_value(S_print_circle) != NIL)
        {
          StringOutputStream * stream = new StringOutputStream(S_character);
          thread->execute(the_symbol(S_output_object)->function(),
                          aref(index),
                          make_value(stream));
          s->append(stream->get_string());
        }
      else
        s->append(::write_to_string(aref(index)));
    }
  else
    {
      INDEX max_length = MOST_POSITIVE_FIXNUM;
      INDEX max_level = MOST_POSITIVE_FIXNUM;
      if (thread->symbol_value(S_print_readably) == NIL)
        {
          Value print_length = thread->symbol_value(S_print_length);
          if (print_length != NIL)
            max_length = check_index(print_length);
          Value print_level = thread->symbol_value(S_print_level);
          if (print_level != NIL)
            max_level = check_index(print_level);
        }
      Value current_print_level = thread->symbol_value(S_current_print_level);
      INDEX current_level = check_index(current_print_level);
      if (current_level < max_level)
        {
          void * last_special_binding = thread->last_special_binding();
          thread->bind_special(S_current_print_level, make_unsigned_integer(current_level + 1));
          s->append_char('(');
          INDEX * dims = (INDEX *) GC_malloc_atomic((ndims - 1) * sizeof(INDEX));
          for (int i = 1; i < ndims; i++)
            dims[i - 1] = dimensions[i];
          int count = 1;
          for (int i = 0; i < ndims - 1; i++)
            count *= dims[i];
          INDEX length = dimensions[0];
          INDEX limit = (length < max_length) ? length : max_length;
          for (INDEX i = 0; i < limit; i++)
            {
              append_contents(ndims - 1, dims, index, s, thread);
              if (i < limit - 1 || limit < length)
                s->append_char(' ');
              index += count;
            }
          if (limit < length)
            s->append("...");
          s->append_char(')');
          thread->set_last_special_binding(last_special_binding);
        }
      else
        s->append_char('#');
    }
}
