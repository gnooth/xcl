// AbstractVector.cpp
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

Value AbstractVector::class_of() const
{
  return C_vector;
}

unsigned int AbstractVector::rank() const
{
  return 1;
}

// "Returns a list of the dimensions of array. (If array is a vector with a
// fill pointer, that fill pointer is ignored.)"
Value AbstractVector::dimensions() const
{
  return make_cons(make_fixnum(_capacity));
}

INDEX AbstractVector::dimension(unsigned int n) const
{
  if (n == 0)
    return _capacity;

  signal_type_error(make_number(n), list3(S_integer, FIXNUM_ZERO, FIXNUM_ZERO));
  // Not reached.
  return 0;
}

// "If the array is a vector with a fill pointer, the fill pointer is ignored
// when calculating the array total size."
INDEX AbstractVector::total_size() const
{
  return _capacity;
}

bool AbstractVector::equalp(Value value) const
{
  if (vectorp(value))
    {
      AbstractVector * vector = the_vector(value);
      if (vector->length() == length())
        {
          for (INDEX i = length(); i-- > 0;)
            {
              Value v1 = vector->elt(i);
              Value v2 = elt(i);
              if (v1 == v2 || ::equalp(v1, v2))
                ; // OK
              else
                return false;
            }
          return true;
        }
    }
  return false;
}

AbstractString * AbstractVector::write_to_string()
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_readably) != NIL)
    {
      String * s = new String("#(");
      const INDEX len = length();
      for (INDEX i = 0; i < len; i++)
        {
          if (i > 0)
            s->append_char(' ');
          s->append(::write_to_string(aref(i)));
        }
      s->append_char(')');
      return s;
    }
  if (thread->symbol_value(S_print_array) != NIL)
    {
      INDEX max_level = MOST_POSITIVE_FIXNUM;
      Value print_level = thread->symbol_value(S_print_level);
//       printf("AbstractVector::write_to_string(): print_level = %s\n", ::write_to_string(print_level)->as_c_string());
      if (print_level != NIL)
        max_level = check_index(print_level);
      Value current_print_level = thread->symbol_value(S_current_print_level);
      INDEX current_level = check_index(current_print_level);
//       printf("AbstractVector::write_to_string(): current_level = %lu\n", current_level);
//       fflush(stdout);
      if (current_level >= max_level)
        return new_simple_string("#");
      String * s = new String("#(");
      INDEX max_length = MOST_POSITIVE_FIXNUM;
      Value print_length = thread->symbol_value(S_print_length);
      if (print_length != NIL)
        max_length = check_index(print_length);
      const INDEX len = length();
      const INDEX limit = (len < max_length) ? len : max_length;
      void * last_special_binding = thread->last_special_binding();
      thread->bind_special(S_current_print_level, make_number(current_level + 1));
      for (INDEX i = 0; i < limit; i++)
        {
          if (i > 0)
            s->append_char(' ');
          s->append(::write_to_string(aref(i)));
        }
      thread->set_last_special_binding(last_special_binding);
      if (limit < len)
        s->append(limit > 0 ? " ..." : "...");
      s->append_char(')');
      return s;
    }
  return unreadable_string();
}
