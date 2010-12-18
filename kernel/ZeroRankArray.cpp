// ZeroRankArray.cpp
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
#include "ZeroRankArray.hpp"
#include "PrintNotReadable.hpp"
#include "StringOutputStream.hpp"

Value ZeroRankArray::type_of() const
{
  return list3(S_simple_array, _element_type, NIL);
}

bool ZeroRankArray::typep(Value type) const
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
                  || upgraded_array_element_type(element_type) == _element_type)
                {
                  if (tail == NIL)
                    return true;
                  if (cdr(tail) == NIL) // i.e. length(tail) == 1
                    {
                      Value dimensions = xcar(tail);
                      if (dimensions == UNSPECIFIED)
                        return true;
                      if (dimensions == FIXNUM_ZERO)
                        return true;
                      if (dimensions == NIL)
                        return true;
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

INDEX ZeroRankArray::dimension(unsigned int n) const
{
  signal_type_error(make_unsigned_integer(n), NIL);
  // not reached
  return 0;
}

Value ZeroRankArray::aref(unsigned long i) const
{
  if (i == 0)
    {
      if (_array)
        return _array->aref(_offset);
      else
        return _data;
    }
  return signal_type_error(make_unsigned_integer(i),
                           list3(S_integer, FIXNUM_ZERO, FIXNUM_ZERO));
}

Value ZeroRankArray::aset(unsigned long i, Value new_value)
{
  if (i == 0)
    {
      if (_array)
        _array->aset(_offset, new_value);
      else
        _data = new_value;
      return new_value;
    }
  return signal_type_error(make_unsigned_integer(i),
                           list3(S_integer, FIXNUM_ZERO, FIXNUM_ZERO));
}

AbstractString * ZeroRankArray::write_to_string()
{
  Thread * thread = current_thread();
  bool print_readably = (thread->symbol_value(S_print_readably) != NIL);
  if (print_readably)
    {
      if (_element_type != T)
        signal_lisp_error(new PrintNotReadable(make_value(this)));
    }
  if (print_readably || thread->symbol_value(S_print_array) != NIL)
    {
      String * s = new String("#0A");
      if (aref(0) == make_value(this) && thread->symbol_value(S_print_circle) != NIL)
        {
          StringOutputStream * stream = new StringOutputStream(S_character);
          thread->execute(the_symbol(S_output_object)->function(), aref(0), make_value(stream));
          s->append(stream->get_string());
        }
      else
        s->append(::write_to_string(aref(0)));
      return s;
    }
  else
    return unreadable_string();
}
