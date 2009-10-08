// NilVector.cpp
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
#include "NilVector.hpp"

NilVector::NilVector(unsigned long capacity, Value fill_pointer)
  : AbstractString(WIDETAG_NIL_VECTOR, capacity)
{
  if (fill_pointer == NIL)
    {
      _has_fill_pointer = false;
      _length = capacity;
    }
  else
    {
      _has_fill_pointer = true;
      _length = (fill_pointer == T) ? capacity : check_index(fill_pointer, 0, capacity);
    }
}

BASE_CHAR * NilVector::data()
{
  access_error();
  // not reached
  return NULL;
}

Value NilVector::type_of() const
{
  if (has_fill_pointer())
    return list3(S_vector, NIL, make_fixnum(_capacity));
  else
    return list3(S_simple_array, NIL, list1(make_fixnum(_capacity)));
}

Value NilVector::class_of() const
{
  return C_nil_vector;
}

bool NilVector::typep(Value type) const
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
              if (element_type == UNSPECIFIED || element_type == NIL)
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
                      if (consp(dimensions))
                        {
                          if (::length(dimensions) == 1)
                            {
                              Value dim = xcar(dimensions);
                              if (dim == UNSPECIFIED || dim == make_fixnum(_capacity))
                                return true;
                            }
                        }
                    }
                }
            }
        }
      else if (type_specifier_atom == S_string
               || type_specifier_atom == S_simple_string)
        {
          Value size = car(tail);
          return (size == UNSPECIFIED || check_index(size) == _capacity);
        }
    }
  else if (symbolp(type))
    {
      if (type == S_nil_vector || type == S_string || type == S_simple_string
          || type == S_vector || type == S_simple_array || type == S_array
          || type == S_sequence || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_nil_vector || type == C_string || type == C_vector
          || type == C_array || type == C_sequence || type == C_t)
        return true;
    }
  return false;
}

Value NilVector::element_type() const
{
  return NIL;
}

void NilVector::set_length(unsigned long length)
{
  assert(length <= _capacity);
  _length = length;
}

void NilVector::fill(Value value)
{
  access_error();
}

Value NilVector::elt(unsigned long i) const
{
  return access_error();
}

Value NilVector::push(Value new_element)
{
  return access_error();
}

Value NilVector::push_extend(Value new_element, unsigned long extension)
{
  return access_error();
}

Value NilVector::pop()
{
  return access_error();
}

Value NilVector::reverse() const
{
  return access_error();
}

Value NilVector::nreverse()
{
  return access_error();
}

Value NilVector::subseq(unsigned long start, unsigned long end) const
{
  return access_error();
}

Value NilVector::aref(unsigned long i) const
{
  return access_error();
}

Value NilVector::aset(unsigned long i, Value new_value)
{
  return access_error();
}

BASE_CHAR NilVector::char_at(unsigned long i) const
{
  access_error();
  // not reached
  return 0;
}

void NilVector::set_char_at(unsigned long i, BASE_CHAR c)
{
  access_error();
}

BASE_CHAR NilVector::fast_char_at(unsigned long i) const
{
  access_error();
  // not reached
  return 0;
}

void NilVector::fast_set_char_at(unsigned long i, BASE_CHAR c)
{
  access_error();
}

long NilVector::index_of(BASE_CHAR c) const
{
  access_error();
  // not reached
  return -1;
}

long NilVector::last_index_of(BASE_CHAR c) const
{
  access_error();
  // not reached
  return -1;
}

SimpleString * NilVector::substring(unsigned long begin, unsigned long end) const
{
  access_error();
  // not reached
  return NULL;
}

SimpleString * NilVector::substring(unsigned long begin) const
{
  access_error();
  // not reached
  return NULL;
}

bool NilVector::equal(Value value) const
{
  if (!stringp(value))
    return false;
  AbstractString * s = the_string(value);
  if (_capacity != s->length())
    return false;
  if (_capacity != 0)
    {
      access_error();
      // not reached
      return false;
    }
  return true;
}

bool NilVector::equal(AbstractString * s) const
{
  if (_capacity != s->length())
    return false;
  if (_capacity != 0)
    {
      access_error();
      // not reached
      return false;
    }
  return true;
}

bool NilVector::equal(const char * s) const
{
  if (_capacity != strlen(s))
    return false;
  if (_capacity != 0)
    {
      access_error();
      // not reached
      return false;
    }
  return true;
}

bool NilVector::equalp(AbstractString * s) const
{
  if (_capacity != s->length())
    return false;
  if (_capacity != 0)
    {
      access_error();
      // not reached
      return false;
    }
  return true;
}

const char * NilVector::as_c_string() const
{
  access_error();
  // not reached
  return NULL;
}

const char * NilVector::copy_to_c_string() const
{
  access_error();
  // not reached
  return NULL;
}

AbstractString * NilVector::downcase() const
{
  if (_capacity == 0)
    return new NilVector(0, _has_fill_pointer ? T : NIL);
  access_error();
  // not reached
  return NULL;
}

AbstractString * NilVector::upcase() const
{
  if (_capacity == 0)
    return new NilVector(0, _has_fill_pointer ? T : NIL);
  access_error();
  // not reached
  return NULL;
}

AbstractString * NilVector::write_to_string()
{
  if (_capacity == 0)
    {
      Thread * thread = current_thread();
      if (thread->symbol_value(S_print_escape) != NIL || thread->symbol_value(S_print_readably) != NIL)
        return new String("\"\"");
      else
        return new String("");
    }
  access_error();
  // not reached
  return NULL;
}
