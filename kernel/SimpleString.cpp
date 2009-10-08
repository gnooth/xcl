// SimpleString.cpp
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

#include <ctype.h>      // tolower, toupper
#include "lisp.hpp"
#include "primitives.hpp"

SimpleString::SimpleString(INDEX capacity, AbstractString * s)
  : AbstractString(WIDETAG_SIMPLE_STRING)
{
  assert(s != NULL);
  _capacity = capacity;
  // REVIEW optimize
  BASE_CHAR * data = s->data();
  if (data)
    for (INDEX i = capacity; i-- > 0;)
      _chars[i] = data[i];
  else
    for (INDEX i = capacity; i-- > 0;)
      _chars[i] = s->fast_char_at(i);
  _chars[capacity] = 0;
}

SimpleString::SimpleString(INDEX capacity, const char * s)
  : AbstractString(WIDETAG_SIMPLE_STRING)
{
  _capacity = capacity;
  memcpy(_chars, s, capacity);
  _chars[capacity] = 0;
}

SimpleString::SimpleString(INDEX capacity)
  : AbstractString(WIDETAG_SIMPLE_STRING, capacity)
{
  memset(_chars, 0, _capacity + 1);
}

SimpleString::SimpleString(INDEX capacity, BASE_CHAR c)
  : AbstractString(WIDETAG_SIMPLE_STRING, capacity)
{
  memset(_chars, c, capacity);
  _chars[capacity] = 0;
}

SimpleString::SimpleString(INDEX capacity, BASE_CHAR * chars)
  : AbstractString(WIDETAG_SIMPLE_STRING, capacity, chars)
{
  for (INDEX i = _capacity; i-- > 0;)
    _chars[i] = chars[i];
  _chars[_capacity] = 0;
}

Value SimpleString::displacement() const
{
  return current_thread()->set_values(NIL, FIXNUM_ZERO);
}

SimpleString * SimpleString::substring(INDEX begin, INDEX end) const
{
  assert(end >= begin);
  assert(end <= _capacity);
  const INDEX len = end - begin;
  SimpleString * s = new_simple_string(len);
  INDEX j = 0;
  for (INDEX i = begin; i < end; i++, j++)
    s->_chars[j] = _chars[i];
  s->_capacity = len;
  return s;
}

bool SimpleString::typep(Value type) const
{
  if (classp(type))
    return (type == C_string || type == C_vector || type == C_array
            || type == C_sequence || type == C_t);
  if (symbolp(type))
    return (type == S_string || type == S_base_string || type == S_simple_string
            || type == S_simple_base_string || type == S_vector
            || type == S_simple_array || type == S_array || type == S_sequence
            || type == S_atom || type == T);
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
              if (element_type == UNSPECIFIED || element_type == S_character
                  || element_type == S_base_char)
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
               || type_specifier_atom == S_base_string
               || type_specifier_atom == S_simple_string
               || type_specifier_atom == S_simple_base_string)
        {
          Value size = car(tail);
          return (size == UNSPECIFIED || check_index(size) == _capacity);
        }
    }
  return false;
}

unsigned long SimpleString::hash()
{
  long hashcode = 0;
  for (INDEX i = 0; i < _capacity; i++)
    {
      hashcode += _chars[i];
      hashcode += (hashcode << 10);
      hashcode ^= (hashcode >> 6);
    }
  hashcode += (hashcode << 3);
  hashcode ^= (hashcode >> 11);
  hashcode += (hashcode << 15);
  return hashcode & MOST_POSITIVE_FIXNUM;
}

unsigned long SimpleString::equalp_hash()
{
  long hashcode = 0;
  for (INDEX i = 0; i < _capacity; i++)
    {
      hashcode += toupper(_chars[i]);
      hashcode += (hashcode << 10);
      hashcode ^= (hashcode >> 6);
    }
  hashcode += (hashcode << 3);
  hashcode ^= (hashcode >> 11);
  hashcode += (hashcode << 15);
  return hashcode & MOST_POSITIVE_FIXNUM;
}

bool SimpleString::equal(Value value) const
{
  if (!stringp(value))
    return false;
  AbstractString * s = the_string(value);
  if (this == s)
    return true;
  if (_capacity != s->length())
    return false;
  // REVIEW optimize
  for (INDEX i = 0; i < _capacity; i++)
    {
      if (_chars[i] != s->char_at(i))
        return false;
    }
  return true;
}

bool SimpleString::equal(AbstractString * s) const
{
  if (_capacity != s->length())
    return false;
  // REVIEW optimize
  BASE_CHAR * data = s->data();
  if (data)
    {
      for (INDEX i = _capacity; i-- > 0;)
        {
          if (_chars[i] != data[i])
            return false;
        }
    }
  else
    {
      for (INDEX i = _capacity; i-- > 0;)
        {
          if (_chars[i] != s->char_at(i))
            return false;
        }
    }
  return true;
}

bool SimpleString::equal(const char * s) const
{
  if (_capacity != (INDEX) strlen(s))
    return false;
  for (INDEX i = 0; i < _capacity; i++)
    {
      if (_chars[i] != s[i])
        return false;
    }
  return true;
}

bool SimpleString::equalp(AbstractString * s) const
{
  if (this == s)
    return true;
  if (_capacity != s->length())
    return false;
  // REVIEW optimize
  for (INDEX i = 0; i < _capacity; i++)
    {
      BASE_CHAR c1 = _chars[i];
      BASE_CHAR c2 = s->char_at(i);
      if (c1 == c2)
        continue;
      if (toupper(c1) == toupper(c2))
        continue;
      if (tolower(c1) == tolower(c2))
        continue;
      return false;
    }
  return true;
}

BASE_CHAR SimpleString::char_at(INDEX i) const
{
  if (i < _capacity)
    return _chars[i];
  signal_lisp_error("bad index");
  // not reached
  return 0;
}

void SimpleString::set_char_at(INDEX i, BASE_CHAR c)
{
  if (i < _capacity)
    _chars[i] = c;
  else
    signal_lisp_error("bad index");
}

long SimpleString::index_of(BASE_CHAR c) const
{
  for (INDEX i = 0; i < _capacity; i++)
    if (_chars[i] == c)
      return i;
  return -1;
}

long SimpleString::last_index_of(BASE_CHAR c) const
{
  for (INDEX i = _capacity; i-- > 0;)
    if (_chars[i] == c)
      return i;
  return -1;
}

void SimpleString::fill(Value value)
{
  BASE_CHAR c = char_value(value);
  for (INDEX i = 0; i < _capacity; i++)
    _chars[i] = c;
}

Value SimpleString::aref(unsigned long i) const
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  return make_character(_chars[i]);
}

Value SimpleString::aset(unsigned long i, Value new_value)
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  _chars[i] = char_value(new_value);
  return new_value;
}

Value SimpleString::elt(unsigned long i) const
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  return make_character(_chars[i]);
}

Value SimpleString::push(Value new_element)
{
  return no_fill_pointer();
}

Value SimpleString::push_extend(Value new_element, unsigned long extension)
{
  return no_fill_pointer();
}

Value SimpleString::pop()
{
  return no_fill_pointer();
}

const char * SimpleString::as_c_string() const
{
  return (const char *) _chars;
}

const char * SimpleString::copy_to_c_string() const
{
  char * s = (char *) GC_malloc_atomic(_capacity + 1);
  memcpy(s, _chars, _capacity);
  s[_capacity] = 0;
  return s;
}

AbstractString * SimpleString::downcase() const
{
  SimpleString * s = new_simple_string(_capacity);
  for (INDEX i = _capacity; i-- > 0;)
    s->_chars[i] = tolower(_chars[i]);
  return s;
}

AbstractString * SimpleString::upcase() const
{
  SimpleString * s = new_simple_string(_capacity);
  for (INDEX i = _capacity; i-- > 0;)
    s->_chars[i] = toupper(_chars[i]);
  return s;
}

SimpleString * SimpleString::ndowncase()
{
  for (INDEX i = _capacity; i-- > 0;)
    _chars[i] = tolower(_chars[i]);
  return this;
}

SimpleString * SimpleString::nupcase()
{
  for (INDEX i = _capacity; i-- > 0;)
    _chars[i] = toupper(_chars[i]);
  return this;
}

Value SimpleString::reverse() const
{
  SimpleString * s = new_simple_string(_capacity);
  if (_capacity > 0)
    {
      INDEX i, j;
      for (i = 0, j = _capacity - 1; i < _capacity; i++, j--)
        s->_chars[i] = _chars[j];
    }
  return make_value(s);
}

Value SimpleString::nreverse()
{
  if (_capacity > 0)
    {
      INDEX i = 0;
      INDEX j = _capacity - 1;
      while (i < j)
        {
          BASE_CHAR temp = _chars[i];
          _chars[i] = _chars[j];
          _chars[j] = temp;
          ++i;
          --j;
        }
    }
  return make_value(this);
}

Value SimpleString::subseq(unsigned long start, unsigned long end) const
{
  assert(end >= start);
  unsigned long capacity = end - start;
  SimpleString * s = new_simple_string(capacity);
  unsigned long i = start, j = 0;
  while (i < end)
    s->_chars[j++] = _chars[i++];
  return make_value(s);
}

AbstractVector * SimpleString::adjust_vector(INDEX new_capacity,
                                             Value initial_element,
                                             Value initial_contents)
{
  if (initial_contents != NIL)
    {
      BASE_CHAR * new_chars = (BASE_CHAR *) GC_malloc_atomic(new_capacity + 1);
      if (listp(initial_contents))
        {
          Value list = initial_contents;
          for (unsigned long i = 0; i < new_capacity; i++)
            {
              new_chars[i] = char_value(car(list));
              list = xcdr(list);
            }
        }
      else if (vectorp(initial_contents))
        {
          AbstractVector * v = the_vector(initial_contents);
          for (unsigned long i = 0; i < new_capacity; i++)
            new_chars[i] = char_value(v->aref(i));
        }
      else
        signal_type_error(initial_contents, S_sequence);
      new_chars[new_capacity] = 0;
      return new_simple_string(new_capacity, new_chars);
    }
  if (_capacity != new_capacity)
    {
      BASE_CHAR * new_chars = (BASE_CHAR *) GC_malloc_atomic(new_capacity + 1);
      unsigned long limit = (_capacity < new_capacity) ? _capacity : new_capacity;
      for (unsigned long i = 0; i < limit; i++)
        new_chars[i] = _chars[i];
      if (_capacity < new_capacity)
        {
          BASE_CHAR c = char_value(initial_element);
          for (unsigned long i = _capacity; i < new_capacity; i++)
            new_chars[i] = c;
        }
      new_chars[new_capacity] = 0;
      return new_simple_string(new_capacity, new_chars);
    }
  // No change.
  return this;
}

AbstractVector * SimpleString::displace_vector(INDEX new_capacity,
                                               AbstractArray * displaced_to,
                                               INDEX offset)
{
  return new String(new_capacity, displaced_to, offset, NIL);
}

AbstractString * SimpleString::write_to_string()
{
  Thread * thread = current_thread();
  bool escape =
    (thread->symbol_value(S_print_escape) != NIL || thread->symbol_value(S_print_readably) != NIL);
  String * s = new String(_capacity + 2);
  if (escape)
    s->append_char('"');
  BASE_CHAR * chars = _chars;
  const INDEX limit = _capacity;
  for (INDEX i = 0; i < limit; i++)
    {
      BASE_CHAR c = chars[i];
      if (escape)
        {
          if (c == '"' || c == '\\')
            s->append_char('\\');
        }
      s->append_char(c);
    }
  if (escape)
    s->append_char('"');
  return s;
}

long simple_string_data_offset()
{
//   SimpleString s(1);
//   return s.data_offset();
  SimpleString * string = new_simple_string(1);
  return string->data_offset();
}

// ### schar string index => character
Value CL_schar(Value string, Value index)
{
  return make_character(check_simple_string(string)->xchar_at(check_index(index)));
}

// ### %schar string index => character
Value SYS_xschar(Value string, Value index)
{
  return make_character(the_simple_string(string)->xchar_at(check_index(index)));
}

// ### set-schar string index character => character
Value SYS_set_schar(Value string, Value index, Value character)
{
  check_simple_string(string)->set_char_at(check_index(index), char_value(character));
  return character;
}

// ### simple-string-p
Value CL_simple_string_p(Value arg)
{
  if (typed_object_p(arg))
    {
      switch (the_typed_object(arg)->widetag())
        {
        case WIDETAG_SIMPLE_STRING:
          return T;
        case WIDETAG_NIL_VECTOR:
          return the_vector(arg)->has_fill_pointer() ? NIL : T;
        }
    }
  return NIL;
}
