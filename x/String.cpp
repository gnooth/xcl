// String.cpp
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

#include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
#include "primitives.hpp"

#define MIN_CAPACITY    128

String::String()
  : AbstractString(WIDETAG_STRING, MIN_CAPACITY), _fill_pointer(0), _array(NULL), _offset(0)
{
  _has_fill_pointer = true;
  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  memset(_chars, 0, _capacity + 1);
}

String::String(const AbstractString * const s)
  : AbstractString(WIDETAG_STRING), _array(NULL), _offset(0)
{
  _has_fill_pointer = true;
  assert(s != NULL);
  _capacity = _fill_pointer = s->length();
  if (_capacity < MIN_CAPACITY)
    _capacity = MIN_CAPACITY;
  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  for (INDEX i = 0; i < _fill_pointer; i++)
    _chars[i] = s->fast_char_at(i);
  if (_fill_pointer < _capacity)
    memset(_chars + _fill_pointer, 0, _capacity + 1 - _fill_pointer);
  else
    _chars[_fill_pointer] = 0;
}

String::String(const char * s)
  : AbstractString(WIDETAG_STRING), _array(NULL), _offset(0)
{
  _has_fill_pointer = true;
  _capacity = _fill_pointer = strlen(s);
  if (_capacity < MIN_CAPACITY)
    _capacity = MIN_CAPACITY;
  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  memcpy(_chars, s, _fill_pointer);
  if (_fill_pointer < _capacity)
    memset(_chars + _fill_pointer, 0, _capacity + 1 - _fill_pointer);
  else
    _chars[_fill_pointer] = 0;
}

String::String(const AbstractString * const s, bool has_fill_pointer)
  : AbstractString(WIDETAG_STRING), _array(NULL), _offset(0)
{
  _has_fill_pointer = has_fill_pointer;
  assert(s != NULL);
  _capacity = s->length();
  _fill_pointer = has_fill_pointer ? _capacity : 0;
  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  for (INDEX i = 0; i < _capacity; i++)
    _chars[i] = s->fast_char_at(i);
  _chars[_capacity] = 0;
}

String::String(const char * s, bool has_fill_pointer)
  : AbstractString(WIDETAG_STRING), _array(NULL), _offset(0)
{
  _has_fill_pointer = has_fill_pointer;
  _capacity = strlen(s);
  _fill_pointer = has_fill_pointer ? _capacity : 0;
  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  memcpy(_chars, s, _capacity);
  _chars[_capacity] = 0;
}

String::String(INDEX capacity)
  : AbstractString(WIDETAG_STRING, capacity), _fill_pointer(0), _array(NULL), _offset(0)
{
  _has_fill_pointer = true;
  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  memset(_chars, 0, _capacity + 1);
}

String::String(INDEX capacity, Value fill_pointer)
  : AbstractString(WIDETAG_STRING, capacity), _array(NULL), _offset(0)
{
  if (fill_pointer == NIL)
    {
      _has_fill_pointer = false;
      _fill_pointer = 0;
    }
  else
    {
      _has_fill_pointer = true;
      _fill_pointer = (fill_pointer == T) ? capacity : check_index(fill_pointer, 0, capacity);
    }

  _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  memset(_chars, 0, _capacity + 1);
}

String::String(INDEX capacity, INDEX length, BASE_CHAR c)
  : AbstractString(WIDETAG_STRING, capacity), _fill_pointer(0), _array(NULL), _offset(0)
{
  _has_fill_pointer = false;
  _chars = (BASE_CHAR*) GC_malloc_atomic_ignore_off_page(_capacity + 1);
  memset(_chars, c, _capacity + 1);
}

// displaced strings
String::String(INDEX capacity, AbstractArray * array, INDEX offset, Value fill_pointer)
  : AbstractString(WIDETAG_STRING, capacity), _array(array), _offset(offset)
{
  if (fill_pointer == NIL)
    {
      _has_fill_pointer = false;
      _fill_pointer = 0;
    }
  else
    {
      _has_fill_pointer = true;
      _fill_pointer = (fill_pointer == T) ? capacity : check_index(fill_pointer, 0, capacity);
    }
  _chars = NULL;
}

// REVIEW move to AbstractArray
bool String::is_displaced() const
{
  return _array != NULL;
}

// REVIEW move to AbstractArray
Value String::displacement() const
{
  return current_thread()->set_values(_array != NULL ? make_value(_array) : NIL,
                                      make_fixnum(_offset));
}

SimpleString * String::substring(INDEX begin, INDEX end) const
{
  assert(end >= begin);
  assert(end <= length());
  SimpleString * s = new_simple_string(end - begin);
  BASE_CHAR * data = s->data();
  assert(data != NULL);
  INDEX j = 0;
  // REVIEW optimize
  if (_chars)
    {
      for (INDEX i = begin; i < end; i++, j++)
        data[j] = _chars[i];
    }
  else
    {
      // displaced
      for (INDEX i = begin; i < end; i++, j++)
        data[j] = fast_char_at(i);
    }
  return s;
}

Value String::type_of() const
{
  return S_string;
}

bool String::typep(Value type) const
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
      else if (type_specifier_atom == S_string || type_specifier_atom == S_base_string)
        {
          Value size = car(tail);
          return (size == UNSPECIFIED || check_index(size) == _capacity);
        }
    }
  else if (symbolp(type))
    {
      if (type == S_string || type == S_base_string || type == S_vector
          || type == S_array || type == S_sequence || type == S_atom || type == T)
        return true;
    }
  else
    {
      if (type == C_string || type == C_vector || type == C_array
          || type == C_sequence || type == C_t)
        return true;
    }
  return false;
}

unsigned long String::hash()
{
  long hashcode = 0;
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        {
          hashcode += _chars[i];
          hashcode += (hashcode << 10);
          hashcode ^= (hashcode >> 6);
        }
    }
  else
    {
      // displaced
      for (INDEX i = 0; i < len; i++)
        {
          hashcode += char_at(i);
          hashcode += (hashcode << 10);
          hashcode ^= (hashcode >> 6);
        }
    }
  hashcode += (hashcode << 3);
  hashcode ^= (hashcode >> 11);
  hashcode += (hashcode << 15);
  return hashcode & MOST_POSITIVE_FIXNUM;
}

unsigned long String::equalp_hash()
{
  long hashcode = 0;
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        {
          hashcode += toupper(_chars[i]);
          hashcode += (hashcode << 10);
          hashcode ^= (hashcode >> 6);
        }
    }
  else
    {
      // displaced
      for (INDEX i = 0; i < len; i++)
        {
          hashcode += toupper(char_at(i));
          hashcode += (hashcode << 10);
          hashcode ^= (hashcode >> 6);
        }
    }
  hashcode += (hashcode << 3);
  hashcode ^= (hashcode >> 11);
  hashcode += (hashcode << 15);
  return hashcode & MOST_POSITIVE_FIXNUM;
}

bool String::equal(Value value) const
{
  if (!stringp(value))
    return false;
  AbstractString * s = the_string(value);
  if (this == s)
    return true;
  if (length() != s->length())
    return false;
  // REVIEW optimize
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        {
          if (_chars[i] != s->fast_char_at(i))
            return false;
        }
    }
  else
    {
      // displaced
      for (INDEX i = 0; i < len; i++)
        {
          if (char_at(i) != s->fast_char_at(i))
            return false;
        }
    }
  return true;
}

bool String::equal(AbstractString * s) const
{
  if (length() != s->length())
    return false;
  // REVIEW optimize
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        {
          if (_chars[i] != s->fast_char_at(i))
            return false;
        }
    }
  else
    {
      for (INDEX i = 0; i < len; i++)
        {
          if (char_at(i) != s->fast_char_at(i))
            return false;
        }
    }
  return true;
}

bool String::equal(const char * s) const
{
  if (length() != strlen(s))
    return false;
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        {
          if (_chars[i] != s[i])
            return false;
        }
    }
  else
    {
      for (INDEX i = 0; i < len; i++)
        {
          if (fast_char_at(i) != s[i])
            return false;
        }
    }
  return true;
}

bool String::equalp(AbstractString * s) const
{
  if (this == s)
    return true;
  if (length() != s->length())
    return false;
  // REVIEW optimize
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        {
          BASE_CHAR c1 = _chars[i];
          BASE_CHAR c2 = s->fast_char_at(i);
          if (c1 == c2)
            continue;
          if (toupper(c1) == toupper(c2))
            continue;
          if (tolower(c1) == tolower(c2))
            continue;
          return false;
        }
    }
  else
    {
      for (INDEX i = 0; i < len; i++)
        {
          BASE_CHAR c1 = fast_char_at(i);
          BASE_CHAR c2 = s->fast_char_at(i);
          if (c1 == c2)
            continue;
          if (toupper(c1) == toupper(c2))
            continue;
          if (tolower(c1) == tolower(c2))
            continue;
          return false;
        }
    }
  return true;
}

// "CHAR ignores fill pointers when accessing elements."
BASE_CHAR String::char_at(INDEX i) const
{
  if (_chars)
    {
      if (i < _capacity)
        return _chars[i];
      return bad_index(i, 0, _capacity);
      // Not reached.
      return 0;
    }
  else
    // displaced
    return char_value(_array->aref(i + _offset));
}

// "CHAR ignores fill pointers when accessing elements."
void String::set_char_at(INDEX i, BASE_CHAR c)
{
  if (i < _capacity)
    {
      if (_chars)
        _chars[i] = c;
      else
        // displaced
        _array->aset(i + _offset, make_character(c));
    }
  else
    bad_index(i, 0, _capacity);
}

// "CHAR ignores fill pointers when accessing elements."
BASE_CHAR String::fast_char_at(INDEX i) const
{
  if (_chars)
    return _chars[i];
  else
    // displaced
    return char_value(_array->aref(i + _offset));
}

// "CHAR ignores fill pointers when accessing elements."
void String::fast_set_char_at(INDEX i, BASE_CHAR c)
{
  if (_chars)
    _chars[i] = c;
  else
    // displaced
    _array->aset(i + _offset, make_character(c));
}

long String::index_of(BASE_CHAR c) const
{
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = 0; i < len; i++)
        if (_chars[i] == c)
          return i;
    }
  else
    {
      // displaced
      for (INDEX i = 0; i < len; i++)
        if (fast_char_at(i) == c)
          return i;
    }
  return -1;
}

long String::last_index_of(BASE_CHAR c) const
{
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = len; i-- > 0;)
        if (_chars[i] == c)
          return i;
    }
  else
    {
      // displaced
      for (INDEX i = len; i-- > 0;)
        if (fast_char_at(i) == c)
          return i;
    }
  return -1;
}

String * String::shrink_to_length()
{
  if (_chars)
    {
      if (_has_fill_pointer && _fill_pointer < _capacity)
        {
          BASE_CHAR * new_chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(_fill_pointer + 1);
          memcpy(new_chars, _chars, _fill_pointer);
          new_chars[_fill_pointer] = 0;
          _chars = new_chars;
          _capacity = _fill_pointer;
        }
    }
  // REVIEW displaced case
  return this;
}

void String::fill(Value value)
{
  BASE_CHAR c = char_value(value);
  if (_chars)
    {
      for (INDEX i = 0; i < _capacity; i++)
        _chars[i] = c;
    }
  else
    {
      // displaced
      for (INDEX i = 0; i < _capacity; i++)
        set_char_at(i, c);
    }
}

// "AREF ignores fill pointers. It is permissible to use AREF to access any
// array element, whether active or not."
Value String::aref(INDEX i) const
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  if (_chars)
    return make_character(_chars[i]);
  // displaced
  return make_character(char_at(i));
}

Value String::aset(INDEX i, Value new_value)
{
  if (i >= _capacity)
    return bad_index(i, 0, _capacity);
  BASE_CHAR c = char_value(new_value);
  if (_chars)
    _chars[i] = c;
  else
    // displaced
    set_char_at(i, c);
  return new_value;
}

Value String::elt(INDEX i) const
{
  if (i >= length())
    return bad_index(i, 0, length());
  if (_chars)
    return make_character(_chars[i]);
  // displaced
  return make_character(fast_char_at(i));
}

inline Value String::_push(Value new_element)
{
  assert(_fill_pointer < _capacity);
  INDEX old_length = _fill_pointer;
  BASE_CHAR c = char_value(new_element);
  if (_chars)
    _chars[_fill_pointer] = c;
  else
    // displaced
    set_char_at(_fill_pointer, c);
  ++_fill_pointer;
  return make_fixnum(old_length);
}

Value String::push(Value new_element)
{
  check_fill_pointer();
  if (_fill_pointer < _capacity)
    return _push(new_element);
  else
    return NIL;
}

Value String::push_extend(Value new_element, INDEX extension)
{
  check_fill_pointer();
  if (_fill_pointer >= _capacity)
    ensure_capacity(_fill_pointer + extension);
  return _push(new_element);
}

Value String::push_extend(Value new_element)
{
  check_fill_pointer();
  if (_fill_pointer >= _capacity)
    {
      INDEX extension = _capacity;
      if (extension < 64)
        extension = 64;
      ensure_capacity(_fill_pointer + extension);
    }
  return _push(new_element);
}

Value String::pop()
{
  check_fill_pointer();
  if (_fill_pointer > 0)
    {
      --_fill_pointer;
      if (_chars)
        return make_character(_chars[_fill_pointer]);
      else
        // displaced
        return make_character(char_at(_fill_pointer));
    }
  else
    return signal_lisp_error("There is nothing left to pop.");
}

const char * String::as_c_string() const
{
  // copy the string to make sure it's null-terminated
  const INDEX len = length();
  char * s = (char *) GC_malloc_atomic_ignore_off_page(len + 1);
  if (_chars)
    memcpy(s, _chars, len);
  else
    {
      // displaced
      for (INDEX i = 0; i < len; i++)
        s[i] = fast_char_at(i);
    }
  s[len] = 0;
  return s;
}

const char * String::copy_to_c_string() const
{
  const INDEX len = length();
  char * s = (char *) GC_malloc_atomic_ignore_off_page(len + 1);
  if (_chars)
    memcpy(s, _chars, len);
  else
    {
      // displaced
      for (INDEX i = 0; i < len; i++)
        s[i] = fast_char_at(i);
    }
  s[len] = 0;
  return s;
}

AbstractString * String::downcase() const
{
  const INDEX len = length();
  SimpleString * s = new_simple_string(len);
  BASE_CHAR * chars = s->data();
  if (_chars)
    {
      for (INDEX i = len; i-- > 0;)
        chars[i] = tolower(_chars[i]);
    }
  else
    {
      // displaced
      for (INDEX i = len; i-- > 0;)
        chars[i] = tolower(fast_char_at(i));
    }
  return s;
}

AbstractString * String::upcase() const
{
  const INDEX len = length();
  SimpleString * s = new_simple_string(len);
  BASE_CHAR * chars = s->data();
  if (_chars)
    {
      for (INDEX i = len; i-- > 0;)
        chars[i] = toupper(_chars[i]);
    }
  else
    {
      // displaced
      for (INDEX i = len; i-- > 0;)
        chars[i] = toupper(fast_char_at(i));
    }
  return s;
}

String * String::ndowncase()
{
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = len; i-- > 0;)
        _chars[i] = tolower(_chars[i]);
    }
  else
    {
      // displaced
      for (INDEX i = len; i-- > 0;)
        fast_set_char_at(i, tolower(fast_char_at(i)));
    }
  return this;
}

String * String::nupcase()
{
  const INDEX len = length();
  if (_chars)
    {
      for (INDEX i = len; i-- > 0;)
        _chars[i] = toupper(_chars[i]);
    }
  else
    {
      // displaced
      for (INDEX i = len; i-- > 0;)
        fast_set_char_at(i, toupper(fast_char_at(i)));
    }
  return this;
}

// FIXME enforce array-dimension-limit
void String::ensure_capacity(INDEX n)
{
  if (_chars)
    {
      check_fill_pointer();
      if (_capacity < n)
        {
          // FIXME check for overflow
          INDEX new_capacity = _capacity * 2;
          if (new_capacity < n)
            new_capacity = n;
          BASE_CHAR * new_chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(new_capacity + 1);
          memcpy(new_chars, _chars, _fill_pointer);
          if (_fill_pointer < new_capacity)
            memset(new_chars + _fill_pointer, 0, new_capacity + 1 - _fill_pointer);
          else
            new_chars[new_capacity] = 0;
          BASE_CHAR * old_chars = _chars;
          _chars = new_chars;
          _capacity = new_capacity;
          GC_free(old_chars);
        }
    }
  else
    {
      // displaced
      if (_capacity < n || _array->total_size() - _offset < n)
        {
          // copy array
          // FIXME check for overflow
          INDEX new_capacity = _capacity * 2;
          if (new_capacity < n)
            new_capacity = n;
          _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(new_capacity + 1);
          memset(_chars, 0, new_capacity + 1);
          INDEX limit = _capacity;
          if (limit > _array->total_size() - _offset)
            limit = _array->total_size() - _offset;
          for (INDEX i = 0; i < limit; i++)
            _chars[i] = char_value(_array->aref(i + _offset));
          _capacity = new_capacity;
          _array = NULL;
          _offset = 0;
        }
    }
}

void String::append_char(char c)
{
  assert(has_fill_pointer());
  check_fill_pointer();
  ensure_capacity(_fill_pointer + 1);
  if (_chars)
    _chars[_fill_pointer] = c;
  else
    set_char_at(_fill_pointer, c);
  ++_fill_pointer;
}

void String::append(const char *s)
{
  // REVIEW s may be null
  assert(s);
  assert(has_fill_pointer());
  check_fill_pointer();
  // FIXME enforce array-dimension-limit
  const INDEX limit = strlen(s);
  ensure_capacity(_fill_pointer + limit + 1);
  if (_chars)
    {
      for (INDEX j = 0; j < limit; j++)
        _chars[_fill_pointer++] = s[j];
    }
  else
    {
      // displaced
      for (INDEX j = 0; j < limit; j++)
        set_char_at(_fill_pointer++, s[j]);
    }
}

void String::append(AbstractString * s)
{
  // REVIEW s may be null
  assert(s);
  assert(has_fill_pointer());
  check_fill_pointer();
  // FIXME enforce array-dimension-limit
  const INDEX limit = s->length();
  ensure_capacity(_fill_pointer + limit + 1);
  if (_chars)
    {
      for (INDEX j = 0; j < limit; j++)
        _chars[_fill_pointer++] = s->fast_char_at(j);
    }
  else
    {
      // displaced
      for (INDEX j = 0; j < limit; j++)
        set_char_at(_fill_pointer++, s->fast_char_at(j));
    }
}

void String::append_long(long n)
{
  assert(has_fill_pointer());
  char buf[128];
  SNPRINTF(buf, sizeof(buf), "%ld", n);
  append(buf);
}

void String::append_unsigned_long(unsigned long n)
{
  assert(has_fill_pointer());
  char buf[128];
  SNPRINTF(buf, sizeof(buf), "%lu", n);
  append(buf);
}

Value String::reverse() const
{
  const INDEX len = length();
  SimpleString * result = new_simple_string(len);
  BASE_CHAR * chars = result->data();
  INDEX i, j;
  if (_chars)
    {
      for (i = 0, j = len - 1; i < len; i++, j--)
        chars[i] = _chars[j];
    }
  else
    {
      // displaced
      for (i = 0, j = len - 1; i < len; i++, j--)
        chars[i] = fast_char_at(j);
    }
  return make_value(result);
}

Value String::nreverse()
{
  const INDEX len = length();
  if (len > 0)
    {
      INDEX i = 0;
      INDEX j = len - 1;
      if (_chars)
        {
          while (i < j)
            {
              BASE_CHAR temp = _chars[i];
              _chars[i] = _chars[j];
              _chars[j] = temp;
              ++i;
              --j;
            }
        }
      else
        {
          // displaced
          while (i < j)
            {
              BASE_CHAR temp = char_at(i);
              set_char_at(i, char_at(j));
              set_char_at(j, temp);
              ++i;
              --j;
            }
        }
    }
  return make_value(this);
}

Value String::subseq(INDEX start, INDEX end) const
{
  assert(end >= start);
  INDEX capacity = end - start;
  SimpleString * s = new_simple_string(capacity);
  BASE_CHAR * data = s->data();
  INDEX i = start, j = 0;
  if (_chars)
    {
      while (i < end)
        data[j++] = _chars[i++];
    }
  else
    {
      // displaced
      while (i < end)
        data[j++] = char_at(i++);
    }
  return make_value(s);
}

AbstractVector * String::adjust_vector(INDEX new_capacity,
                                       Value initial_element,
                                       Value initial_contents)
{
  if (initial_contents != NIL)
    {
      BASE_CHAR * new_chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(new_capacity + 1);
      if (listp(initial_contents))
        {
          Value list = initial_contents;
          for (INDEX i = 0; i < new_capacity; i++)
            {
              new_chars[i] = char_value(car(list));
              list = xcdr(list);
            }
        }
      else if (vectorp(initial_contents))
        {
          AbstractVector * v = the_vector(initial_contents);
          for (INDEX i = 0; i < new_capacity; i++)
            new_chars[i] = char_value(v->aref(i));
        }
      else
        signal_type_error(initial_contents, S_sequence);
      new_chars[new_capacity] = 0;
      _chars = new_chars;
    }
  else
    {
      if (_chars == NULL)
        {
          _chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(new_capacity + 1);
          INDEX limit = (_capacity < new_capacity) ? _capacity : new_capacity;
          for (INDEX i = 0; i < limit; i++)
            _chars[i] = char_value(_array->aref(i + _offset));
          if (_capacity < new_capacity)
            {
              BASE_CHAR c = (initial_element != NIL ? char_value(initial_element) : 0);
              for (INDEX i = _capacity; i < new_capacity; i++)
                _chars[i] = c;
            }
        }
      else if (_capacity != new_capacity)
        {
          BASE_CHAR * new_chars = (BASE_CHAR *) GC_malloc_atomic_ignore_off_page(new_capacity + 1);
          INDEX limit = (_capacity < new_capacity) ? _capacity : new_capacity;
          for (INDEX i = 0; i < limit; i++)
            new_chars[i] = _chars[i];
          if (_capacity < new_capacity)
            {
              BASE_CHAR c = (initial_element != NIL ? char_value(initial_element) : 0);
              for (INDEX i = _capacity; i < new_capacity; i++)
                new_chars[i] = c;
            }
          new_chars[new_capacity] = 0;
          _chars = new_chars;
        }
    }
  _capacity = new_capacity;

  // "The consequences are unspecified if array is adjusted to a size smaller
  // than its fill pointer without supplying the fill-pointer argument so that
  // its fill-pointer is properly adjusted in the process."
  if (_fill_pointer > _capacity)
    _fill_pointer = _capacity;

  _array = NULL;
  _offset = 0;
  return this;
}

AbstractVector * String::displace_vector(INDEX new_capacity,
                                         AbstractArray * displaced_to,
                                         INDEX offset)
{
  _capacity = new_capacity;

  // "The consequences are unspecified if array is adjusted to a size smaller
  // than its fill pointer without supplying the fill-pointer argument so that
  // its fill-pointer is properly adjusted in the process."
  if (_fill_pointer > _capacity)
    _fill_pointer = _capacity;

  _array = displaced_to;
  _offset = offset;
  _chars = NULL;
  return this;
}

AbstractString * String::write_to_string()
{
  Thread * thread = current_thread();
  bool escape =
    (thread->symbol_value(S_print_escape) != NIL || thread->symbol_value(S_print_readably) != NIL);
  const INDEX len = length();
  String * s = new String(len + 2);
  if (escape)
    s->append_char('"');
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = fast_char_at(i);
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

// ### char string index => character
Value CL_char(Value string, Value index)
{
  return make_character(check_string(string)->char_at(check_index(index)));
}

// ### set-char string index character => character
Value SYS_set_char(Value string, Value index, Value character)
{
  // "CHAR ignores fill pointers when accessing elements."
  check_string(string)->set_char_at(check_index(index), char_value(character));
  return character;
}

// ### %make-string
Value SYS_make_string_internal(Value size, Value initial_element, Value element_type)
{
  INDEX n = check_index(size);
  BASE_CHAR c = (initial_element != NIL) ? char_value(initial_element) : 0;
  return make_value(new_simple_string(n, c));
}

// ### %string-downcase string start end => cased-string
Value SYS_string_downcase_internal(Value arg1, Value arg2, Value arg3)
{
  if (!stringp(arg1))
    arg1 = CL_string(arg1);
  AbstractString * s = the_string(arg1);
  INDEX len = s->length();
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 != NIL) ? check_index(arg3, start, len) : len;
  SimpleString * s2 = new_simple_string(len);
  if (len)
    {
      BASE_CHAR * data = s->data();
      BASE_CHAR * data2 = s2->data();
      INDEX i;
      if (data)
        {
          for (i = 0; i < start; i++)
            data2[i] = data[i];
          for (i = start; i < end; i++)
            data2[i] = tolower(data[i]);
          for (i = end; i < len; i++)
            data2[i] = data[i];
        }
      else
        {
          // displaced
          for (i = 0; i < start; i++)
            data2[i] = s->fast_char_at(i);
          for (i = start; i < end; i++)
            data2[i] = tolower(s->fast_char_at(i));
          for (i = end; i < len; i++)
            data2[i] = s->fast_char_at(i);
        }
    }
  return make_value(s2);
}

// ### %string-upcase string start end => cased-string
Value SYS_string_upcase_internal(Value arg1, Value arg2, Value arg3)
{
  if (!stringp(arg1))
    arg1 = CL_string(arg1);
  AbstractString * s = the_string(arg1);
  INDEX len = s->length();
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 != NIL) ? check_index(arg3, start, len) : len;
  SimpleString * s2 = new_simple_string(len);
  if (len)
    {
      BASE_CHAR * data = s->data();
      BASE_CHAR * data2 = s2->data();
      INDEX i;
      if (data)
        {
          for (i = 0; i < start; i++)
            data2[i] = data[i];
          for (i = start; i < end; i++)
            data2[i] = toupper(data[i]);
          for (i = end; i < len; i++)
            data2[i] = data[i];
        }
      else
        {
          // displaced
          for (i = 0; i < start; i++)
            data2[i] = s->fast_char_at(i);
          for (i = start; i < end; i++)
            data2[i] = toupper(s->fast_char_at(i));
          for (i = end; i < len; i++)
            data2[i] = s->fast_char_at(i);
        }
    }
  return make_value(s2);
}

// ### %string-capitalize string start end => cased-string
Value SYS_string_capitalize_internal(Value arg1, Value arg2, Value arg3)
{
  if (!stringp(arg1))
    arg1 = CL_string(arg1);
  AbstractString * s = the_string(arg1);
  INDEX len = s->length();
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 != NIL) ? check_index(arg3, start, len) : len;
  SimpleString * s2 = new_simple_string(len);
  if (len)
    {
      BASE_CHAR * data = s->data();
      BASE_CHAR * data2 = s2->data();
      INDEX i;
      bool last_char_was_alphanumeric = false;
      if (data)
        {
          for (i = 0; i < start; i++)
            data2[i] = data[i];
          for (i = start; i < end; i++)
            {
              BASE_CHAR c = data[i];
              if (islower(c))
                {
                  data2[i] = (last_char_was_alphanumeric ? c : toupper(c));
                  last_char_was_alphanumeric = true;
                }
              else if (isupper(c))
                {
                  data2[i] = (last_char_was_alphanumeric ? tolower(c) : c);
                  last_char_was_alphanumeric = true;
                }
              else
                {
                  data2[i] = c;
                  last_char_was_alphanumeric = isdigit(c);
                }
            }
          for (i = end; i < len; i++)
            data2[i] = data[i];
        }
      else
        {
          // displaced
          for (i = 0; i < start; i++)
            data2[i] = s->fast_char_at(i);
          for (i = start; i < end; i++)
            {
              BASE_CHAR c = s->fast_char_at(i);
              if (islower(c))
                {
                  data2[i] = (last_char_was_alphanumeric ? c : toupper(c));
                  last_char_was_alphanumeric = true;
                }
              else if (isupper(c))
                {
                  data2[i] = (last_char_was_alphanumeric ? tolower(c) : c);
                  last_char_was_alphanumeric = true;
                }
              else
                {
                  data2[i] = c;
                  last_char_was_alphanumeric = isdigit(c);
                }
            }
          for (i = end; i < len; i++)
            data2[i] = s->fast_char_at(i);
        }
    }
  return make_value(s2);
}

// ### %nstring-downcase string start end => cased-string
Value SYS_nstring_downcase_internal(Value arg1, Value arg2, Value arg3)
{
  if (!stringp(arg1))
    arg1 = CL_string(arg1);
  AbstractString * s = the_string(arg1);
  INDEX len = s->length();
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 != NIL) ? check_index(arg3, start, len) : len;
  for (INDEX i = start; i < end; i++)
    s->fast_set_char_at(i, tolower(s->fast_char_at(i)));
  return make_value(s);
}

// ### %nstring-upcase string start end => cased-string
Value SYS_nstring_upcase_internal(Value arg1, Value arg2, Value arg3)
{
  if (!stringp(arg1))
    arg1 = CL_string(arg1);
  AbstractString * s = the_string(arg1);
  INDEX len = s->length();
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 != NIL) ? check_index(arg3, start, len) : len;
  for (INDEX i = start; i < end; i++)
    s->fast_set_char_at(i, toupper(s->fast_char_at(i)));
  return make_value(s);
}

// ### %nstring-capitalize string start end => cased-string
Value SYS_nstring_capitalize_internal(Value arg1, Value arg2, Value arg3)
{
  if (!stringp(arg1))
    arg1 = CL_string(arg1);
  AbstractString * s = the_string(arg1);
  INDEX len = s->length();
  INDEX start = check_index(arg2, 0, len);
  INDEX end = (arg3 != NIL) ? check_index(arg3, start, len) : len;
  bool last_char_was_alphanumeric = false;
  for (INDEX i = start; i < end; i++)
    {
      BASE_CHAR c = s->fast_char_at(i);
      if (islower(c))
        {
          s->fast_set_char_at(i, (last_char_was_alphanumeric ? c : toupper(c)));
          last_char_was_alphanumeric = true;
        }
      else if (isupper(c))
        {
          s->fast_set_char_at(i, (last_char_was_alphanumeric ? tolower(c) : c));
          last_char_was_alphanumeric = true;
        }
      else
        {
          s->fast_set_char_at(i, c);
          last_char_was_alphanumeric = isdigit(c);
        }
    }
  return make_value(s);
}
