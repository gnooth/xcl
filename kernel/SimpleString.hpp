// SimpleString.hpp
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

#ifndef __SIMPLE_STRING_HPP
#define __SIMPLE_STRING_HPP

// REVIEW SimpleBaseString
class SimpleString : public AbstractString
{
private:
  BASE_CHAR _chars[0];

public:
  void * operator new(size_t size, INDEX capacity)
  {
    // REVIEW GC_malloc_atomic_ignore_off_page
    return GC_malloc_atomic_ignore_off_page(sizeof(SimpleString) + (capacity + 1) * sizeof(BASE_CHAR));
  }

  SimpleString(INDEX capacity, AbstractString * s);
  SimpleString(INDEX capacity, const char * s);
  SimpleString(INDEX capacity);
  SimpleString(INDEX capacity, BASE_CHAR c);
  SimpleString(INDEX capacity, BASE_CHAR * chars);

  long data_offset()
  {
    return ((long)(&(this->_chars))) - ((long)this);
  }

  virtual BASE_CHAR * data()
  {
    return _chars;
  }

  virtual bool has_fill_pointer() const
  {
    return false;
  }

  virtual bool is_adjustable() const
  {
    return false;
  }

  virtual INDEX length() const
  {
    return _capacity;
  }

  virtual void set_length(INDEX length)
  {
    no_fill_pointer();
  }

  virtual bool is_displaced() const
  {
    return false;
  }

  virtual Value displacement() const;

  virtual Value type_of() const
  {
    return S_simple_base_string;
  }

  virtual bool typep(Value type) const;

  virtual unsigned long hash();

  virtual unsigned long equalp_hash();

  // not virtual
  BASE_CHAR xchar_at(INDEX i) const
  {
    if (i < _capacity)
      return _chars[i];
    signal_lisp_error("bad index");
    // not reached
    return 0;
  }

  virtual BASE_CHAR char_at(INDEX i) const;

  virtual void set_char_at(INDEX i, BASE_CHAR c);

  BASE_CHAR xfast_char_at(INDEX i) const
  {
    return _chars[i];
  }

  virtual BASE_CHAR fast_char_at(INDEX i) const
  {
    return _chars[i];
  }

  virtual void fast_set_char_at(INDEX i, BASE_CHAR c)
  {
    _chars[i] = c;
  }

  long index_of(BASE_CHAR c) const;

  long last_index_of(BASE_CHAR c) const;

  virtual void fill(Value value);

  virtual Value aref(INDEX i) const;

  virtual Value aset(INDEX i, Value new_value);

  virtual Value elt(INDEX i) const;

  virtual Value push(Value new_element);

  virtual Value push_extend(Value new_element, INDEX extension);

  virtual Value pop();

  virtual Value reverse() const;

  virtual Value nreverse();

  virtual Value subseq(INDEX start, INDEX end) const;

  virtual AbstractVector * adjust_vector(INDEX new_capacity,
                                         Value initial_element,
                                         Value initial_contents);

  virtual AbstractVector * displace_vector(INDEX new_capacity,
                                           AbstractArray * displaced_to,
                                           INDEX offset);

  virtual SimpleString * substring(INDEX begin, INDEX end) const;

  virtual SimpleString * substring(INDEX begin) const
  {
    return substring(begin, length());
  }

  virtual bool equal(Value value) const;

  virtual bool equal(AbstractString * s) const;
  virtual bool equal(const char * s) const;
  virtual bool equalp(AbstractString * s) const;

  virtual const char * as_c_string() const;
  virtual const char * copy_to_c_string() const;

  virtual AbstractString * downcase() const;
  virtual AbstractString * upcase() const;

  SimpleString * ndowncase();
  SimpleString * nupcase();

  virtual AbstractString * write_to_string();
};

inline SimpleString * new_simple_string(AbstractString * s)
{
  INDEX capacity = s->length();
  return new(capacity) SimpleString(capacity, s);
}

inline SimpleString * new_simple_string(const char * s)
{
  INDEX capacity = strlen(s);
  return new(capacity) SimpleString(capacity, s);
}

inline SimpleString * new_simple_string(INDEX capacity)
{
  return new(capacity) SimpleString(capacity);
}

inline SimpleString * new_simple_string(INDEX capacity, BASE_CHAR c)
{
  return new(capacity) SimpleString(capacity, c);
}

inline SimpleString * new_simple_string(INDEX capacity, BASE_CHAR * chars)
{
  return new(capacity) SimpleString(capacity, chars);
}

inline bool simple_string_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SIMPLE_STRING);
}

inline SimpleString * the_simple_string(Value value)
{
  assert(simple_string_p(value));
  return reinterpret_cast<SimpleString *>(value - LOWTAG_TYPED_OBJECT);
}

inline SimpleString * check_simple_string(Value value)
{
  if (simple_string_p(value))
    return the_simple_string(value);
  signal_type_error(value, S_simple_string);
  // not reached
  return NULL;
}

inline Value make_simple_string(const char * s)
{
  return make_value(new_simple_string(s));
}

extern long simple_string_data_offset();

#endif
