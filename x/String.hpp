// String.hpp
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

#ifndef __STRING_HPP
#define __STRING_HPP

class String : public AbstractString
{
private:
  BASE_CHAR * _chars;

  bool _has_fill_pointer;
  INDEX _fill_pointer;

  // displaced strings
  AbstractArray * _array;
  INDEX _offset;

  void ensure_capacity(INDEX n);

  Value _push(Value new_element);

public:
  String();
  String(const AbstractString * const s);
  String(const char * s);
  String(const AbstractString * const s, bool has_fill_pointer);
  String(const char * s, bool has_fill_pointer);
  String(INDEX capacity);
  String(INDEX capacity, Value fill_pointer);
  String(INDEX capacity, INDEX length, BASE_CHAR c);

  // displaced strings
  String(INDEX capacity, AbstractArray * array, INDEX offset, Value fill_pointer);

  virtual BASE_CHAR * data()
  {
    return _chars;
  }

  virtual bool has_fill_pointer() const
  {
    return _has_fill_pointer;
  }

  virtual bool is_adjustable() const
  {
    return true;
  }

  virtual INDEX length() const
  {
    return _has_fill_pointer ? _fill_pointer : _capacity;
  }

  virtual void set_length(INDEX length)
  {
    if (_has_fill_pointer)
      {
        assert(length <= _capacity);
        _fill_pointer = length;
      }
    else
      no_fill_pointer();
  }

  virtual bool is_displaced() const;

  virtual Value displacement() const;

  virtual Value type_of() const;

  virtual bool typep(Value type) const;

  virtual unsigned long hash();

  virtual BASE_CHAR char_at(INDEX i) const;
  virtual void set_char_at(INDEX i, BASE_CHAR c);

  virtual BASE_CHAR fast_char_at(INDEX i) const;
  virtual void fast_set_char_at(INDEX i, BASE_CHAR c);

  virtual long index_of(BASE_CHAR c) const;
  virtual long last_index_of(BASE_CHAR c) const;

  String * shrink_to_length();

  virtual void fill(Value value);

  virtual Value aref(INDEX i) const;
  virtual Value aset(INDEX i, Value new_value);

  virtual Value elt(INDEX i) const;

  virtual Value push(Value new_element);
  virtual Value push_extend(Value new_element, INDEX extension);
  virtual Value push_extend(Value new_element);

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

  const char * as_c_string() const;
  const char * copy_to_c_string() const;

  void append_char(char c);
  void append(const char * s);
  void append(AbstractString * s);
  void append_long(long n);
  void append_unsigned_long(unsigned long n);

  AbstractString * downcase() const;
  AbstractString * upcase() const;

  String * ndowncase();
  String * nupcase();

  virtual AbstractString * write_to_string();
};

#endif
