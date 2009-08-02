// NilVector.hpp
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

#ifndef __NIL_VECTOR_HPP
#define __NIL_VECTOR_HPP

class SimpleString;

class NilVector : public AbstractString
{
private:
  bool _has_fill_pointer;
  INDEX _length;

  Value access_error() const
  {
    // REVIEW
    return signal_lisp_error("Attempt to access an array of element type NIL.");
  }

public:
  NilVector(unsigned long capacity, Value fill_pointer);

  BASE_CHAR * data();

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual Value element_type() const;

  virtual bool is_adjustable() const
  {
    return false;
  }

  virtual bool has_fill_pointer() const
  {
    return _has_fill_pointer;
  }

  virtual INDEX length() const
  {
    return _length;
  }

  virtual void set_length(INDEX length);

  virtual void fill(Value value);

  virtual Value elt(INDEX i) const;

  virtual Value push(Value new_element);
  virtual Value push_extend(Value new_element, INDEX extension);

  virtual Value pop();

  virtual Value reverse() const;
  virtual Value nreverse();

  virtual Value subseq(INDEX start, INDEX end) const;

  virtual Value aref(INDEX i) const;
  virtual Value aset(INDEX i, Value new_value);

  virtual BASE_CHAR char_at(INDEX i) const;
  virtual void set_char_at(INDEX i, BASE_CHAR c);

  virtual BASE_CHAR fast_char_at(INDEX i) const;
  virtual void fast_set_char_at(INDEX i, BASE_CHAR c);

  virtual long index_of(BASE_CHAR c) const;
  virtual long last_index_of(BASE_CHAR c) const;

  virtual SimpleString * substring(INDEX begin, INDEX end) const;
  virtual SimpleString * substring(INDEX begin) const;

  virtual unsigned long hash()
  {
    return 0;
  }

  virtual bool equal(Value value) const;
  virtual bool equal(AbstractString * s) const;
  virtual bool equal(const char * s) const;

  virtual bool equalp(AbstractString * s) const;

  virtual const char * as_c_string() const;
  virtual const char * copy_to_c_string() const;

  virtual AbstractString * downcase() const;
  virtual AbstractString * upcase() const;

  virtual AbstractString * write_to_string();
};

inline bool nil_vector_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_NIL_VECTOR);
}

inline NilVector * the_nil_vector(Value value)
{
  assert(nil_vector_p(value));
  return reinterpret_cast<NilVector *>(value - LOWTAG_TYPED_OBJECT);
}

inline NilVector * check_nil_vector(Value value)
{
  if (nil_vector_p(value))
    return the_nil_vector(value);
  signal_type_error(value, S_nil_vector);
  // Not reached.
  return NULL;
}

#endif // NilVector.hpp
