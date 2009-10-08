// AbstractString.hpp
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

#ifndef __ABSTRACT_STRING_HPP
#define __ABSTRACT_STRING_HPP

class SimpleString;

class AbstractString : public AbstractVector
{
protected:
  AbstractString(long widetag) : AbstractVector(widetag)
  {
  }

  AbstractString(long widetag, INDEX capacity)
    : AbstractVector(widetag, capacity)
  {
  }

  AbstractString(long widetag, INDEX capacity, BASE_CHAR * chars)
    : AbstractVector(widetag, capacity)
  {
  }

public:
  virtual BASE_CHAR * data() = 0;

  virtual Value class_of() const;

  virtual Value element_type() const;

  virtual BASE_CHAR char_at(INDEX i) const = 0;
  virtual void set_char_at(INDEX i, BASE_CHAR c) = 0;

  virtual BASE_CHAR fast_char_at(INDEX i) const = 0;
  virtual void fast_set_char_at(INDEX i, BASE_CHAR c) = 0;

  virtual long index_of(BASE_CHAR c) const = 0;
  virtual long last_index_of(BASE_CHAR c) const = 0;

  virtual SimpleString * substring(INDEX begin, INDEX end) const = 0;
  virtual SimpleString * substring(INDEX begin) const = 0;

  virtual bool equal(AbstractString * s) const = 0;
  virtual bool equal(const char * s) const = 0;

  virtual bool equalp(Value value) const;
  virtual bool equalp(AbstractString * s) const = 0;

  virtual const char * as_c_string() const = 0;
  virtual const char * copy_to_c_string() const = 0;

  virtual AbstractString * downcase() const = 0;
  virtual AbstractString * upcase() const = 0;
};

inline bool stringp(Value value)
{
  if (typed_object_p(value))
    {
      switch (the_typed_object(value)->widetag())
        {
        case WIDETAG_STRING:
        case WIDETAG_SIMPLE_STRING:
        case WIDETAG_NIL_VECTOR:
          return true;
        }
    }
  return false;
}

inline AbstractString * the_string(Value value)
{
  assert(stringp(value));
  return reinterpret_cast<AbstractString *>(value - LOWTAG_TYPED_OBJECT);
}

inline AbstractString * check_string(Value value)
{
  if (stringp(value))
    return the_string(value);
  signal_type_error(value, S_string);
  // not reached
  return NULL;
}

extern AbstractString * string(Value value);

#endif // AbstractString.hpp
