// TypedObject.hpp
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

#ifndef __TYPED_OBJECT_HPP
#define __TYPED_OBJECT_HPP

class AbstractString;

class TypedObject : public gc
{
private:
  long _widetag;

public:
  TypedObject(long widetag) : _widetag(widetag)
  {
  }

  // REVIEW
  virtual ~TypedObject() {}

  long widetag() const
  {
    return _widetag;
  }

  long widetag_offset()
  {
    return ((long)(&(this->_widetag))) - ((long)this);
  }

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual unsigned long hash()
  {
    // REVIEW
    return ((long)this) & MOST_POSITIVE_FIXNUM;
  }

  virtual unsigned long equalp_hash()
  {
    return hash();
  }

  virtual bool eql(Value value) const;

  virtual bool equal(Value value) const;

  virtual bool equalp(Value value) const;

  virtual AbstractString * write_to_string();

  virtual AbstractString * princ_to_string();

  virtual AbstractString * prin1_to_string();

  virtual AbstractString * unreadable_string();

  virtual AbstractString * unreadable_string(AbstractString * string);

  Value not_a_function();

  Value not_a_sequence();

  virtual Value execute()
  {
    return not_a_function();
  }

  virtual Value execute(Value arg)
  {
    return not_a_function();
  }

  virtual Value execute(Value arg1, Value arg2)
  {
    return not_a_function();
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3)
  {
    return not_a_function();
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4)
  {
    return not_a_function();
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
  {
    return not_a_function();
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
  {
    return not_a_function();
  }

  virtual Value execute(unsigned int numargs, Value args[])
  {
    return not_a_function();
  }

  virtual int arity() const;

  virtual unsigned int minargs() const;

  virtual unsigned int maxargs() const;

  virtual unsigned long call_count() const
  {
    return 0;
  }

  virtual void set_call_count(unsigned long n)
  {
  }

  virtual void increment_call_count()
  {
  }

  virtual Value operator_name() const
  {
    return NIL;
  }

  virtual Value parts();
};

inline bool typed_object_p(Value value)
{
  return lowtag_of(value) == LOWTAG_TYPED_OBJECT;
}

inline TypedObject * the_typed_object(Value value)
{
  assert(lowtag_of(value) == LOWTAG_TYPED_OBJECT);
  return reinterpret_cast<TypedObject *>(value - LOWTAG_TYPED_OBJECT);
}

inline TypedObject * check_typed_object(Value value)
{
  if (typed_object_p(value))
    return the_typed_object(value);
  signal_type_error(value, S_typed_object);
  // not reached
  return NULL;
}

inline Value make_value(const TypedObject * p)
{
  assert((((long)p) & LOWTAG_MASK) == 0);
  return (Value) (reinterpret_cast<long>(p) | LOWTAG_TYPED_OBJECT);
}

inline Value TypedObject::not_a_function()
{
  return signal_type_error(make_value(this), S_function);
}

inline Value TypedObject::not_a_sequence()
{
  return signal_type_error(make_value(this), S_sequence);
}

inline TYPECODE typecode_of(Value value)
{
  const long lowtag = lowtag_of(value);
  switch (lowtag)
    {
    case LOWTAG_EVEN_FIXNUM:
    case LOWTAG_ODD_FIXNUM:
      return TYPECODE_FIXNUM;
    case LOWTAG_TYPED_OBJECT:
      return the_typed_object(value)->widetag();
    default:
      return lowtag;
    }
}

#endif
