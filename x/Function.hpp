// Function.hpp
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

#ifndef __FUNCTION_HPP
#define __FUNCTION_HPP

class Function : public Operator
{
protected:
  int _arity;
  unsigned int _minargs;
  unsigned int _maxargs;
  Value _plist;

  unsigned long _call_count;

public:
  Function(long widetag, Value name) : Operator(widetag, name), _plist(NULL_VALUE)
  {
  }

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual bool is_special_operator()
  {
    return false;
  }

  virtual int arity() const
  {
    return _arity;
  }

  virtual unsigned int minargs() const
  {
    return _minargs;
  }

  virtual unsigned int maxargs() const
  {
    return _maxargs;
  }

  Value plist()
  {
    if (_plist == NULL_VALUE)
      _plist = NIL;
    return _plist;
  }

  void set_plist(Value plist)
  {
    _plist = plist;
  }

  unsigned long call_count() const
  {
    return _call_count;
  }

  void set_call_count(unsigned long n)
  {
    _call_count = n;
  }

  void increment_call_count()
  {
    ++_call_count;
  }

  virtual Value parts();

  virtual AbstractString * write_to_string();
};

inline bool functionp(Value value)
{
  if (typed_object_p(value))
    {
      if (the_typed_object(value)->widetag() & WIDETAG_FUNCTION_BIT)
        return true;
    }
  return false;
}

inline Function * the_function(Value value)
{
  assert(functionp(value));
  return reinterpret_cast<Function *>(value - LOWTAG_TYPED_OBJECT);
}

inline Function * check_function(Value value)
{
  if (functionp(value))
    return the_function(value);
  signal_type_error(value, S_function);
  // not reached
  return NULL;
}

#endif // Function.hpp
