// FuncallableStandardObject.hpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

#ifndef __FUNCALLABLE_STANDARD_OBJECT_HPP
#define __FUNCALLABLE_STANDARD_OBJECT_HPP

class FuncallableStandardObject : public StandardObject
{
private:
  TypedObject * _function;
  bool _autocompilep;

public:
  FuncallableStandardObject(Layout * layout)
    : StandardObject(WIDETAG_FUNCALLABLE_STANDARD_OBJECT, layout, layout->numslots()),
      _function(NULL), _autocompilep(false)
  {
  }

  TypedObject * funcallable_instance_function() const
  {
    return _function;
  }

  void set_funcallable_instance_function(TypedObject * function)
  {
    _function = function;
    if (function->widetag() == WIDETAG_CLOSURE) // REVIEW
      _autocompilep = true;
  }

  virtual Value execute()
  {
    if (_autocompilep)
      autocompile();
    return _function->execute();
  }

  virtual Value execute(Value arg)
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(arg);
  }

  virtual Value execute(Value arg1, Value arg2)
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(arg1, arg2);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3)
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(arg1, arg2, arg3);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4)
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(arg1, arg2, arg3, arg4);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(arg1, arg2, arg3, arg4, arg5);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(arg1, arg2, arg3, arg4, arg5, arg6);
  }

  virtual Value execute(unsigned int numargs, Value args[])
  {
    if (_autocompilep)
      autocompile();
    return _function->execute(numargs, args);
  }

  virtual int arity() const
  {
    return _function->arity();
  }

  virtual unsigned int minargs() const
  {
    return _function->minargs();
  }

  virtual unsigned int maxargs() const
  {
    return _function->maxargs();
  }

  virtual void increment_call_count()
  {
    _function->increment_call_count();
  }

  void autocompile();
};

inline bool funcallable_instance_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_FUNCALLABLE_STANDARD_OBJECT);
}

inline FuncallableStandardObject * the_funcallable_instance(Value value)
{
  assert(funcallable_instance_p(value));
  return reinterpret_cast<FuncallableStandardObject *>(value - LOWTAG_TYPED_OBJECT);
}

inline FuncallableStandardObject * check_funcallable_instance(Value value)
{
  if (funcallable_instance_p(value))
    return the_funcallable_instance(value);
  signal_type_error(value, S_funcallable_standard_object);
  // Not reached.
  return NULL;
}

#endif
