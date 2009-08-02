// Environment.hpp
//
// Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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

#ifndef __ENVIRONMENT_HPP
#define __ENVIRONMENT_HPP

#include "Frame.hpp"

class Environment : public TypedObject
{
private:
  Binding * _vars;
  Binding * _local_functions;

public:
  Environment();

  Environment(Environment * parent);

  bool is_empty() const
  {
    return _vars == NULL;
  }

  Binding * vars() const
  {
    return _vars;
  }

  virtual Value type_of() const;

  virtual bool typep(Value type) const;

  void bind(Value name, Value value)
  {
    _vars = new Binding(name, value, _vars);
  }

  Value lookup(Value name);

  Binding * get_binding(Value name);

  void rebind(Value name, Value value)
  {
    Binding * binding = get_binding(name);
    binding->set_value(value);
  }

  void declare_special(Value name)
  {
    _vars = new Binding(name, NULL_VALUE, true, _vars);
  }

  void declare_specials(Value specials)
  {
    while (specials != NIL)
      {
        declare_special(car(specials));
        specials = cdr(specials);
      }
  }

  bool is_declared_special(Value name)
  {
    Binding * binding = get_binding(name);
    return binding ? binding->specialp() : false;
  }

  void add_local_function(Value name, Value value)
  {
    _local_functions = new Binding(name, value, _local_functions);
  }

  TypedObject * lookup_function(Value name);

  Value process_declarations(Value body);
};

inline Value make_value(Environment * env)
{
  if (env)
    return (Value) (reinterpret_cast<long>(env) | LOWTAG_TYPED_OBJECT);
  else
    return NULL_VALUE;
}

inline bool environmentp(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_ENVIRONMENT);
}

inline Environment * the_environment(Value value)
{
  assert(environmentp(value));
  return reinterpret_cast<Environment *>(value - LOWTAG_TYPED_OBJECT);
}

inline Environment * check_environment(Value value)
{
  if (environmentp(value))
    return the_environment(value);
  signal_type_error(value, S_environment);
  // Not reached.
  return NULL;
}

#endif
