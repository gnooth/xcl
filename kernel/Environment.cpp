// Environment.cpp
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

#include "lisp.hpp"
#include "Environment.hpp"
#include "primitives.hpp"

Environment::Environment()
  : TypedObject(WIDETAG_ENVIRONMENT), _vars(NULL), _local_functions(NULL)
{
}

Environment::Environment(Environment * parent)
  : TypedObject(WIDETAG_ENVIRONMENT)
{
  if (parent)
    {
      _vars = parent->_vars;
      _local_functions = parent->_local_functions;
    }
  else
    {
      _vars = NULL;
      _local_functions = NULL;
    }
}

bool Environment::typep(Value type) const
{
  return (type == S_environment || type == S_atom || type == T || type == C_t);
}

Value Environment::type_of() const
{
  return S_environment;
}

Value Environment::lookup(Value name)
{
  Binding * binding = _vars;
  while (binding)
    {
      if (binding->name() == name)
        return binding->value();
      binding = binding->next();
    }
  return NULL_VALUE;
}

TypedObject * Environment::lookup_function(Value name)
{
  Binding * binding = _local_functions;
  if (symbolp(name))
    {
      while (binding)
        {
          if (binding->name() == name)
            return the_operator(binding->value());
          binding = binding->next();
        }
      return the_symbol(name)->function();
    }
  else if (consp(name))
    {
      while (binding)
        {
          if (::equal(binding->name(), name))
            return the_operator(binding->value());
          binding = binding->next();
        }
    }
  return NULL;
}

Binding * Environment::get_binding(Value name)
{
  Binding * binding = _vars;
  while (binding)
    {
      if (binding->name() == name)
        return binding;
      binding = binding->next();
    }
  return NULL;
}

// Returns body with declarations removed.
Value Environment::process_declarations(Value body)
{
  while (body != NIL)
    {
      Value obj = car(body);
      if (consp(obj) && xcar(obj) == S_declare)
        {
          Value decls = xcdr(obj);
          while (decls != NIL)
            {
              Value decl = car(decls);
              if (consp(decl) && xcar(decl) == S_special)
                {
                  Value names = xcdr(decl);
                  while (names != NIL)
                    {
                      declare_special(car(names));
                      names = xcdr(names);
                    }
                }
              decls = xcdr(decls);
            }
          body = xcdr(body);
        }
      else
        break;
    }
  return body;
}

// make-environment
Value SYS_make_environment(unsigned int numargs, Value args[])
{
  Environment * parent = NULL;
  switch (numargs)
    {
    case 0:
      break;
    case 1:
      if (args[0] != NIL)
        parent = check_environment(args[0]);
      break;
    default:
      return wrong_number_of_arguments(S_make_environment, numargs, 0, 1);
    }
  Environment * env = parent ? new Environment(parent) : new Environment();
  return make_value(env);
}

// ### environment-add-function-definition
Value SYS_environment_add_function_definition(Value env, Value name, Value value)
{
  check_environment(env)->add_local_function(name, value);
  return env;
}

// ### environment-add-macro-definition
Value SYS_environment_add_macro_definition(Value env, Value name, Value value)
{
  check_environment(env)->add_local_function(name, value);
  return env;
}

// ### environment-empty-p
Value SYS_environment_empty_p(Value arg)
{
  return check_environment(arg)->is_empty() ? T : NIL;
}

// ### environment-variables
// REVIEW package?
Value EXT_environment_variables(Value arg)
{
  Environment * env = check_environment(arg);
  Value result = NIL;
  for (Binding * binding = env->vars(); binding != NULL; binding = binding->next())
    if (!binding->specialp())
      result = make_cons(make_cons(binding->name(), binding->value()), result);
  return CL_nreverse(result);
}
