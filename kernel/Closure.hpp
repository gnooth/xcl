// Closure.hpp
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

#ifndef __CLOSURE_HPP
#define __CLOSURE_HPP

class Closure : public Function
{
private:
  Value _lambda_list;
  Value _decls;
  Value _body;
  Environment * _env;

  Value _restvar;
  Value _envvar;
  Value _auxvars;

  Value _specials;

  Value * _required_parameters;

  bool _allow_other_keys;

  void bind(Value name, Value value, Environment * env);

  unsigned int _num_optional_parameters;
  class OptionalParameter * * _op;

  unsigned int _num_keyword_parameters;
  class KeywordParameter * * _kp;

  bool _non_constant_initform_p;

  Value _lambda_list_names; // a list of symbols (or NIL)

  void init(Value name, Value lambda_expression);
  void init(Value name, Value lambda_list, Value body, Value decls, Value doc);
  void init_common();

  void initialize_lambda_list_names();

public:
  Closure(Value lambda_expression, Environment * env);
  Closure(Value name, Value lambda_expression, Environment * env);

  Closure(Value name, Value lambda_list, Value body, Value decls, Value doc,
          Environment * env);

  Value lambda_list() const
  {
    return _lambda_list;
  }

  Value declarations() const
  {
    return _decls;
  }

  Value body() const
  {
    return _body;
  }

  Environment * environment() const
  {
    return _env;
  }

  Value lambda_list_names() const
  {
    return _lambda_list_names;
  }

  virtual Value execute();
  virtual Value execute(Value arg);
  virtual Value execute(Value arg1, Value arg2);
  virtual Value execute(Value arg1, Value arg2, Value arg3);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6);
  virtual Value execute(unsigned int numargs, Value args[]);

  Value * process_args(unsigned int numargs, Value args[], Value vals[]);

  virtual AbstractString * write_to_string();
};

inline bool closurep(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_CLOSURE);
}

inline Closure * the_closure(Value value)
{
  assert(closurep(value));
  return reinterpret_cast<Closure *>(value - LOWTAG_TYPED_OBJECT);
}

inline Closure * check_closure(Value value)
{
  if (closurep(value))
    return the_closure(value);
  signal_type_error(value, S_closure);
  // Not reached.
  return NULL;
}

#endif // Closure.hpp
