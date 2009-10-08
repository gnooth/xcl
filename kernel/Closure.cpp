// Closure.cpp
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

#if defined(WIN32)
#include <malloc.h>
#elif defined(__NetBSD__) || defined(__FreeBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

#include "lisp.hpp"
#include "primitives.hpp"
#include "Closure.hpp"
#include "Environment.hpp"
#include "Package.hpp"
#include "PrintNotReadable.hpp"
#include "WrongNumberOfArgumentsError.hpp"

enum
{
  STATE_REQUIRED = 0,
  STATE_OPTIONAL = 1,
  STATE_REST = 2,
  STATE_KEYWORD = 3,
  STATE_AUX = 4
};

class OptionalParameter : public gc
{
public:
  Value _var;
  Value _initform;
  Value _initval;
  Value _supplied_p_var;
};

class KeywordParameter : public gc
{
public:
  Value _keyword;
  Value _var;
  Value _initform;
  Value _initval;
  Value _supplied_p_var;
};

static void invalid_parameter(Value value)
{
  String * s = new String("The value ");
  s->append(prin1_to_string(value));
  s->append(" may not be used as a variable in a lambda list.");
  signal_lisp_error(s);
}

Closure::Closure(Value lambda_expression, Environment * env)
  : Function(WIDETAG_CLOSURE, NULL_VALUE), _env(env)
{
  init(NULL_VALUE, lambda_expression);
}

Closure::Closure(Value name, Value lambda_expression, Environment * env)
  : Function(WIDETAG_CLOSURE, name), _env(env)
{
  init(name, lambda_expression);
}

Closure::Closure(Value name, Value lambda_list, Value body, Value decls,
                 Value doc, Environment * env)
  : Function(WIDETAG_CLOSURE, name), _env(env)
{
  init(name, lambda_list, body, decls, doc);
}

static Value process_initform(Value initform)
{
  if (constantp(initform))
    {
      if (symbolp(initform))
        return the_symbol(initform)->value();
      if (consp(initform) && xcar(initform) == S_quote)
        return CL_cadr(initform);
      return initform;
    }
  if (consp(initform) && xcar(initform) == S_function)
    {
      Value name = car(xcdr(initform));
      if (symbolp(name) && the_symbol(name)->is_kernel_function())
        return make_value(the_symbol(name)->function());
    }
  return NULL_VALUE;
}

void Closure::init(Value name, Value lambda_list, Value body, Value decls,
                   Value doc)
{
  assert(widetag() == WIDETAG_FUNCTION || WIDETAG_CLOSURE);
  _lambda_list = lambda_list;
  _body = body;
  _decls = decls;
  init_common();
}

void Closure::init(Value name, Value lambda_expression)
{
  assert(widetag() == WIDETAG_FUNCTION || WIDETAG_CLOSURE);
  if (!listp(lambda_expression))
    signal_type_error(lambda_expression, S_list);
  assert(car(lambda_expression) == S_lambda);
  _lambda_list = CL_cadr(lambda_expression);
  Thread * thread = current_thread();
  _body = parse_body(CL_cddr(lambda_expression), true, thread);
  assert(thread->values_length() == 3);
  _decls = thread->nth_value(1);
  assert(_decls == NIL || consp(_decls));
  thread->clear_values();
  init_common();
}

void Closure::init_common()
{
  _restvar = NIL;
  _envvar = NIL;
  _auxvars = NIL;

  _allow_other_keys = false;

  _minargs = 0;
  _maxargs = 0;

  _num_optional_parameters = 0;
  _num_keyword_parameters = 0;

  _non_constant_initform_p = false;

  if (_lambda_list != NIL)
    {
      long state = STATE_REQUIRED;
      Value tail = _lambda_list;
      while (tail != NIL)
        {
          // STATE_REQUIRED
          Value parameter = car(tail);
          tail = cdr(tail);
          if (parameter == S_and_environment)
            {
              _envvar = car(tail);
              if (!symbolp(_envvar))
                signal_type_error(_envvar, S_symbol);
              tail = cdr(tail);
              _maxargs = CALL_ARGUMENTS_LIMIT; // REVIEW
              continue;
            }
          if (parameter == S_and_optional)
            {
              state = STATE_OPTIONAL;
              break;
            }
          if (parameter == S_and_rest)
            {
              state = STATE_REST;
              break;
            }
          if (parameter == S_and_key)
            {
              state = STATE_KEYWORD;
              break;
            }
          if (parameter == S_and_aux)
            {
              state = STATE_AUX;
              break;
            }
          // required arguments are always symbols
          if (!symbolp(parameter))
            invalid_parameter(parameter);
          ++_minargs;
          ++_maxargs;
        }
      if (state == STATE_OPTIONAL)
        {
          // count the optional parameters
          assert(_num_optional_parameters == 0);
          Value list = tail;
          while (list != NIL)
            {
              Value parameter = car(list);
              list = xcdr(list);
              if (parameter == S_and_environment)
                {
                  list = cdr(list);
                  continue;
                }
              if (parameter == S_and_rest)
                break;
              if (parameter == S_and_key)
                break;
              if (parameter == S_and_aux)
                break;
              ++_num_optional_parameters;
            }

          _op = new (GC) OptionalParameter * [_num_optional_parameters];
          unsigned long i = 0;
          while (tail != NIL)
            {
              Value parameter = xcar(tail);
              tail = xcdr(tail);
              if (parameter == S_and_environment)
                {
                  _envvar = xcar(tail);
                  if (!symbolp(_envvar))
                    signal_type_error(_envvar, S_symbol);
                  tail = xcdr(tail);
                  _maxargs = CALL_ARGUMENTS_LIMIT; // REVIEW
                  continue;
                }
              if (parameter == S_and_rest)
                {
                  state = STATE_REST;
                  break;
                }
              if (parameter == S_and_key)
                {
                  state = STATE_KEYWORD;
                  break;
                }
              if (parameter == S_and_aux)
                {
                  state = STATE_AUX;
                  break;
                }
              OptionalParameter * op = new OptionalParameter();
              if (symbolp(parameter))
                {
                  op->_var = parameter;
                  op->_initform = NIL;
                  op->_initval = NIL;
                  op->_supplied_p_var = NIL;
                }
              else if (consp(parameter))
                {
                  switch (length(parameter))
                    {
                    case 0:
                      invalid_parameter(parameter);
                      break;
                    case 1:
                      {
                        op->_var = xcar(parameter);
                        op->_initform = NIL;
                        op->_initval = NIL;
                        op->_supplied_p_var = NIL;
                        break;
                      }
                    case 2:
                      {
                        op->_var = xcar(parameter);
                        Value initform = xcar(xcdr(parameter));
                        op->_initform = initform;
                        Value initval = process_initform(initform);
                        if (initval == NULL_VALUE)
                          _non_constant_initform_p = true;
                        op->_initval = initval;
                        op->_supplied_p_var = NIL;
                        break;
                      }
                    case 3:
                      {
                        op->_var = xcar(parameter);
                        Value initform = xcar(xcdr(parameter));
                        op->_initform = initform;
                        Value initval = process_initform(initform);
                        if (initval == NULL_VALUE)
                          _non_constant_initform_p = true;
                        op->_initval = initval;
                        op->_supplied_p_var = xcar(xcdr(xcdr(parameter)));
                        break;
                      }
                    default:
                      invalid_parameter(parameter);
                      break;
                    }
                }
              else
                invalid_parameter(parameter);
              _op[i++] = op;
              ++_maxargs;
            }
        }
      if (state == STATE_REST)
        {
          if (tail != NIL)
            {
              _restvar = car(tail);
              if (!symbolp(_restvar))
                signal_type_error(_restvar, S_symbol);
              _maxargs = CALL_ARGUMENTS_LIMIT; // REVIEW
              tail = xcdr(tail);
              if (car(tail) == S_and_environment)
                {
                  tail = cdr(tail);
                  _envvar = car(tail);
                  if (!symbolp(_envvar))
                    signal_type_error(_envvar, S_symbol);
                  tail = cdr(tail);
                  _maxargs = CALL_ARGUMENTS_LIMIT; // REVIEW
                }
              if (car(tail) == S_and_key)
                {
                  state = STATE_KEYWORD;
                  tail = cdr(tail);
                }
            }
          else
            signal_lisp_error("&REST must be followed by a variable.");
        }
      if (state == STATE_KEYWORD)
        {
          _maxargs = CALL_ARGUMENTS_LIMIT; // REVIEW

          // Count the keyword parameters.
          assert(_num_keyword_parameters == 0);
          Value list = tail;
          while (list != NIL)
            {
              Value parameter = car(list);
              if (parameter == S_and_allow_other_keys)
                _allow_other_keys = true;
              else if (parameter == S_and_aux)
                break;
              else
                ++_num_keyword_parameters;
              list = xcdr(list);
            }

          _kp = new (GC) KeywordParameter * [_num_keyword_parameters];
          unsigned long i = 0;
          while (tail != NIL)
            {
              Value parameter = car(tail);
              tail = xcdr(tail);
              if (parameter == S_and_allow_other_keys)
                continue;
              if (parameter == S_and_environment)
                {
                  _envvar = car(tail);
                  if (!symbolp(_envvar))
                    signal_type_error(_envvar, S_symbol);
                  tail = cdr(tail);
                  _maxargs = CALL_ARGUMENTS_LIMIT; // REVIEW
                  continue;
                }
              if (parameter == S_and_aux)
                {
                  state = STATE_AUX;
                  break;
                }
              KeywordParameter * kp = new KeywordParameter();
              if (symbolp(parameter))
                {
                  kp->_keyword = PACKAGE_KEYWORD->intern(the_symbol(parameter)->name(), true);
                  kp->_var = parameter;
                  kp->_initform = NIL;
                  kp->_initval = NIL;
                  kp->_supplied_p_var = NIL;
                  assert(i < _num_keyword_parameters);
                }
              else if (consp(parameter))
                {
                  const unsigned long len = ::length(parameter);
                  if (len > 3)
                    invalid_parameter(parameter);
                  Value varspec = xcar(parameter);
                  if (symbolp(varspec))
                    {
                      kp->_keyword = PACKAGE_KEYWORD->intern(the_symbol(varspec)->name(), true);
                      kp->_var = varspec;
                    }
                  else if (consp(varspec) && ::length(varspec) == 2
                           && symbolp(xcar(varspec))  && symbolp(CL_cadr(varspec)))
                    {
                      kp->_keyword = xcar(varspec);
                      kp->_var = CL_cadr(varspec);
                    }
                  else
                    invalid_parameter(parameter);
                  Value initform = CL_cadr(parameter);
                  kp->_initform = initform;
                  Value initval = process_initform(initform);
                  if (initval == NULL_VALUE)
                    _non_constant_initform_p = true;
                  kp->_initval = initval;
                  if (len == 3)
                    {
                      kp->_supplied_p_var = CL_caddr(parameter);
                      if (!symbolp(kp->_supplied_p_var)) // supplied-p
                        invalid_parameter(parameter);
                    }
                  else
                    kp->_supplied_p_var = NIL;
                }
              else
                invalid_parameter(parameter);
              _kp[i++] = kp;
            }
        }
      if (state == STATE_AUX)
        {
          _maxargs = CALL_ARGUMENTS_LIMIT; // FIXME

          while (tail != NIL)
            {
              Value parameter = car(tail);
              tail = xcdr(tail);
              if (consp(parameter) && symbolp(xcar(parameter)))
                ; // OK
              else if (symbolp(parameter))
                ; // OK
              else
                invalid_parameter(parameter);
              _auxvars = make_cons(parameter, _auxvars);
            }
          _auxvars = CL_nreverse(_auxvars);
        }

      // done
      if (_minargs == _maxargs)
        _arity = _maxargs;
      else
        _arity = -1;
    }

  // process declarations
  _specials = NIL;
  if (_decls != NIL)
    {
      Value decls = _decls;
      while (decls != NIL)
        {
          Value obj = car(decls);
          if (consp(obj) && xcar(obj) == S_declare)
            {
              Value forms = xcdr(obj);
              while (forms != NIL)
                {
                  Value decl = car(forms);
                  if (consp(decl) && xcar(decl) == S_special)
                    {
                      Value names = xcdr(decl);
                      while (names != NIL)
                        {
                          _specials = make_cons(car(names), _specials);
                          names = xcdr(names);
                        }
                    }
                  forms = xcdr(forms);
                }
              decls = xcdr(decls);
            }
          else
            break;
        }
    }

  if (_arity > 0)
    {
      _required_parameters = new (GC) Value[_arity];
      Value list = _lambda_list;
      assert(length(list) == (unsigned long) _arity);
      for (long i = 0; i < _arity; i++)
        {
          _required_parameters[i] = xcar(list);
          list = xcdr(list);
        }
    }

  initialize_lambda_list_names();
}

void Closure::initialize_lambda_list_names()
{
  Value list = NIL;
  if (_minargs > 0)
    {
      Value lambda_list = _lambda_list;
      for (unsigned int i = 0; i < _minargs; i++)
        {
          Value name = xcar(lambda_list);
          list = make_cons(name, list);
          lambda_list = xcdr(lambda_list);
        }
    }
  if (_num_optional_parameters > 0)
    {
      for (unsigned int i = 0; i < _num_optional_parameters; i++)
        {
          list = make_cons(_op[i]->_var, list);
          if (_op[i]->_supplied_p_var != NIL)
            list = make_cons(_op[i]->_supplied_p_var, list);
        }
    }
  if (_restvar != NIL)
    list = make_cons(_restvar, list);
  if (_num_keyword_parameters > 0)
    {
      for (unsigned int i = 0; i < _num_keyword_parameters; i++)
        {
          list = make_cons(_kp[i]->_var, list);
          if (_kp[i]->_supplied_p_var != NIL)
            list = make_cons(_kp[i]->_supplied_p_var, list);
        }
    }
  _lambda_list_names = CL_nreverse(list);
}

void Closure::bind(Value name, Value value, Environment * env)
{
  if (the_symbol(name)->is_special_variable() || memq(name, _specials))
    current_thread()->bind_special(name, value);
  else
    env->bind(name, value);
}

Value Closure::execute()
{
  if (_arity == 0)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[0];
      return execute(0, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 0, _minargs, _maxargs);
}

Value Closure::execute(Value arg)
{
  if (_arity == 1)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      bind(_required_parameters[0], arg, ext);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[1];
      args[0] = arg;
      return execute(1, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 1, _minargs, _maxargs);
}

Value Closure::execute(Value arg1, Value arg2)
{
  if (_arity == 2)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      bind(_required_parameters[0], arg1, ext);
      bind(_required_parameters[1], arg2, ext);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[2];
      args[0] = arg1;
      args[1] = arg2;
      return execute(2, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 2, _minargs, _maxargs);
}

Value Closure::execute(Value arg1, Value arg2, Value arg3)
{
  if (_arity == 3)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      bind(_required_parameters[0], arg1, ext);
      bind(_required_parameters[1], arg2, ext);
      bind(_required_parameters[2], arg3, ext);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[3];
      args[0] = arg1;
      args[1] = arg2;
      args[2] = arg3;
      return execute(3, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 3, _minargs, _maxargs);
}

Value Closure::execute(Value arg1, Value arg2, Value arg3, Value arg4)
{
  if (_arity == 4)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      bind(_required_parameters[0], arg1, ext);
      bind(_required_parameters[1], arg2, ext);
      bind(_required_parameters[2], arg3, ext);
      bind(_required_parameters[3], arg4, ext);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[4];
      args[0] = arg1;
      args[1] = arg2;
      args[2] = arg3;
      args[3] = arg4;
      return execute(4, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 4, _minargs, _maxargs);
}

Value Closure::execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  if (_arity == 5)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      bind(_required_parameters[0], arg1, ext);
      bind(_required_parameters[1], arg2, ext);
      bind(_required_parameters[2], arg3, ext);
      bind(_required_parameters[3], arg4, ext);
      bind(_required_parameters[4], arg5, ext);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[5];
      args[0] = arg1;
      args[1] = arg2;
      args[2] = arg3;
      args[3] = arg4;
      args[4] = arg5;
      return execute(5, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 5, _minargs, _maxargs);
}

Value Closure::execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  if (_arity == 6)
    {
      Environment * ext = new Environment(_env);
      ext->declare_specials(_specials);
      bind(_required_parameters[0], arg1, ext);
      bind(_required_parameters[1], arg2, ext);
      bind(_required_parameters[2], arg3, ext);
      bind(_required_parameters[3], arg4, ext);
      bind(_required_parameters[4], arg5, ext);
      bind(_required_parameters[5], arg6, ext);
      return CL_progn(_body, ext, current_thread());
    }
  else if (_arity < 0)
    {
      Value args[6];
      args[0] = arg1;
      args[1] = arg2;
      args[2] = arg3;
      args[3] = arg4;
      args[4] = arg5;
      args[5] = arg6;
      return execute(6, args);
    }
  else
    return wrong_number_of_arguments(make_value(this), 6, _minargs, _maxargs);
}

Value Closure::execute(unsigned int numargs, Value args[])
{
  if ((_minargs > 0 && numargs < _minargs) || (_maxargs > 0 && numargs > _maxargs))
    return wrong_number_of_arguments(make_value(this), numargs, _minargs, _maxargs);

  Thread * thread = current_thread();
  Environment * ext = new Environment(_env);

  ext->declare_specials(_specials);

  if (_envvar != NIL)
    bind(_envvar, make_value(_env), ext);

  Value * vals = (Value *) alloca(::length(_lambda_list_names) * sizeof(Value));
  process_args(numargs, args, vals);

  Value vars = _lambda_list_names;
  INDEX end = ::length(_lambda_list_names);
  for (INDEX index = 0; index < end; index++)
    {
      bind(car(vars), vals[index], ext);
      vars = xcdr(vars);
    }

  // &aux vars
  if (_auxvars != NIL)
    {
      Value auxvars = _auxvars;
      do
        {
          Value auxvar = car(auxvars);
          if (consp(auxvar))
            {
              Value var = xcar(auxvar);
              Value value = eval(car(xcdr(auxvar)), ext, thread);
              bind(var, value, ext);
            }
          else
            bind(auxvar, NIL, ext);
          auxvars = xcdr(auxvars);
        }
      while (auxvars != NIL);
    }

  return progn(_body, ext, thread);
}

Value * Closure::process_args(unsigned int numargs, Value args[], Value vals[])
{
  Thread * const thread = current_thread();

  Environment * temp;
  if (_non_constant_initform_p || _auxvars != NIL)
    temp = new Environment(_env);
  else
    temp = NULL;

  unsigned int i = 0;
  unsigned int args_used = 0;

  // required parameters
  Value lambda_list = _lambda_list;
  while (args_used < _minargs)
    {
      Value var = xcar(lambda_list);
      assert(var == CL_elt(_lambda_list_names, make_fixnum(i)));
      if (temp)
        bind(var, args[args_used], temp);
      vals[i] = args[args_used];
      ++i;
      ++args_used;
      lambda_list = xcdr(lambda_list);
    }

  if (_arity < 0)
    {
      // optional parameters
      if (_num_optional_parameters != 0)
        {
          for (unsigned int j = 0; j < _num_optional_parameters; j++)
            {
              OptionalParameter * op = _op[j];
              Value var = op->_var;
              Value value;
              Value supplied_p;
              if (args_used < numargs)
                {
                  value = args[args_used];
                  ++args_used;
                  supplied_p = T;
                }
              else
                {
                  // not supplied
                  if (op->_initval != NULL_VALUE)
                    value = op->_initval;
                  else
                    value = eval(op->_initform, temp, thread);
                  supplied_p = NIL;
                }
              assert(var == CL_elt(_lambda_list_names, make_fixnum(i)));
              if (temp)
                bind(var, value, temp);
              vals[i] = value;
              ++i;
              if (op->_supplied_p_var != NIL)
                {
                  assert(op->_supplied_p_var == CL_elt(_lambda_list_names, make_fixnum(i)));
                  if (temp)
                    bind(op->_supplied_p_var, supplied_p, temp);
                  vals[i] = supplied_p;
                  ++i;
                }
            }
        }

      // &rest
      if (_restvar != NIL)
        {
          assert(_restvar == CL_elt(_lambda_list_names, make_fixnum(i)));
          Value restval = NIL;
          for (unsigned int j = numargs; j-- > args_used;)
            restval = make_cons(args[j], restval);
          if (temp)
            bind(_restvar, restval, temp);
          vals[i] = restval;
          ++i;
        }

      Value allow_other_keys_value = NULL_VALUE;

      // keyword parameters
      if (_num_keyword_parameters > 0)
        {
          if (((numargs - args_used) % 2) != 0)
            {
              signal_lisp_error(new ProgramError("Odd number of keyword arguments."));
              // Not reached.
              return NULL;
            }

          for (unsigned int j = 0; j < _num_keyword_parameters; j++)
            {
              KeywordParameter * kp = _kp[j];
              Value var = kp->_var;
              Value supplied_p_var = kp->_supplied_p_var;
              Value value = NULL_VALUE;
              Value supplied_p = NIL;
              for (unsigned int k = args_used; k < numargs; k += 2)
                {
                  if (args[k] == kp->_keyword)
                    {
                      value = args[k + 1];
                      supplied_p = T;
                      break;
                    }
                }
              if (value == NULL_VALUE)
                {
                  if (kp->_initval != NULL_VALUE)
                    value = kp->_initval;
                  else
                    value = eval(kp->_initform, temp, thread);
                }
              if (temp)
                {
                  bind(var, value, temp);
                  if (supplied_p_var != NIL)
                    bind(supplied_p_var, supplied_p, temp);
                }
              assert(var == CL_elt(_lambda_list_names, make_fixnum(i)));
              vals[i] = value;
              ++i;
              if (supplied_p_var != NIL)
                {
                  assert(supplied_p_var == CL_elt(_lambda_list_names, make_fixnum(i)));
                  vals[i] = supplied_p;
                  ++i;
                }
              if (kp->_keyword == K_allow_other_keys)
                {
                  if (allow_other_keys_value == NULL_VALUE)
                    allow_other_keys_value = value;
                }
            }
        }

      assert(i == ::length(_lambda_list_names));

      if (_restvar == NIL && numargs > args_used)
        {
          bool allow_other_keys = _allow_other_keys;
          if (!allow_other_keys)
            {
              if (allow_other_keys_value != NULL_VALUE && allow_other_keys_value != NIL)
                allow_other_keys = true;
            }

          Value unrecognized = NULL_VALUE;
          for (unsigned int k = args_used; k < numargs; k += 2)
            {
              Value keyword = args[k];
              if (keyword == NULL_VALUE)
                continue;
              if (keyword == K_allow_other_keys)
                {
                  if (allow_other_keys_value == NULL_VALUE)
                    {
                      allow_other_keys_value = args[k + 1];
                      if (allow_other_keys_value != NIL)
                        {
                          allow_other_keys = true;
                          break;
                        }
                    }
                  continue;
                }
              // unused keyword argument
              if (unrecognized == NULL_VALUE)
                {
                  // check for repeat key
                  bool ok = false;
                  for (long k = _num_keyword_parameters; k-- > 0;)
                    {
                      if (_kp[k]->_keyword == keyword)
                        {
                          // found it!
                          ok = true;
                          break;
                        }
                    }
                  if (!ok)
                    unrecognized = keyword;
                }
            }
          if (!allow_other_keys && unrecognized != NULL_VALUE)
            {
              String * s = new String("Unrecognized keyword argument ");
              s->append(::prin1_to_string(unrecognized));
              signal_lisp_error(new ProgramError(s));
              // not reached
              return NULL;
            }
        }
    }

  return vals;
}

AbstractString * Closure::write_to_string()
{
  Value name = operator_name();
  if (current_thread()->symbol_value(S_print_readably) != NIL)
    {
      if (symbolp(name) || is_valid_setf_function_name(name))
        {
          String * s = new String();
          s->append("#.(");
          s->append(the_symbol(S_coerce_to_function)->prin1_to_string());
          s->append(" '");
          s->append(::prin1_to_string(
            name));
          s->append_char(')');
          return s;
        }
      signal_lisp_error(new PrintNotReadable(make_value(this)));
      // not reached
      return NULL;
    }
  String * s = new String();
  s->append(the_symbol(S_function)->write_to_string());
  if (name != NULL_VALUE)
    {
      s->append_char(' ');
      s->append(::prin1_to_string(name));
    }
  else
    {
      s->append_char(' ');
      s->append(::prin1_to_string(list2(S_lambda, _lambda_list)));
    }
  return unreadable_string(s);
}

// ### closurep
// REVIEW
Value EXT_closurep(Value arg)
{
  return closurep(arg) ? T : NIL;
}

// ### closure-environment
// REVIEW
Value EXT_closure_environment(Value arg)
{
  Environment * env = check_closure(arg)->environment();
  return env ? make_value(env) : NIL;
}

// ### lambda-list-names lambda-list => names
Value SYS_lambda_list_names(Value arg)
{
  Closure * closure = new Closure(list3(S_lambda, arg, NIL), NULL);
  return closure->lambda_list_names();
}
