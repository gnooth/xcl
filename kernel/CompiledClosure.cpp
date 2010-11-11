// CompiledClosure.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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
#include "primitives.hpp"
#include "CompiledFunction.hpp"
#include "Primitive.hpp"
#include "SimpleArray_UB8_1.hpp"

class ClosureTemplateFunction : public CompiledFunction
{
public:
  ClosureTemplateFunction(Value name, void * code, long minargs, long maxargs, Value constants)
    : CompiledFunction(name, code, 0, minargs, maxargs, constants)
  {
    assert(widetag() == WIDETAG_COMPILED_FUNCTION);
  }

  virtual Value execute(Value * data);
  virtual Value execute(Value * data, Value arg);
  virtual Value execute(Value * data, Value arg1, Value arg2);
  virtual Value execute(Value * data, Value arg1, Value arg2, Value arg3);
  virtual Value execute(Value * data, Value arg1, Value arg2, Value arg3, Value arg4);
  virtual Value execute(Value * data, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);
  virtual Value execute(Value * data, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6);
  virtual Value execute(Value * data, unsigned int numargs, Value args[]);

  virtual AbstractString * write_to_string();
};

Value ClosureTemplateFunction::execute(Value * data)
{
  if (_arity == 0)
    {
      Value (*code) (Value *) = (Value (*) (Value *)) _code;
      return (*code)(data);
    }
    return wrong_number_of_arguments(operator_name(), 0, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, Value arg)
{
  if (_arity == 1)
    {
      Value (*code) (Value *, Value) = (Value (*) (Value *, Value)) _code;
      return (*code)(data, arg);
    }
  return wrong_number_of_arguments(operator_name(), 1, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, Value arg1, Value arg2)
{
  if (_arity == 2)
    {
      Value (*code) (Value *, Value, Value) = (Value (*) (Value *, Value, Value)) _code;
      return (*code)(data, arg1, arg2);
    }
  return wrong_number_of_arguments(operator_name(), 2, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, Value arg1, Value arg2, Value arg3)
{
  if (_arity == 3)
    {
      Value (*code) (Value *, Value, Value, Value) = (Value (*) (Value *, Value, Value, Value)) _code;
      return (*code)(data, arg1, arg2, arg3);
    }
  return wrong_number_of_arguments(operator_name(), 3, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, Value arg1, Value arg2, Value arg3, Value arg4)
{
  if (_arity == 4)
    {
      Value (*code) (Value *, Value, Value, Value, Value) = (Value (*) (Value *, Value, Value, Value, Value)) _code;
      return (*code)(data, arg1, arg2, arg3, arg4);
    }
  return wrong_number_of_arguments(operator_name(), 4, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  if (_arity == 5)
    {
      Value (*code) (Value *, Value, Value, Value, Value, Value) = (Value (*) (Value *, Value, Value, Value, Value, Value)) _code;
      return (*code)(data, arg1, arg2, arg3, arg4, arg5);
    }
  return wrong_number_of_arguments(operator_name(), 5, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
{
  if (_arity == 6)
    {
      Value (*code) (Value *, Value, Value, Value, Value, Value, Value) = (Value (*) (Value *, Value, Value, Value, Value, Value, Value)) _code;
      return (*code)(data, arg1, arg2, arg3, arg4, arg5, arg6);
    }
  return wrong_number_of_arguments(operator_name(), 6, _minargs, _maxargs);
}

Value ClosureTemplateFunction::execute(Value * data, unsigned int numargs, Value args[])
{
  assert(_minargs >= 0);
  assert(_minargs < CALL_ARGUMENTS_LIMIT);
  assert(_maxargs >= 0);
  assert(_maxargs < CALL_ARGUMENTS_LIMIT);
  assert(numargs < CALL_ARGUMENTS_LIMIT);
  if (_minargs <= numargs && numargs <= _maxargs)
    {
      Value (*code) (Value *, unsigned int, Value *) = (Value (*) (Value *, unsigned int, Value *)) _code;
      return (*code)(data, numargs, args);
    }
  return wrong_number_of_arguments(operator_name(), numargs, _minargs, _maxargs);
}

AbstractString * ClosureTemplateFunction::write_to_string()
{
  String * string = new String(the_symbol(S_closure_template_function)->prin1_to_string());
  Value name = operator_name();
  if (name != NULL_VALUE)
    {
      string->append_char(' ');
      string->append(::prin1_to_string(name));
    }
  return unreadable_string(string);
}

// ### make-closure-template-function name code minargs maxargs constants => ctf
Value SYS_make_closure_template_function(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
{
  SimpleArray_UB8_1 * vector = check_simple_array_ub8_1(arg2);
  long minargs = fixnum_value(arg3);
  long maxargs = fixnum_value(arg4);
  return make_value(new ClosureTemplateFunction(arg1, vector->data(), minargs, maxargs, arg5));
}

class CompiledClosure : public Function
{
private:
  ClosureTemplateFunction * _ctf;
  Value * _data;

public:
  CompiledClosure(ClosureTemplateFunction * ctf, Value * data)
    : Function(WIDETAG_COMPILED_CLOSURE, NIL), _ctf(ctf), _data(data)
  {
    assert(ctf->widetag() == WIDETAG_COMPILED_FUNCTION);
    assert(ctf->arity() >= -1);
    _arity = ctf->arity();
    assert(ctf->minargs() >= 0);
    assert(ctf->minargs() < CALL_ARGUMENTS_LIMIT);
    _minargs = ctf->minargs();
    assert(ctf->maxargs() >= 0);
    assert(ctf->maxargs() < CALL_ARGUMENTS_LIMIT);
    _maxargs = ctf->maxargs();
  }

  ClosureTemplateFunction * ctf() const
  {
    return _ctf;
  }

  virtual Value execute()
  {
    if (_arity == 0)
      return _ctf->execute(_data);
    if (_arity < 0)
      {
        Value args[0];
        return _ctf->execute(_data, 0, args);
      }
    return wrong_number_of_arguments(operator_name(), 0, _minargs, _maxargs);
  }

  virtual Value execute(Value arg)
  {
    if (_arity == 1)
      return _ctf->execute(_data, arg);
    if (_arity < 0)
      {
        Value args[1];
        args[0] = arg;
        return _ctf->execute(_data, 1, args);
      }
    return wrong_number_of_arguments(operator_name(), 1, _minargs, _maxargs);
  }

  virtual Value execute(Value arg1, Value arg2)
  {
    if (_arity == 2)
      return _ctf->execute(_data, arg1, arg2);
    if (_arity < 0)
      {
        Value args[2];
        args[0] = arg1;
        args[1] = arg2;
        return _ctf->execute(_data, 2, args);
      }
    return wrong_number_of_arguments(operator_name(), 2, _minargs, _maxargs);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3)
  {
    if (_arity == 3)
      return _ctf->execute(_data, arg1, arg2, arg3);
    if (_arity < 0)
      {
        Value args[3];
        args[0] = arg1;
        args[1] = arg2;
        args[2] = arg3;
        return _ctf->execute(_data, 3, args);
      }
    return wrong_number_of_arguments(operator_name(), 3, _minargs, _maxargs);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4)
  {
    if (_arity == 4)
      return _ctf->execute(_data, arg1, arg2, arg3, arg4);
    if (_arity < 0)
      {
        Value args[4];
        args[0] = arg1;
        args[1] = arg2;
        args[2] = arg3;
        args[3] = arg4;
        return _ctf->execute(_data, 4, args);
      }
    return wrong_number_of_arguments(operator_name(), 4, _minargs, _maxargs);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5)
  {
    if (_arity == 5)
      return _ctf->execute(_data, arg1, arg2, arg3, arg4, arg5);
    if (_arity < 0)
      {
        Value args[5];
        args[0] = arg1;
        args[1] = arg2;
        args[2] = arg3;
        args[3] = arg4;
        args[4] = arg5;
        return _ctf->execute(_data, 5, args);
      }
    return wrong_number_of_arguments(operator_name(), 5, _minargs, _maxargs);
  }

  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6)
  {
    if (_arity == 6)
      return _ctf->execute(_data, arg1, arg2, arg3, arg4, arg5, arg6);
    if (_arity < 0)
      {
        Value args[6];
        args[0] = arg1;
        args[1] = arg2;
        args[2] = arg3;
        args[3] = arg4;
        args[4] = arg5;
        args[5] = arg6;
        return _ctf->execute(_data, 6, args);
      }
    return wrong_number_of_arguments(operator_name(), 6, _minargs, _maxargs);
  }

  virtual Value execute(unsigned int numargs, Value args[])
  {
    assert(_minargs >= 0);
    assert(_minargs < CALL_ARGUMENTS_LIMIT);
    assert(_maxargs >= 0);
    assert(_maxargs < CALL_ARGUMENTS_LIMIT);
    assert(numargs < CALL_ARGUMENTS_LIMIT);
    return _ctf->execute(_data, numargs, args);
  }

  virtual Value parts();

  virtual AbstractString * write_to_string();
};

Value CompiledClosure::parts()
{
  String * description = new String(prin1_to_string());
  description->append_char('\n');
  Value elements = NIL;
  Value name = operator_name();
  elements = make_cons(make_cons(make_simple_string("NAME"),
                                 name != NULL_VALUE ? name : NIL),
                       elements);
  elements = make_cons(make_cons(make_simple_string("ARITY"),
                                 make_fixnum(arity())),
                       elements);
  elements = make_cons(make_cons(make_simple_string("MINARGS"),
                                 make_fixnum(minargs())),
                       elements);
  elements = make_cons(make_cons(make_simple_string("MAXARGS"),
                                 make_fixnum(maxargs())),
                       elements);
  elements = make_cons(make_cons(make_simple_string("TEMPLATE"),
                                 make_value(ctf())),
                       elements);
  return current_thread()->set_values(make_value(description), T, CL_nreverse(elements));
}

AbstractString * CompiledClosure::write_to_string()
{
  String * string = new String("#<");
  string->append(the_symbol(S_compiled_closure)->write_to_string());
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  string->append(buf);
  return string;
}

Value make_compiled_closure(Value template_function, Value * data)
{
  assert(lowtag_of(template_function) == LOWTAG_TYPED_OBJECT);
  assert(the_typed_object(template_function)->widetag() == WIDETAG_COMPILED_FUNCTION);
  ClosureTemplateFunction * ctf =
    reinterpret_cast<ClosureTemplateFunction *>(template_function - LOWTAG_TYPED_OBJECT);
  return make_value(new CompiledClosure(ctf, data));
}

// ### function-code
Value SYS_function_code(Value arg)
{
  if (typed_object_p(arg))
    {
      long widetag = the_typed_object(arg)->widetag();
      // REVIEW
      if (widetag == WIDETAG_PRIMITIVE || widetag == WIDETAG_COMPILED_FUNCTION)
        {
          Primitive * primitive = reinterpret_cast<Primitive *>(arg - LOWTAG_TYPED_OBJECT);
          void * code = primitive->code();
          return make_unsigned_integer((unsigned long) code);
        }
      if (widetag == WIDETAG_COMPILED_CLOSURE)
        {
          CompiledClosure * cc = reinterpret_cast<CompiledClosure *>(arg - LOWTAG_TYPED_OBJECT);
          ClosureTemplateFunction * ctf = cc->ctf();
          void * code = ctf->code();
          return make_unsigned_integer((unsigned long) code);
        }
    }
  return NIL;
}

// ### function-code-size
Value SYS_function_code_size(Value arg)
{
  if (typed_object_p(arg))
    {
      long widetag = the_typed_object(arg)->widetag();
      // REVIEW
      if (widetag == WIDETAG_PRIMITIVE || widetag == WIDETAG_COMPILED_FUNCTION)
        {
          Primitive * primitive = reinterpret_cast<Primitive *>(arg - LOWTAG_TYPED_OBJECT);
          return primitive->code_size();
        }
      if (widetag == WIDETAG_COMPILED_CLOSURE)
        {
          CompiledClosure * cc = reinterpret_cast<CompiledClosure *>(arg - LOWTAG_TYPED_OBJECT);
          ClosureTemplateFunction * ctf = cc->ctf();
          return ctf->code_size();
        }
    }
  return NIL;
}

// ### set-function-code-size
Value SYS_set_function_code_size(Value arg1, Value arg2)
{
  if (typed_object_p(arg1))
    {
      long widetag = the_typed_object(arg1)->widetag();
      // REVIEW
      if (widetag == WIDETAG_PRIMITIVE || widetag == WIDETAG_COMPILED_FUNCTION)
        {
          Primitive * primitive = reinterpret_cast<Primitive *>(arg1 - LOWTAG_TYPED_OBJECT);
          primitive->set_code_size(arg2);
          return arg2;
        }
      if (widetag == WIDETAG_COMPILED_CLOSURE)
        {
          CompiledClosure * cc = reinterpret_cast<CompiledClosure *>(arg1 - LOWTAG_TYPED_OBJECT);
          ClosureTemplateFunction * ctf = cc->ctf();
          ctf->set_code_size(arg2);
        }
    }
  return signal_type_error(arg1, S_function);
}
