// Symbol.hpp
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

#ifndef __SYMBOL_HPP
#define __SYMBOL_HPP

class Package;
class Operator;
class SimpleString;
class TypedObject;

extern Value S_nil;

class Symbol : public LispObject
{
private:
  enum
  {
    // bit flags
    FLAG_SPECIAL_VARIABLE = 0x0001,
    FLAG_SPECIAL_OPERATOR = 0x0002,
    FLAG_MACRO            = 0x0004,
    FLAG_CONSTANT         = 0x0008,
    FLAG_AUTOLOAD         = 0x0010,
    FLAG_KERNEL_FUNCTION  = 0x0020
  };
  unsigned long _flags;
  unsigned long _hash;
#define SYMBOL_NAME_OFFSET    (BYTES_PER_WORD * 2) // +SYMBOL-NAME-OFFSET+
  SimpleString * const _name;
#define SYMBOL_PACKAGE_OFFSET (BYTES_PER_WORD * 3) // +SYMBOL-PACKAGE-OFFSET+
  Value _package;
#define SYMBOL_VALUE_OFFSET   (BYTES_PER_WORD * 4) // +SYMBOL-VALUE-OFFSET+
  Value _value;
  TypedObject * _function;
  Value _plist;
  INDEX _binding_index;

public:
  Symbol(const char * name);
  Symbol(AbstractString * name);
  Symbol(const char * name, Package * package);
  Symbol(AbstractString * name, Package * package);

  SimpleString * name()
  {
    return _name;
  }

  Value package()
  {
    return _package;
  }

  void set_package(Value package)
  {
    _package = package;
  }

  unsigned long hash();

  unsigned long flags()
  {
    return _flags;
  }

  void set_flags(unsigned long flags)
  {
    _flags = flags;
  }

  Value value() { return _value; }

  void set_value(Value value) { _value = value; }

  void set_constant_value(Value value)
  {
    assert(_value == NULL_VALUE);
    _value = value;
    _flags |= FLAG_CONSTANT;
  }

  void initialize_constant(Value value)
  {
    _flags |= (FLAG_SPECIAL_VARIABLE | FLAG_CONSTANT);
    _value = value;
  }

  long is_constant() const
  {
    return _flags & FLAG_CONSTANT;
  }

  void initialize_special()
  {
     // no initial value
    _flags |= FLAG_SPECIAL_VARIABLE;
  }

  void initialize_special(Value value)
  {
    _flags |= FLAG_SPECIAL_VARIABLE;
    _value = value;
  }

  void proclaim_special()
  {
    _flags |= FLAG_SPECIAL_VARIABLE;
  }

  TypedObject * function()
  {
    return _function;
  }

  void set_function(TypedObject * function)
  {
    _function = function;
    _flags &= ~FLAG_SPECIAL_OPERATOR; // REVIEW
    _flags &= ~FLAG_MACRO;
    _flags &= ~FLAG_AUTOLOAD;
  }

  void set_autoload(TypedObject * function)
  {
    _function = function;
    _flags &= ~FLAG_SPECIAL_OPERATOR; // REVIEW
    _flags &= ~FLAG_MACRO;
    _flags |= FLAG_AUTOLOAD;
  }

  void set_autoload_macro(TypedObject * function)
  {
    if (_flags & FLAG_SPECIAL_OPERATOR)
      put(S_macro, make_value(function));
    else
      _function = function;
    _flags |= (FLAG_AUTOLOAD | FLAG_MACRO);
  }

  void set_macro_function(class Function * function);

  void set_special_operator(class SpecialOperator * special_operator);

  long is_macro() const
  {
    return _flags & FLAG_MACRO;
  }

  long is_special_operator() const
  {
    return _flags & FLAG_SPECIAL_OPERATOR;
  }

  long is_special_variable() const
  {
    return _flags & FLAG_SPECIAL_VARIABLE;
  }

  long is_autoload() const
  {
    return _flags & FLAG_AUTOLOAD;
  }

  bool is_autoload_macro()  const
  {
    return (_flags & (FLAG_AUTOLOAD | FLAG_MACRO)) == (FLAG_AUTOLOAD | FLAG_MACRO);
  }

  long is_kernel_function () const
  {
    return _flags & FLAG_KERNEL_FUNCTION;
  }

  void set_kernel_function(bool b)
  {
    if (b)
      _flags |= FLAG_KERNEL_FUNCTION;
    else
      _flags &= ~FLAG_KERNEL_FUNCTION;
  }

  void fmakunbound()
  {
    _function = NULL;;
//     _flags &= ~FLAG_SPECIAL_OPERATOR;
//     _flags &= ~FLAG_MACRO;
//     _flags &= ~FLAG_AUTOLOAD;
//     _flags &= ~FLAG_KERNEL_FUNCTION;
    _flags &= ~(FLAG_SPECIAL_OPERATOR | FLAG_MACRO | FLAG_AUTOLOAD | FLAG_KERNEL_FUNCTION);
  }

  Value plist()
  {
    if (_plist == NULL_VALUE)
      _plist = NIL;
    return _plist;
  }

  void set_plist(Value plist) { _plist = plist; }

  INDEX binding_index() const
  {
    return _binding_index;
  }

  INDEX assign_binding_index();

  Value get(Value indicator) const;

  void put(Value indicator, Value new_value);

  Value remprop(Value indicator);

  bool apropos(const char * s);

  AbstractString * princ_to_string();

  AbstractString * prin1_to_string();

  AbstractString * write_to_string();
};

inline Symbol * the_symbol(Value value)
{
  if (value == NIL)
    value = S_nil;
  assert(lowtag_of(value) == LOWTAG_SYMBOL);
  return (Symbol *) (value - LOWTAG_SYMBOL);
}

inline Value make_value(Symbol * symbol)
{
  if (symbol == (Symbol *) (S_nil - LOWTAG_SYMBOL))
    return NIL;
  return make_value(symbol, LOWTAG_SYMBOL);
}

inline bool symbolp(Value value)
{
  if (value == NIL)
    return T;
  return (value & LOWTAG_MASK) == LOWTAG_SYMBOL;
}

inline Symbol * check_symbol(Value value)
{
  if (symbolp(value))
    return the_symbol(value);
  signal_type_error(value, S_symbol);
  // not reached
  return NULL;
}

Value get(Value symbol, Value indicator);
Value get(Value symbol, Value indicator, Value default_value);
Value put(Value symbol, Value indicator, Value value);

#endif
