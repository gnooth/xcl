// Readtable.hpp
//
// Copyright (C) 2006-2008 Peter Graves <peter@armedbear.org>
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

#ifndef __READTABLE_HPP
#define __READTABLE_HPP

#define SYNTAX_TYPE_CONSTITUENT                 0
#define SYNTAX_TYPE_WHITESPACE                  1
#define SYNTAX_TYPE_TERMINATING_MACRO           2
#define SYNTAX_TYPE_NON_TERMINATING_MACRO       3
#define SYNTAX_TYPE_SINGLE_ESCAPE               4
#define SYNTAX_TYPE_MULTIPLE_ESCAPE             5

class DispatchTable : public gc // REVIEW
{
public:
  Value * _functions;

  DispatchTable()
  {
    _functions = (Value *) GC_malloc(CHAR_CODE_LIMIT * sizeof(Value));
    for (int i = 0; i < CHAR_CODE_LIMIT; i++)
      _functions[i] = NULL_VALUE;
  }

  DispatchTable(DispatchTable * dt)
  {
    _functions = (Value *) GC_malloc(CHAR_CODE_LIMIT * sizeof(Value));
    for (int i = 0; i < CHAR_CODE_LIMIT; i++)
      _functions[i] = dt->_functions[i];
  }
};

class Readtable : public TypedObject
{
private:
  void initialize();

protected:
//   byte[]          syntax               = new byte[CHAR_MAX];
//   LispObject[]    readerMacroFunctions = new LispObject[CHAR_MAX];
//   DispatchTable[] dispatchTables       = new DispatchTable[CHAR_MAX];
  unsigned char * _syntax;
  Value * _reader_macro_functions;
  DispatchTable * * _dispatch_tables;

  Value _readtable_case;

protected:
  Readtable(bool init_p) : TypedObject(WIDETAG_READTABLE)
  {
    if (init_p)
      initialize();
  }

public:
  Readtable() : TypedObject(WIDETAG_READTABLE)
  {
    initialize();
  }

  static void copy_readtable(Readtable * from, Readtable * to);

  Readtable(Readtable * rt);

  Value readtable_case() const
  {
    return _readtable_case;
  }

  void set_readtable_case(Value arg)
  {
    _readtable_case = arg;
  }

  virtual Value type_of() const
  {
    return S_readtable;
  }

  virtual Value class_of() const
  {
    return C_readtable;
  }

  virtual bool typep(Value type) const;

  unsigned int syntax(BASE_CHAR c) const
  {
    return _syntax[c];
  }

  void set_syntax(BASE_CHAR c, unsigned int syntax)
  {
    _syntax[c] = syntax;
  }

  Value reader_macro_function(BASE_CHAR c)
  {
    return _reader_macro_functions[c];
  }

  void set_reader_macro_function(BASE_CHAR c, Value function)
  {
    _reader_macro_functions[c] = function;
  }

  Value get_macro_character(BASE_CHAR c);

  void set_macro_character(BASE_CHAR c, Value function, Value non_terminating_p);

  void make_dispatch_macro_character(BASE_CHAR c, Value non_terminating_p);

  Value get_dispatch_macro_character(BASE_CHAR dispchar, BASE_CHAR subchar);

  void set_dispatch_macro_character(BASE_CHAR dispchar, BASE_CHAR subchar, Value function);

  bool is_whitespace(BASE_CHAR c)
  {
    return _syntax[c] == SYNTAX_TYPE_WHITESPACE;
  }

  void check_invalid(BASE_CHAR c, Stream * stream);

  void set_syntax_from_char(BASE_CHAR from_char, BASE_CHAR to_char, Readtable * rt);
};

inline bool readtablep(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_READTABLE);
}

inline Readtable * the_readtable(Value value)
{
  assert(readtablep(value));
  return reinterpret_cast<Readtable *>(value - LOWTAG_TYPED_OBJECT);
}

inline Readtable * check_readtable(Value value)
{
  if (readtablep(value))
    return the_readtable(value);
  signal_type_error(value, S_readtable);
  // Not reached.
  return NULL;
}

#endif // Readtable.hpp
