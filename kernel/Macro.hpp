// Macro.hpp
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

#ifndef __MACRO_HPP
#define __MACRO_HPP

class Macro : public Function
{
private:
  Function * _expansion_function;

public:
  Macro(Value name, Function * expansion_function)
    : Function(WIDETAG_MACRO, name), _expansion_function(expansion_function)
  {
    assert(expansion_function != NULL);
    expansion_function->set_operator_name(list2(S_macro_function, name));
  }

  Function * expansion_function() const { return _expansion_function; }

  virtual Value execute(Value arg1, Value arg2)
  {
    assert(_expansion_function != NULL);
    return _expansion_function->execute(arg1, arg2);
  }

  virtual String * write_to_string()
  {
    String * string = new String("#<");
    string->append(the_symbol(S_macro)->prin1_to_string());
    Value name = operator_name();
    if (name != NULL_VALUE)
      {
        string->append_char(' ');
        string->append(::prin1_to_string(name));
      }
    char buf[256];
    SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
    string->append(buf);
    return string;
  }

};

inline bool macrop(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_MACRO);
}

inline Macro * the_macro(Value value)
{
  assert(macrop(value));
  return reinterpret_cast<Macro *>(value - LOWTAG_TYPED_OBJECT);
}

#endif // Macro.hpp
