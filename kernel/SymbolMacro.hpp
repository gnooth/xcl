// SymbolMacro.hpp
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

#ifndef __SYMBOL_MACRO_HPP
#define __SYMBOL_MACRO_HPP

class SymbolMacro : public TypedObject
{
private:
  Value _expansion;

public:
  SymbolMacro(Value expansion)
    : TypedObject(WIDETAG_SYMBOL_MACRO), _expansion(expansion)
  {
  }

  Value expansion() const { return _expansion; }
};

inline bool symbol_macro_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SYMBOL_MACRO);
}

inline SymbolMacro * the_symbol_macro(Value value)
{
  assert(symbol_macro_p(value));
  return reinterpret_cast<SymbolMacro *>(value - LOWTAG_TYPED_OBJECT);
}

#endif // SymbolMacro.hpp
