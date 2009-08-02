// SpecialOperator.hpp
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

#ifndef __SPECIALOPERATOR_HPP
#define __SPECIALOPERATOR_HPP

#include "PrintNotReadable.hpp"

class SpecialOperator : public Operator
{
private:
  SpecialOperatorFunction _pf;

public:
  SpecialOperator(Value name, SpecialOperatorFunction pf)
    : Operator(WIDETAG_SPECIAL_OPERATOR, name), _pf(pf)
  {
  }

  virtual Value type_of() const
  {
    return S_special_operator;
  }

  SpecialOperatorFunction pf()
  {
    return _pf;
  }

  virtual bool is_special_operator()
  {
    return true;
  }

  AbstractString * write_to_string()
  {
    Value name = operator_name();
    if (current_thread()->symbol_value(S_print_readably) != NIL)
      {
        signal_lisp_error(new PrintNotReadable(make_value(this)));
        // not reached
        return NULL;
      }
    String * s = new String();
    s->append(the_symbol(S_special_operator)->write_to_string());
    if (name != NULL_VALUE)
      {
        s->append_char(' ');
        s->append(::prin1_to_string(name));
      }
    return unreadable_string(s);
  }
};

#endif
