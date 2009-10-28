// ArithmeticError.hpp
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

#ifndef __ARITHMETIC_ERROR_HPP
#define __ARITHMETIC_ERROR_HPP

class ArithmeticError : public Condition
{
private:
  static Layout * get_layout_for_class();

public:
  ArithmeticError()
    : Condition(WIDETAG_CONDITION, get_layout_for_class())
  {
  }

  ArithmeticError(Value operation, Value operands);

  virtual void initialize(Value initargs);

  virtual Value type_of() const
  {
    return S_arithmetic_error;
  }

  virtual Value class_of() const
  {
    return C_arithmetic_error;
  }

  virtual bool typep(Value type) const;
};

#endif
