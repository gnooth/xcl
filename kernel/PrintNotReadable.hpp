// PrintNotReadable.hpp
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

#ifndef __PRINT_NOT_READABLE_HPP
#define __PRINT_NOT_READABLE_HPP

class PrintNotReadable : public Condition
{
private:
  static Layout * get_layout_for_class();

public:
  PrintNotReadable()
    : Condition(WIDETAG_CONDITION, get_layout_for_class())
  {
    set_slot_value(S_object, NIL);
  }

  PrintNotReadable(Value object)
    : Condition(WIDETAG_CONDITION, get_layout_for_class())
  {
    set_slot_value(S_object, object);
  }

  void initialize(Value initargs);

  virtual Value type_of() const
  {
    return S_print_not_readable;
  }

  virtual Value class_of() const
  {
    return C_print_not_readable;
  }

  virtual bool typep(Value type) const;

  virtual AbstractString * write_to_string();
};

#endif // PrintNotReadable.hpp
