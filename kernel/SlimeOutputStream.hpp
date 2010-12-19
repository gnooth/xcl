// SlimeOutputStream.hpp
//
// Copyright (C) 2009-2010 Peter Graves <gnooth@gmail.com>
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

#ifndef __SLIME_OUTPUT_STREAM_HPP
#define __SLIME_OUTPUT_STREAM_HPP

#include "Stream.hpp"

class SlimeOutputStream : public AnsiStream
{
private:
  TypedObject * _function;
  String * _string;

public:
  SlimeOutputStream(TypedObject * function)
    : AnsiStream(WIDETAG_SLIME_OUTPUT_STREAM, DIRECTION_OUTPUT),
      _function(function), _string(new String())
  {
  }

  virtual Value type_of() const
  {
    return S_slime_output_stream;
  }

  virtual Value class_of() const
  {
    return C_slime_output_stream;
  }

  virtual bool typep(Value type) const;

  virtual Value close()
  {
    _open = false;
    return T;
  }

  virtual void write_char(BASE_CHAR c)
  {
    _string->append_char(c);
  }

  virtual void finish_output()
  {
    if (_string->length() > 0)
      {
        current_thread()->execute(_function, make_value(_string));
        _string->set_length(0);
      }
  }
};

#endif
