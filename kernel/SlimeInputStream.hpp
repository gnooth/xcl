// SlimeInputStream.hpp
//
// Copyright (C) 2009 Peter Graves <peter@armedbear.org>
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

#ifndef __SLIME_INPUT_STREAM_HPP
#define __SLIME_INPUT_STREAM_HPP

#include "Stream.hpp"

class SlimeInputStream : public Stream
{
private:
  AbstractString * _string;
  INDEX _length;
  TypedObject * _function;
  Stream * _stream;

public:
  SlimeInputStream(TypedObject * function, Stream * stream)
    : Stream(WIDETAG_SLIME_INPUT_STREAM, DIRECTION_INPUT),
      _function(function), _stream(stream)
  {
  }

  virtual Value type_of() const
  {
    return S_slime_input_stream;
  }

  virtual Value class_of() const
  {
    return C_slime_input_stream;
  }

  virtual bool typep(Value type) const;

  virtual Value close()
  {
    _open = false;
    return T;
  }

  virtual int read_char();

  virtual void unread_char(BASE_CHAR c)
  {
    if (_offset > 0)
      --_offset;
  }

  virtual bool _charReady()
  {
    return _offset < _length ? true : false;
  }

  virtual void clear_input();
};

#endif
