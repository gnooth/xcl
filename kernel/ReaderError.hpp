// ReaderError.hpp
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

#ifndef __READER_ERROR_HPP
#define __READER_ERROR_HPP

#include "StreamError.hpp"

class ReaderError : public StreamError
{
public:
  ReaderError() : StreamError()
  {
  }

  ReaderError(Stream * stream) : StreamError(stream)
  {
  }

  ReaderError(Stream * stream, AbstractString * s) : StreamError(stream)
  {
    set_slot_value(S_format_control, make_value(new_simple_string(s)));
    set_slot_value(S_format_arguments, NIL);
  }

  ReaderError(Stream * stream, const char * s) : StreamError(stream)
  {
    set_slot_value(S_format_control, make_value(new_simple_string(s)));
    set_slot_value(S_format_arguments, NIL);
  }

  ReaderError(Value streamarg) : StreamError(streamarg)
  {
  }

  ReaderError(Value streamarg, AbstractString * s) : StreamError(streamarg)
  {
    set_slot_value(S_format_control, make_value(new_simple_string(s)));
    set_slot_value(S_format_arguments, NIL);
  }

  ReaderError(Value streamarg, const char * s) : StreamError(streamarg)
  {
    set_slot_value(S_format_control, make_value(new_simple_string(s)));
    set_slot_value(S_format_arguments, NIL);
  }

  virtual Value type_of() const
  {
    return S_reader_error;
  }

  virtual Value class_of() const
  {
    return C_reader_error;
  }

  virtual bool typep(Value type) const;
};

#endif // ReaderError.hpp
