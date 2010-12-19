// SlimeInputStream.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "SlimeInputStream.hpp"

bool SlimeInputStream::typep(Value type) const
{
  return (type == S_slime_input_stream || type == S_stream || type == S_atom || type == T
          || type == C_slime_input_stream || type == C_stream || type == C_t);
}

int SlimeInputStream::read_char()
{
  if (_offset >= _length)
    {
      _stream->finish_output();
      _string = check_string(current_thread()->execute(_function));
      if (_string->length() == 0)
        return -1;
      _offset = 0;
      _length = _string->length();
    }
  BASE_CHAR c = _string->char_at(_offset);
  ++_offset;
  return c;
}

void SlimeInputStream::clear_input()
{
  while (is_char_ready())
    read_char();
  _offset = 0;
  _length = 0;
}

// ### make-slime-input-stream function output-stream => slime-input-stream
Value EXT_make_slime_input_stream(Value arg1, Value arg2)
{
  TypedObject * function;
  if (symbolp(arg1))
    function = check_function(CL_symbol_function(arg1));
  else
    function = check_function(arg1);
  AnsiStream * stream = check_ansi_stream(arg2);
  if (!stream->is_output_stream())
    {
      String * s = new String("The value ");
      s->append(::write_to_string(arg2));
      s->append(" is not an output stream.");
      return signal_lisp_error(s);
    }
  return make_value(new SlimeInputStream(function, stream));
}
