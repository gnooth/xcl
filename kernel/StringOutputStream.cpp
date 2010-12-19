// StringOutputStream.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "NilVector.hpp"
#include "StringOutputStream.hpp"

StringOutputStream::StringOutputStream(Value element_type)
  : AnsiStream(WIDETAG_STRING_OUTPUT_STREAM, DIRECTION_OUTPUT), _string(new String())
{
  _element_type = element_type;
}

StringOutputStream::StringOutputStream(String * string)
  : AnsiStream(WIDETAG_STRING_OUTPUT_STREAM, DIRECTION_OUTPUT), _string(string)
{
}

bool StringOutputStream::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_string_output_stream || type == S_string_stream
            || type == S_stream || type == S_atom || type == T);
  else
    return (type == C_string_stream || type == C_stream || type == C_t);
}

void StringOutputStream::write_char(BASE_CHAR c)
{
  _string->append_char(c);
  if (c == '\n')
    _charpos = 0;
  else
    ++_charpos;
}

AbstractString * StringOutputStream::get_string()
{
  if (_element_type == NIL)
    return new NilVector(0, NIL);
  SimpleString * string = new_simple_string(_string);
  _string->set_length(0);
  return string;
}

// ### %make-string-output-stream element-type => string-stream
Value SYS_make_string_output_stream_internal(Value arg)
{
  return make_value(new StringOutputStream(arg));
}

// ### make-fill-pointer-output-stream string => string-stream
Value SYS_make_fill_pointer_output_stream(Value arg)
{
  AbstractString * string = check_string(arg);
  string->check_fill_pointer();
  // If it has a fill pointer, it must be a String (and not a SimpleString).
  return make_value(new StringOutputStream((String *)string));
}

// ### get-output-stream-string string-output-stream => string

// "Returns a string containing, in order, all the characters that have been
// output to STRING-OUTPUT-STREAM. This operation clears any characters on
// STRING-OUTPUT-STREAM, so the string contains only those characters which
// have been output since the last call to GET-OUTPUT-STREAM-STRING or since
// the creation of the string-output-stream, whichever occurred most recently."
Value CL_get_output_stream_string(Value arg)
{
  return make_value(check_string_output_stream(arg)->get_string());
}
