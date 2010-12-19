// EchoStream.cpp
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
#include "EchoStream.hpp"

EchoStream::EchoStream(AnsiStream * in, AnsiStream * out)
  : AnsiStream(WIDETAG_ECHO_STREAM, DIRECTION_IO), _in(in), _out(out)
{
  Value itype = in->element_type();
  Value otype = out->element_type();
  if (::equal(itype, otype))
    _element_type = itype;
  else
    _element_type = list3(S_and, itype, otype);
}

bool EchoStream::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_echo_stream || type == S_ansi_stream || type == S_stream || type == S_atom || type == T);
  else
    return (type == C_echo_stream || type == C_ansi_stream || type == C_stream || type == C_t);
}

int EchoStream::read_char()
{
  int n = _in->read_char();
  if (n >= 0)
    {
      // not at end of file
      if (_last_char < 0)
        _out->write_char((char)n);
      else
        _last_char = -1;
    }
  return n;
}

// ### make-echo-stream input-stream output-stream => echo-stream
Value CL_make_echo_stream(Value arg1, Value arg2)
{
  return make_value(new EchoStream(check_ansi_stream(arg1), check_ansi_stream(arg2)));
}

// ### echo-stream-input-stream echo-stream => input-stream
Value CL_echo_stream_input_stream(Value arg)
{
  return make_value(check_echo_stream(arg)->input_stream());
}

// ### echo-stream-output-stream echo-stream => output-stream
Value CL_echo_stream_output_stream(Value arg)
{
  return make_value(check_echo_stream(arg)->output_stream());
}
