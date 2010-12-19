// ConcatenatedStream.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "ConcatenatedStream.hpp"

Value ConcatenatedStream::element_type() const
{
  if (_streams == NIL)
    return NIL;
  return check_ansi_stream(xcar(_streams))->element_type();
}

bool ConcatenatedStream::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_concatenated_stream || type == S_ansi_stream || type == S_stream || type == S_atom|| type == T);
  else
    return (type == C_concatenated_stream || type == C_ansi_stream || type == C_stream || type == C_t);
}

bool ConcatenatedStream::is_char_ready()
{
  if (_last_char >= 0)
    return true;
  if (_streams == NIL)
    return true;
  AnsiStream * stream = check_ansi_stream(xcar(_streams));
  if (stream->is_char_ready())
    return true;
  Value remaining_streams = xcdr(_streams);
  while (remaining_streams != NIL)
    {
      stream = check_ansi_stream(xcar(remaining_streams));
      if (stream->is_char_ready())
        return true;
      remaining_streams = xcdr(remaining_streams);
  }
  return false;
}

int ConcatenatedStream::read_char()
{
  int n;
  if (_last_char >= 0)
    {
      n = _last_char;
      _last_char = -1;
      return n;
    }
  if (_streams == NIL)
    return -1;
  AnsiStream * stream = check_ansi_stream(xcar(_streams));
  n = stream->read_char();
  if (n >= 0)
    return n;
  _streams = xcdr(_streams);
  return read_char();
}

long ConcatenatedStream::read_byte()
{
  if (_streams == NIL)
    return -1;
  AnsiStream * stream = check_ansi_stream(xcar(_streams));
  long n = stream->read_byte();
  if (n >= 0)
    return n;
  _streams = xcdr(_streams);
  return read_byte();
}

// ### make-concatenated-stream &rest streams => concatenated-stream
Value CL_make_concatenated_stream(unsigned int numargs, Value args[])
{
  Value streams = NIL;
  for (unsigned int i = 0; i < numargs; i++)
    {
      AnsiStream * stream = check_ansi_stream(args[i]);
      Direction direction = stream->direction();
      if (direction == DIRECTION_INPUT || direction == DIRECTION_IO)
        streams = make_cons(make_value(stream), streams);
      else
        return signal_type_error(args[0], list2(S_satisfies, S_input_stream_p));
    }
  return make_value(new ConcatenatedStream(CL_nreverse(streams)));
}

// ### concatenated-stream-streams concatenated-stream => streams
Value CL_concatenated_stream_streams(Value arg)
{
  return check_concatenated_stream(arg)->streams();
}
