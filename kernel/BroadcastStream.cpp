// BroadcastStream.cpp
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

#include "lisp.hpp"
#include "BroadcastStream.hpp"

bool BroadcastStream::typep(Value type) const
{
  return (type == S_broadcast_stream || type == S_stream || type == S_atom || type == T
          || type == C_broadcast_stream || type == C_stream || type == C_t);
}

void BroadcastStream::write_byte(unsigned char n)
{
  for (INDEX i = 0; i < _numstreams; i++)
    _streams[i]->write_byte(n);
}

void BroadcastStream::write_char(BASE_CHAR c)
{
  for (INDEX i = 0; i < _numstreams; i++)
    _streams[i]->write_char(c);
  if (c == '\n')
    _charpos = 0;
  else
    ++_charpos;
}

Value BroadcastStream::file_string_length(Value arg) const
{
  if (_numstreams > 0)
    return _streams[_numstreams - 1]->file_string_length(arg);
  else
    return FIXNUM_ONE;
}

// ### make-broadcast-stream &rest streams => broadcast-stream
Value CL_make_broadcast_stream(unsigned int numargs, Value args[])
{
  AnsiStream * * streams = (AnsiStream * *) GC_malloc(numargs * sizeof(AnsiStream *));
  for (unsigned int i = 0; i < numargs; i++)
    {
      AnsiStream * stream = check_ansi_stream(args[i]);
      Direction direction = stream->direction();
      if (direction == DIRECTION_OUTPUT || direction == DIRECTION_IO)
        streams[i] = stream;
      else
        return signal_type_error(args[i], list2(S_satisfies, S_output_stream_p));
    }
  return make_value(new BroadcastStream(numargs, streams));
}

Value CL_broadcast_stream_streams(Value arg)
{
  BroadcastStream * broadcast_stream = check_broadcast_stream(arg);
  AnsiStream * * streams = broadcast_stream->streams();
  Value result = NIL;
  for (INDEX i = broadcast_stream->numstreams(); i-- > 0;)
    result = make_cons(make_value(streams[i]), result);
  return result;
}
