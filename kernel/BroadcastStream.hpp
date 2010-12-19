// BroadcastStream.hpp
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

#ifndef __BROADCAST_STREAM_HPP
#define __BROADCAST_STREAM_HPP

class BroadcastStream : public AnsiStream
{
private:
  INDEX _numstreams;
  AnsiStream * * _streams;

public:
  BroadcastStream(unsigned long numstreams, AnsiStream * * streams)
    : AnsiStream(WIDETAG_BROADCAST_STREAM, DIRECTION_OUTPUT), _numstreams(numstreams),
      _streams(streams)
  {
  }

  unsigned long numstreams() const
  {
    return _numstreams;
  }

  AnsiStream * * streams() const
  {
    return _streams;
  }

  virtual Value type_of() const
  {
    return S_broadcast_stream;
  }

  virtual Value class_of() const
  {
    return C_broadcast_stream;
  }

  virtual bool typep(Value type) const;

  virtual void write_byte(unsigned char n);

  virtual void write_char(BASE_CHAR c);

  virtual Value file_string_length(Value arg) const;

  virtual Value close()
  {
    // "The effect of CLOSE on a constructed stream is to close the argument
    // stream only. There is no effect on the constituents of composite
    // streams."
    _open = false;
    return T;
  }

  virtual Value file_position()
  {
    if (_numstreams > 0)
      return _streams[_numstreams - 1]->file_position();
    else
      return FIXNUM_ZERO;
  }

  virtual Value set_file_position(Value arg)
  {
    return NIL;
  }

  virtual Value file_length() const
  {
    if (_numstreams > 0)
      return _streams[_numstreams - 1]->file_length();
    else
      return FIXNUM_ZERO;
  }

  virtual Value external_format() const
  {
    if (_numstreams > 0)
      return _streams[_numstreams - 1]->external_format();
    else
      return K_default;
  }
};

inline bool broadcast_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_BROADCAST_STREAM);
}

inline BroadcastStream * the_broadcast_stream(Value value)
{
  assert(broadcast_stream_p(value));
  return reinterpret_cast<BroadcastStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline BroadcastStream * check_broadcast_stream(Value value)
{
  if (broadcast_stream_p(value))
    return the_broadcast_stream(value);
  signal_type_error(value, S_broadcast_stream);
  // Not reached.
  return NULL;
}

#endif // BroadcastStream.hpp
