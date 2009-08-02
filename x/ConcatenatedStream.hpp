// ConcatenatedStream.hpp
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

#ifndef __CONCATENATED_STREAM_HPP
#define __CONCATENATED_STREAM_HPP

class ConcatenatedStream : public Stream
{
private:
  Value _streams;

public:
  ConcatenatedStream(Value streams)
    : Stream(WIDETAG_CONCATENATED_STREAM, DIRECTION_INPUT), _streams(streams)
  {
  }

  Value streams() const
  {
    return _streams;
  }

  virtual Value element_type() const;

  virtual Value type_of() const
  {
    return S_concatenated_stream;
  }

  virtual Value class_of() const
  {
    return C_concatenated_stream;
  }

  virtual bool typep(Value type) const;

  virtual bool is_char_ready();

  virtual int read_char();

  virtual long read_byte();

  virtual void write_byte(unsigned char n)
  {
    signal_type_error(make_value(this), list2(S_satisfies, S_output_stream_p));
  }

  virtual void write_char(BASE_CHAR c)
  {
    signal_type_error(make_value(this), list2(S_satisfies, S_output_stream_p));
  }

  virtual Value close()
  {
    // "The effect of CLOSE on a constructed stream is to close the argument
    // stream only. There is no effect on the constituents of composite
    // streams."
    _open = false;
    return T;
  }
};

inline bool concatenated_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_CONCATENATED_STREAM);
}

inline ConcatenatedStream * the_concatenated_stream(Value value)
{
  assert(concatenated_stream_p(value));
  return reinterpret_cast<ConcatenatedStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline ConcatenatedStream * check_concatenated_stream(Value value)
{
  if (concatenated_stream_p(value))
    return the_concatenated_stream(value);
  signal_type_error(value, S_concatenated_stream);
  // Not reached.
  return NULL;
}

#endif // ConcatenatedStream.hpp
