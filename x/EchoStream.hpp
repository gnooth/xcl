// EchoStream.hpp
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

#ifndef __ECHO_STREAM_HPP
#define __ECHO_STREAM_HPP

class EchoStream : public Stream
{
private:
  Stream * _in;
  Stream * _out;

public:
  EchoStream(Stream * in, Stream * out);

  Stream * input_stream() const
  {
    return _in;
  }

  Stream * output_stream() const
  {
    return _out;
  }

  virtual Value type_of() const
  {
    return S_echo_stream;
  }

  virtual Value class_of() const
  {
    return C_echo_stream;
  }

  virtual bool typep(Value type) const;

  virtual bool is_char_ready()
  {
    return _in->is_char_ready();
  }

  virtual int read_char();

  virtual void unread_char(BASE_CHAR c)
  {
    _in->unread_char(c);
    _last_char = c;
  }

  virtual void write_char(BASE_CHAR c)
  {
    _out->write_char(c);
  }

  virtual long read_byte()
  {
    long n = _in->read_byte();
    if (n >= 0)
      _out->write_byte(n);
    return n;
  }

  virtual void write_byte(unsigned char n)
  {
    _out->write_byte(n);
  }

  virtual Value close()
  {
    // "The effect of CLOSE on a constructed stream is to close the argument
    // stream only. There is no effect on the constituents of composite
    // streams."
    _open = false;
    return T;
  }

  virtual Value fresh_line()
  {
    return _out->fresh_line();
  }
};

inline bool echo_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_ECHO_STREAM);
}

inline EchoStream * the_echo_stream(Value value)
{
  assert(echo_stream_p(value));
  return reinterpret_cast<EchoStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline EchoStream * check_echo_stream(Value value)
{
  if (echo_stream_p(value))
    return the_echo_stream(value);
  signal_type_error(value, S_echo_stream);
  // Not reached.
  return NULL;
}

#endif // EchoStream.hpp
