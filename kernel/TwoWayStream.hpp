// TwoWayStream.hpp
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

#ifndef __TWO_WAY_STREAM_HPP
#define __TWO_WAY_STREAM_HPP

class TwoWayStream : public AnsiStream
{
private:
  AnsiStream * _in;
  AnsiStream * _out;

protected:
  TwoWayStream(long widetag, AnsiStream * in, AnsiStream * out)
    : AnsiStream(widetag, DIRECTION_IO), _in(in), _out(out)
  {
  }

public:
  TwoWayStream(AnsiStream * in, AnsiStream * out)
    : AnsiStream(WIDETAG_TWO_WAY_STREAM, DIRECTION_IO), _in(in), _out(out)
  {
  }

  TwoWayStream(AnsiStream * in, AnsiStream * out, bool interactive)
    : AnsiStream(WIDETAG_TWO_WAY_STREAM, DIRECTION_IO), _in(in), _out(out)
  {
    _interactive = interactive;
  }

  AnsiStream * input_stream() const
  {
    return _in;
  }

  AnsiStream * output_stream() const
  {
    return _out;
  }

  virtual Value element_type() const
  {
    Value input_type = _in->element_type();
    Value output_type = _out->element_type();
    if (::equal(input_type, output_type))
      return input_type;
    return list3(S_and, input_type, output_type);
  }

  virtual Value type_of() const
  {
    return S_two_way_stream;
  }

  virtual Value class_of() const
  {
    return C_two_way_stream;
  }

  virtual bool typep(Value type) const;

  virtual bool is_char_ready()
  {
    return _in->is_char_ready();
  }

  virtual int read_char()
  {
    return _in->read_char();
  }

  virtual void unread_char(BASE_CHAR c)
  {
    _in->unread_char(c);
  }

  virtual long read_byte()
  {
    return _in->read_byte();
  }

  virtual void write_byte(unsigned char n)
  {
    _out->write_byte(n);
  }

  virtual void write_char(BASE_CHAR c)
  {
    _out->write_char(c);
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

inline bool two_way_stream_p(Value value)
{
  return (typed_object_p(value) &&
          the_typed_object(value)->widetag() == WIDETAG_TWO_WAY_STREAM);
}

inline TwoWayStream * the_two_way_stream(Value value)
{
  assert(two_way_stream_p(value));
  return reinterpret_cast<TwoWayStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline TwoWayStream * check_two_way_stream(Value value)
{
  if (two_way_stream_p(value))
    return the_two_way_stream(value);
  signal_type_error(value, S_two_way_stream);
  // Not reached.
  return NULL;
}

#endif // TwoWayStream.hpp
