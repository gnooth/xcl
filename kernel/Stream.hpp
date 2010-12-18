// Stream.hpp
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

#ifndef __STREAM_HPP
#define __STREAM_HPP

class Readtable;

enum Direction
{
  DIRECTION_INVALID = 0,
  DIRECTION_INPUT   = 1,
  DIRECTION_OUTPUT  = 2,
  DIRECTION_IO      = 3
};

class Stream : public TypedObject
{
protected:
  int _last_char;
  Direction _direction;
  bool _interactive;
  Value _element_type;
  int _fd;
#ifdef WIN32
  HANDLE _h;
#endif
  INDEX _offset;

  // the number of characters on the current line of output
  long _charpos;
  bool _open;

  Value _read_byte_function;

  Stream(long widetag);
  Stream(long widetag, Direction direction);
  Stream(long widetag, Direction direction, int fd);

public:
  Stream(Direction direction, int fd);

#ifdef WIN32
  Stream(Direction direction, HANDLE h);
#endif

  Direction direction() const { return _direction; }

  virtual bool is_input_stream () const
  {
    return (_direction == DIRECTION_INPUT || _direction == DIRECTION_IO);
  }

  virtual bool is_output_stream () const
  {
    return (_direction == DIRECTION_OUTPUT || _direction == DIRECTION_IO);
  }

  virtual bool is_interactive() const
  {
    return _interactive;
  }

  virtual Value element_type() const
  {
    return _element_type;
  }

  int fd() const
  {
    return _fd;
  }

  unsigned long offset() const
  {
    return _offset;
  }

  long charpos() const
  {
    return _charpos;
  }

  void set_charpos(long n)
  {
    _charpos = n;
  }

  bool is_open() const
  {
    return _open;
  }

  Value read_byte_function() const
  {
    return _read_byte_function;
  }

  virtual Value type_of() const
  {
    return S_stream;
  }

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual bool is_char_ready();

  virtual int read_char();

  virtual void unread_char(BASE_CHAR c);

  virtual Value read_char(bool eof_error_p, Value eof_value);

  Value read_line(bool eof_error_p, Value eof_value);

  void skip_balanced_comment();

  // not virtual
  void inline_write_char(BASE_CHAR c)
  {
#ifdef WIN32
    if (_h != INVALID_HANDLE_VALUE)
      {
        DWORD bytes_written;
        WriteFile(_h, &c, 1, &bytes_written, NULL);
        if (c == '\n')
          _charpos = 0;
        else
          ++_charpos;
        return;
      }
#endif
    WRITE(_fd, &c, 1);
    if (c == '\n')
      _charpos = 0;
    else
      ++_charpos;
  }

  virtual void write_char(BASE_CHAR c)
  {
    inline_write_char(c);
  }

  virtual void write_string(const char * s);

  virtual void write_string(AbstractString * string);

  virtual long read_byte();

  Value read_binary_data(INDEX length);

  // not virtual
  void inline_write_byte(BYTE b)
  {
#ifdef WIN32
    if (_h != INVALID_HANDLE_VALUE)
      {
        DWORD bytes_written;
        WriteFile(_h, &b, 1, &bytes_written, NULL);
        return;
      }
#endif
    WRITE(_fd, &b, 1);
  }

  virtual void write_byte(BYTE b)
  {
    inline_write_byte(b);
  }

  virtual Value close();

  Value prin1(Value value);
  Value princ(Value value);

  Value terpri()
  {
    write_char('\n');
    return NIL;
  }

  virtual Value fresh_line();

  virtual void finish_output()
  {
    // REVIEW fsync(_fd)
  }

  virtual void clear_input();

  virtual Value file_position()
  {
    return NIL;
  }

  virtual Value set_file_position(Value arg)
  {
    return NIL;
  }

  virtual Value file_length() const
  {
    return signal_type_error(make_value(this), S_file_stream);
  }

  virtual Value file_string_length(Value arg) const;

  virtual Value external_format() const
  {
    return signal_type_error(make_value(this), S_file_stream);
  }
};

inline bool streamp(Value value)
{
  if (typed_object_p(value))
    {
      if (the_typed_object(value)->widetag() & WIDETAG_STREAM_BIT) // REVIEW
        return true;
    }
  return false;
}

inline bool ansi_stream_p(Value value)
{
  if (typed_object_p(value))
    {
      if (the_typed_object(value)->widetag() & WIDETAG_STREAM_BIT) // REVIEW
        return true;
    }
  return false;
}

inline Stream * the_stream(Value value)
{
  assert(streamp(value));
  return reinterpret_cast<Stream *>(value - LOWTAG_TYPED_OBJECT);
}

inline Stream * check_stream(Value value)
{
  if (streamp(value))
    return the_stream(value);
  signal_type_error(value, S_stream);
  // not reached
  return NULL;
}

extern Stream * STANDARD_INPUT;
extern Stream * STANDARD_OUTPUT;
extern Stream * ERROR_OUTPUT;

#endif // Stream.hpp
