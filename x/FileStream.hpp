// FileStream.hpp
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

#ifndef __FILE_STREAM_HPP
#define __FILE_STREAM_HPP

class FileStream : public Stream
{
private:
  Value _pathname;
  long _bytes_per_unit;

  void * _data;
  INDEX _data_size;
  INDEX _data_offset;

#ifdef WIN32
  HANDLE _hfile;
  HANDLE _hmapping;
#endif

  BYTE * _output_buffer;
  INDEX _output_buffer_size;
  INDEX _output_buffer_offset;

  void flush_output_buffer();

public:
  FileStream(Value pathname, Value namestring, Value element_type,
             Value direction, Value if_exists);

  Value pathname() const
  {
    return _pathname;
  }

  virtual Value type_of() const;
  virtual Value class_of() const;
  virtual bool typep(Value type) const;

  virtual bool is_char_ready()
  {
    return true;
  }

  virtual int read_char();

  void write_byte_to_output_buffer(BYTE b)
  {
    if (_output_buffer_offset >= _output_buffer_size)
      flush_output_buffer();
    _output_buffer[_output_buffer_offset++] = b;
  }

  virtual void write_char(BASE_CHAR c);

  virtual void write_string(const char * s);

  virtual void write_string(AbstractString * string);

  virtual long read_byte();

  virtual void write_byte(BYTE b);

  virtual void clear_input()
  {
  }

  virtual Value close();

  virtual void finish_output();

  virtual Value file_position();

  virtual Value set_file_position(Value arg);

  virtual Value file_length() const;

  virtual Value external_format() const
  {
    return K_default;
  }
};

inline bool file_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_FILE_STREAM);
}

inline FileStream * the_file_stream(Value value)
{
  assert(file_stream_p(value));
  return reinterpret_cast<FileStream *>(value - LOWTAG_TYPED_OBJECT);
}

#endif // FileStream.hpp
