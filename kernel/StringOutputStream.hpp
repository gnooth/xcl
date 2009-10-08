// StringOutputStream.hpp
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

#ifndef __STRING_OUTPUT_STREAM_HPP
#define __STRING_OUTPUT_STREAM_HPP

class StringOutputStream : public Stream
{
private:
  String * _string;

public:
  StringOutputStream(Value element_type);
  StringOutputStream(String * string);

  virtual Value type_of() const
  {
    return S_string_output_stream;
  }

  virtual Value class_of() const
  {
    return C_string_stream;
  }

  virtual bool typep(Value type) const;

  virtual void write_char(BASE_CHAR c);

  virtual Value file_position()
  {
    return make_fixnum(_string->length());
  }

  AbstractString * get_string();
};

inline bool string_output_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_STRING_OUTPUT_STREAM);
}

inline StringOutputStream * the_string_output_stream(Value value)
{
  assert(string_output_stream_p(value));
  return reinterpret_cast<StringOutputStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline StringOutputStream * check_string_output_stream(Value value)
{
  if (string_output_stream_p(value))
    return the_string_output_stream(value);
  signal_type_error(value, S_string_output_stream);
  // Not reached.
  return NULL;
}

#endif // StringOutputStream.hpp
