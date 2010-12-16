// StringInputStream.cpp
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
#include "reader.hpp"
#include "Readtable.hpp"

class StringInputStream : public Stream
{
private:
  AbstractString * const _string;
  const unsigned long _start;
  const unsigned long _end;

public:
  StringInputStream(AbstractString * string, unsigned long start, unsigned long end)
    : Stream(WIDETAG_STRING_INPUT_STREAM, DIRECTION_INPUT), _string(string), _start(start), _end(end)
  {
    _offset = start;
  }

  virtual Value type_of() const
  {
    return S_string_input_stream;
  }

  virtual Value class_of() const
  {
    return C_string_stream;
  }

  virtual bool typep(Value type) const;

  virtual bool is_char_ready();

  virtual int read_char();

  virtual void unread_char(BASE_CHAR c);
};

inline bool string_input_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_STRING_INPUT_STREAM);
}

inline StringInputStream * the_string_input_stream(Value value)
{
  assert(string_input_stream_p(value));
  return reinterpret_cast<StringInputStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline StringInputStream * check_string_input_stream(Value value)
{
  if (string_input_stream_p(value))
    return the_string_input_stream(value);
  signal_type_error(value, S_string_input_stream);
  // Not reached.
  return NULL;
}

bool StringInputStream::typep(Value type) const
{
  return (type == S_string_input_stream || type == S_string_stream
          || type == S_stream || type == S_atom || type == T);
}

bool StringInputStream::is_char_ready()
{
  return true;
}

int StringInputStream::read_char()
{
  if (_offset < _end)
    return _string->fast_char_at(_offset++);
  else
    return -1;
}

void StringInputStream::unread_char(BASE_CHAR c)
{
  if (_offset > _start)
    --_offset;
}

// ### make-string-input-stream string &optional start end => string-stream
Value CL_make_string_input_stream(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 3)
    return wrong_number_of_arguments(S_make_string_input_stream, numargs, 1, 3);

  AbstractString * string = check_string(args[0]);
  unsigned long start;
  if (numargs >= 2)
    start = check_index(args[1]);
  else
    start = 0;
  unsigned long end;
  if (numargs == 3 && args[2] != NIL)
    end = check_index(args[2]);
  else
    end = string->length();
  return make_value(new StringInputStream(string, start, end));
}

// ### string-input-stream-current stream => pos
Value SYS_string_input_stream_current(Value arg)
{
  return make_fixnum(check_string_input_stream(arg)->offset());
}

// ### %read-from-string string eof-error-p eof-value start end preserve-whitespace
// => object, position
Value SYS_read_from_string_internal(Value arg1, Value arg2, Value arg3,
                                    Value arg4, Value arg5, Value arg6)
{
  AbstractString * string = check_string(arg1);
  bool eof_error_p = (arg2 != NIL);
  bool preserve_whitespace = (arg6 != NIL);
  INDEX start;
  if (arg4 != NIL)
    start = check_index(arg4);
  else
    start = 0;
  INDEX end;
  if (arg5 != NIL)
    end = check_index(arg5);
  else
    end = string->length();
  StringInputStream * in = new StringInputStream(string, start, end);
  Thread * const thread = current_thread();
  Value result;
  Readtable * rt = check_readtable(thread->symbol_value(S_current_readtable));
  if (preserve_whitespace)
    result = in->read_preserving_whitespace(eof_error_p, arg3, false, thread, rt);
  else
    result = stream_read(make_value(in), eof_error_p, arg3, false, thread, rt);
  return thread->set_values(result, make_fixnum(in->offset()));
}
