// Stream.cpp
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

#include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
#include "runtime.h"
#include "primitives.hpp"
#include "reader.hpp"
#include "Complex.hpp"
#include "EndOfFile.hpp"
#include "Package.hpp"
#include "Pathname.hpp"
#include "ProgramError.hpp"
#include "ReaderError.hpp"
#include "Readtable.hpp"
#include "SimpleArray_T.hpp"
#include "SimpleArray_UB8_1.hpp"
#include "ZeroRankArray.hpp"
#include "keywordp.hpp"

Stream * STANDARD_INPUT;
Stream * STANDARD_OUTPUT;
Stream * ERROR_OUTPUT;

Stream::Stream(long widetag)
  : TypedObject(widetag), _last_char(-1), _direction(DIRECTION_INVALID),
    _element_type(S_character), _fd(-1), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

Stream::Stream(long widetag, Direction direction)
  : TypedObject(widetag), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(-1), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

Stream::Stream(long widetag, Direction direction, int fd)
  : TypedObject(widetag), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(fd), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

Stream::Stream(Direction direction, int fd)
  : TypedObject(WIDETAG_STREAM), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(fd), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

#ifdef WIN32
Stream::Stream(Direction direction, HANDLE h)
  : TypedObject(WIDETAG_STREAM), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(-1), _h(h), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
}
#endif


Value Stream::class_of() const
{
  return C_stream;
}

bool Stream::typep(Value type) const
{
  return (type == S_stream || type == S_atom || type == T || type == C_stream || type == C_t);
}

Value Stream::read_binary_data(INDEX length)
{
//   bool suppress = (thread->symbol_value(S_read_suppress) != NIL);
//   String * s = new String();
//   while (true)
//     {
//       int n = read_char();
//       if (n < 0)
//         break;
//       BASE_CHAR c = (BASE_CHAR) n;
//       if (c == '0' || c == '1')
//         s->append_char(c);
//       else
//         {
//           unsigned int syntax = rt->syntax(c);
//           if (syntax == SYNTAX_TYPE_WHITESPACE || syntax == SYNTAX_TYPE_TERMINATING_MACRO)
//             {
//               unread_char(c);
//               break;
//             }
//           else if (!suppress)
//             {
//               String * message = new String("Illegal element for bit-vector: #\\");
//               Value name = CL_char_name(make_character(c));
//               if (stringp(name))
//                 message->append(the_string(name));
//               else
//                 message->append_char(c);
//               return signal_lisp_error(new ReaderError(this, message));
//             }
//         }
//     }
//   if (suppress)
//     return NIL;
//   if (n >= 0)
//     {
//       // numeric arg was supplied
//       const long len = s->length();
//       if (len == 0)
//         {
//           if (n > 0)
//             {
//               String * message = new String("No element specified for bit vector of length ");
//               message->append_long(n);
//               message->append_char('.');
//               return signal_lisp_error(new ReaderError(this, message));
//             }
//         }
//       if (n > len)
//         {
//           const BASE_CHAR c = s->char_at(len - 1);
//           for (long i = len; i < n; i++)
//             s->append_char(c);
//         }
//       else if (n < len)
//         {
//           String * message = new String("Bit vector is longer than specified length: #");
//           message->append_long(n);
//           message->append_char('*');
//           message->append(s);
//           return signal_lisp_error(new ReaderError(this, message));
//         }
      SimpleArray_UB8_1 * array = new_simple_array_ub8_1(length);
      unsigned char * data = array->data();
      for (INDEX i = 0; i < length; i++)
        data[i] = read_byte();
      return make_value(array);
//     }
//   else
//     {
//       String * message = new String("No length specified for binary data.");
//       return signal_lisp_error(new ReaderError(this, message));
//     }
}

bool Stream::is_char_ready()
{
  if (_last_char >= 0)
    return true;

#ifdef WIN32
  return false; // REVIEW
#else
  struct timeval tv;
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  fd_set readfds;
  FD_ZERO(&readfds);
  FD_SET(_fd, &readfds);
  return select(0, &readfds, NULL, NULL, &tv) ? true : false;
#endif
}

int Stream::read_char()
{
  BASE_CHAR c;
  if (_last_char >= 0)
    {
      c = (BASE_CHAR) _last_char;
      _last_char = -1;
      return c;
    }
#ifdef WIN32
 top:
#endif
  int n = READ(_fd, &c, 1);
#ifdef WIN32
  if (c == '\r' && n == 1)
    n = READ(_fd, &c, 1);
  if (n == 0)
    {
      if (GetLastError() == ERROR_OPERATION_ABORTED)
        {
          SetLastError(0);
          if (interrupted)
            RT_handle_interrupt();
          goto top;
        }
    }
#endif
  return n <= 0 ? -1 : c;
}

void Stream::unread_char(BASE_CHAR c)
{
  _last_char = c;
}

long Stream::read_byte()
{
  unsigned char c;
  long n = READ(_fd, &c, 1);
  return n <= 0 ? -1 : c;
}

Value Stream::read_char(bool eof_error_p, Value eof_value)
{
  int n = read_char();
  if (n >= 0)
    return make_character(n);
  if (eof_error_p)
    return signal_lisp_error(new EndOfFile(this));
  else
    return eof_value;
}

// read-line &optional stream eof-error-p eof-value recursive-p => line, missing-newline-p
// recursive-p is ignored
Value Stream::read_line(bool eof_error_p, Value eof_value)
{
  Thread * const thread = current_thread();
  String * string = new String();
  while (true)
    {
      int n = read_char();
      if (n < 0)
        {
          if (string->length() == 0)
            {
              if (eof_error_p)
                return signal_lisp_error(new EndOfFile(this));
              else
                return thread->set_values(eof_value, T);
            }
          else
            return thread->set_values(make_value(string), T);
        }
      if (n == '\n')
        return thread->set_values(make_value(string), NIL);
      else
        string->append_char((char)n);
    }
}

void Stream::skip_balanced_comment()
{
  while (true)
    {
      long n = read_char();
      if (n < 0)
        return;
      if (n == '|')
        {
          n = read_char();
          if (n == '#')
            return;
          else
            unread_char(n);
        }
      else if (n == '#')
        {
          n = read_char();
          if (n == '|')
            skip_balanced_comment(); // nested comment
          else
            unread_char(n);
        }
    }
}

// void Stream::write_char(BASE_CHAR c)
// {
// #ifdef WIN32
//   if (_h != INVALID_HANDLE_VALUE)
//     {
//       DWORD bytes_written;
// //       printf("calling WriteFile ...\n");
// //       fflush(stdout);
//       BOOL b = WriteFile(_h, &c, 1, &bytes_written, NULL); // FIXME errors
//       if (!b)
//         {
//           printf("write_char: WriteFile failed!\n");
//           fflush(stdout);
//         }
//       if (c == '\n')
//         _charpos = 0;
//       else
//         ++_charpos;
//       return;
//     }
// #endif
//   WRITE(_fd, &c, 1);
//   if (c == '\n')
//     _charpos = 0;
//   else
//     ++_charpos;
// }

void Stream::write_string(const char * s)
{
  const INDEX len = strlen(s);
#ifdef WIN32
  if (_h != INVALID_HANDLE_VALUE)
    {
      DWORD bytes_written;
      BOOL b = WriteFile(_h, s, len, &bytes_written, NULL);
      if (!b)
        {
          printf("write_string: WriteFile failed!\n");
          fflush(stdout);
        }
      for (INDEX i = len; i-- > 0;)
        {
          if (s[i] == '\n')
            {
              _charpos = len - (i + 1);
              // REVIEW FlushFileBuffers(_h);
              return;
            }
        }
      _charpos += len;
      return;
    }
#endif
  if (_fd >= 0)
    {
      WRITE(_fd, s, len);
      for (INDEX i = len; i-- > 0;)
        {
          if (s[i] == '\n')
            {
              _charpos = len - (i + 1);
              // REVIEW FlushFileBuffers(_h);
              return;
            }
        }
      _charpos += len;
    }
  else
    {
      for (INDEX i = 0; i < len; i++)
        write_char(s[i]); // write_char() updates _charpos
    }
}

void Stream::write_string(AbstractString * string)
{
  assert(string != NULL);
  const INDEX len = string->length();
#ifdef WIN32
  if (_h != INVALID_HANDLE_VALUE)
    {
      DWORD bytes_written;
      BOOL b = WriteFile(_h, string->as_c_string(), len, &bytes_written, NULL);
      if (!b)
        {
          printf("write_string: WriteFile failed!\n");
          fflush(stdout);
        }
      long index = string->last_index_of('\n');
      if (index < 0)
        _charpos += len;
      else
        {
          _charpos = len - (index + 1);
          // REVIEW FlushFileBuffers(_h);
        }
      return;
    }
#endif
  if (_fd >= 0)
    {
      WRITE(_fd, string->as_c_string(), len);
      long index = string->last_index_of('\n');
      if (index < 0)
        _charpos += len;
      else
        {
          _charpos = len - (index + 1);
          // REVIEW FlushFileBuffers(_h);
        }
    }
  else
    {
      for (INDEX i = 0; i < len; i++)
        write_char(string->fast_char_at(i)); // write_char() updates _charpos
    }
}

void Stream::clear_input()
{
  if (_fd >= 0)
    {
      if (_direction == DIRECTION_INPUT || _direction == DIRECTION_IO)
        {
          while (is_char_ready())
            read_char();
        }
    }
}

Value Stream::file_string_length(Value arg) const
{
  if (characterp(arg))
    return FIXNUM_ONE;
  if (stringp(arg))
    return make_unsigned_integer(the_string(arg)->length());
  return signal_type_error(arg, list3(S_or, S_string, S_character));
}

Value Stream::close()
{
#ifdef WIN32
  if (_h != INVALID_HANDLE_VALUE)
    {
      CloseHandle(_h);
      _h = INVALID_HANDLE_VALUE;
    }
#endif
  if (_fd >= 0)
    {
      ::close(_fd);
      _fd = -1;
    }
  _open = false;
  return T;
}

Value Stream::prin1(Value arg)
{
  write_string(::prin1_to_string(arg));
  return arg;
}

Value Stream::princ(Value arg)
{
  write_string(::princ_to_string(arg));
  return arg;
}

// ### %stream-princ stream object => object
Value SYS_stream_princ_internal(Value stream, Value object)
{
  return check_stream(stream)->princ(object);
}

// ### %stream-write-object stream object => object
Value SYS_stream_write_object_internal(Value stream, Value object)
{
  check_stream(stream)->write_string(::write_to_string(object));
  return object;
}

Value Stream::fresh_line()
{
  if (_charpos == 0)
    return NIL;
  else
    {
      write_char('\n');
      return T;
    }
}

// ### designator-input-stream
Value SYS_designator_input_stream(Value arg)
{
  if (streamp(arg))
    return arg;
  Thread * thread = current_thread();
  if (arg == T)
    return thread->symbol_value(S_terminal_io);
  if (arg == NIL)
    return thread->symbol_value(S_standard_input);
  return signal_type_error(arg,
                           list3(S_or, S_stream,
                                 list3(S_member, T, NIL)));
}

// ### designator-output-stream
Value SYS_designator_output_stream(Value arg)
{
  if (streamp(arg))
    return arg;
  Thread * thread = current_thread();
  if (arg == T)
    return thread->symbol_value(S_terminal_io);
  if (arg == NIL)
    return thread->symbol_value(S_standard_output);
  return signal_type_error(arg,
                           list3(S_or, S_stream,
                                 list3(S_member, T, NIL)));
}

// ### streamp object => generalized-boolean
Value CL_streamp(Value arg)
{
  return streamp(arg) ? T : NIL;
}

// ### open-stream-p stream => generalized-boolean
Value CL_open_stream_p(Value arg)
{
  return check_stream(arg)->is_open() ? T : NIL;
}

// ### stream-element-type stream => typespec
Value CL_stream_element_type(Value arg)
{
  return check_stream(arg)->element_type();
}

// ### input-stream-p stream => generalized-boolean
Value CL_input_stream_p(Value arg)
{
  return check_stream(arg)->is_input_stream() ? T : NIL;
}

// ### output-stream-p stream => generalized-boolean
Value CL_output_stream_p(Value arg)
{
  return check_stream(arg)->is_output_stream() ? T : NIL;
}

// ### interactive-stream-p stream => generalized-boolean
Value CL_interactive_stream_p(Value arg)
{
  return check_stream(arg)->is_interactive() ? T : NIL;
}

// ### %stream-charpos stream => position
// Returns the number of characters on the current line of output.
Value SYS_stream_charpos_internal(Value stream)
{
  // REVIEW verify that the stream is a character output stream
  return make_integer(check_stream(stream)->charpos());
}

// ### %stream-set-charpos stream position => position
Value SYS_stream_set_charpos_internal(Value stream, Value position)
{
  // REVIEW verify that the stream is a character output stream
  check_stream(stream)->set_charpos(check_index(position));
  return position;
}

// ### %stream-terpri
Value SYS_stream_terpri_internal(Value arg)
{
  return check_stream(arg)->terpri();
}

// ### terpri &optional output-stream => nil
Value CL_terpri(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_terpri, numargs, 0, 1);
  Stream * stream;
  if (numargs == 1)
    stream = check_stream(SYS_designator_output_stream(args[0]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_output));
  return stream->terpri();
}

// ### %stream-fresh-line
Value SYS_stream_fresh_line_internal(Value arg)
{
  return check_stream(arg)->fresh_line();
}

// ### fresh-line &optional output-stream => generalized-boolean
Value CL_fresh_line(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_fresh_line, numargs, 0, 1);
  Stream * stream;
  if (numargs == 1)
    stream = check_stream(SYS_designator_output_stream(args[0]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_output));
  return stream->fresh_line();
}

// ### %stream-read input-stream eof-error-p eof-value recursive-p => object
Value SYS_stream_read_internal(Value arg1, Value arg2, Value arg3, Value arg4)
{
  Thread * thread = current_thread();
//   Stream * stream = check_stream(arg1);
  bool eof_error_p = (arg2 != NIL);
  Value eof_value = arg3;
  bool recursive_p = (arg4 != NIL);
  Readtable * rt = current_readtable(thread);
  return stream_read(arg1, eof_error_p, eof_value, recursive_p, thread, rt);
}

// ### read &optional input-stream eof-error-p eof-value recursive-p => object
Value CL_read(unsigned int numargs, Value args[])
{
  if (numargs > 4)
    return wrong_number_of_arguments(S_read, numargs, 0, 4);
  Thread * thread = current_thread();
  Stream * stream =
    check_stream(numargs == 0 ? thread->symbol_value(S_standard_input) : SYS_designator_input_stream(args[0]));
  bool eof_error_p = !(numargs > 1 && args[1] == NIL);
  Value eof_value = (numargs > 2) ? args[2] : NIL;
  bool recursive_p = (numargs > 3 && args[3] != NIL);
  Readtable * rt = current_readtable(thread);
  // REVIEW call SYS_stream_read_internal
  return stream_read(make_value(stream), eof_error_p, eof_value, recursive_p, thread, rt);
}

// ### read-preserving-whitespace &optional input-stream eof-error-p eof-value recursive-p => object
Value CL_read_preserving_whitespace(unsigned int numargs, Value args[])
{
  if (numargs > 4)
    return wrong_number_of_arguments(S_read, numargs, 0, 4);
  Thread * thread = current_thread();
  Stream * stream =
    check_stream(numargs == 0 ? thread->symbol_value(S_standard_input) : SYS_designator_input_stream(args[0]));
  bool eof_error_p = !(numargs > 1 && args[1] == NIL);
  Value eof_value = (numargs > 2) ? args[2] : NIL;
  bool recursive_p = (numargs > 3 && args[3] != NIL);
  Readtable * rt = current_readtable(thread);
  // REVIEW
  return stream_read_preserving_whitespace(make_value(stream), eof_error_p, eof_value,
                                           recursive_p, thread, rt);
}

// ### read-line &optional input-stream eof-error-p eof-value recursive-p => line, missing-newline-p
Value CL_read_line(unsigned int numargs, Value args[])
{
  if (numargs <= 4)
    {
      Stream * stream;
      if (numargs > 0)
        stream = check_stream(SYS_designator_input_stream(args[0]));
      else
        stream = check_stream(current_thread()->symbol_value(S_standard_input));
      switch (numargs)
        {
        case 0:
        case 1:
          return stream->read_line(true, NIL);
        case 2:
          return stream->read_line(args[1] != NIL, NIL);
        case 3:
        case 4:
          return stream->read_line(args[1] != NIL, args[2]);
        }
    }
  return wrong_number_of_arguments(S_read_line, numargs, 0, 4);
}

// ### read-char &optional input-stream eof-error-p eof-value recursive-p => char
Value CL_read_char(unsigned int numargs, Value args[])
{
  if (numargs <= 4)
    {
      Stream * stream;
      if (numargs > 0)
        stream = check_stream(SYS_designator_input_stream(args[0]));
      else
        stream = check_stream(current_thread()->symbol_value(S_standard_input));
      int n = stream->read_char();
      if (n >= 0)
        return make_character((BASE_CHAR)n);
      switch (numargs)
        {
        case 0:
        case 1:
          return signal_lisp_error(new EndOfFile(stream));
        case 2:
          if (args[1] != NIL)
            return signal_lisp_error(new EndOfFile(stream));
          else
            return NIL;
        case 3:
        case 4:
          if (args[1] != NIL)
            return signal_lisp_error(new EndOfFile(stream));
          else
            return args[2];
        }
    }
  return wrong_number_of_arguments(S_read_char, numargs, 0, 4);
}

// ### read-char-no-hang &optional input-stream eof-error-p eof-value recursive-p => char
Value CL_read_char_no_hang(unsigned int numargs, Value args[])
{
  if (numargs <= 4)
    {
      Stream * stream;
      if (numargs > 0)
        stream = check_stream(SYS_designator_input_stream(args[0]));
      else
        stream = check_stream(current_thread()->symbol_value(S_standard_input));
      if (!stream->is_char_ready())
        return NIL;
      long c = stream->read_char();
      if (c >= 0)
        return make_character(c);
      switch (numargs)
        {
        case 0:
        case 1:
          return signal_lisp_error(new EndOfFile(stream));
        case 2:
          if (args[1] != NIL)
            return signal_lisp_error(new EndOfFile(stream));
          else
            return NIL;
        case 3:
        case 4:
          if (args[1] != NIL)
            return signal_lisp_error(new EndOfFile(stream));
          else
            return args[2];
        }
    }
  return wrong_number_of_arguments(S_read_char_no_hang, numargs, 0, 4);
}

// ### unread-char character &optional input-stream => nil
Value CL_unread_char(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_unread_char, numargs, 1, 2);
  BASE_CHAR c = char_value(args[0]);
  Stream * stream;
  if (numargs == 2)
    stream = check_stream(SYS_designator_input_stream(args[1]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_input));
  stream->unread_char(c);
  return NIL;
}

// ### listen &optional input-stream => generalized-boolean
Value CL_listen(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_listen, numargs, 0, 1);
  Stream * stream;
  if (numargs == 1)
    stream = check_stream(SYS_designator_input_stream(args[0]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_input));
  if (stream->is_char_ready())
    {
      long c = stream->read_char();
      if (c >= 0)
        {
          stream->unread_char(c);
          return T;
        }
    }
  return NIL;
}

// ### read-delimited-list char &optional input-stream recursive-p => list
Value CL_read_delimited_list(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 3)
    wrong_number_of_arguments(S_read_delimited_list, numargs, 1, 3);
  BASE_CHAR c = char_value(args[0]);
  Thread * thread = current_thread();
  Stream * stream;
  if (numargs > 1)
    stream = check_stream(SYS_designator_input_stream(args[1]));
  else
    stream = check_stream(thread->symbol_value(S_standard_input));
  return stream_read_delimited_list(make_value(stream), c, thread);
}

// ### %stream-write-string stream string start end => string
Value SYS_stream_write_string_internal(Value arg1, Value arg2, Value arg3, Value arg4)
{
  Stream * stream = check_stream(arg1);
  AbstractString * string = check_string(arg2);
  INDEX start = check_index(arg3);
  INDEX end = check_index(arg4);
  for (INDEX i = start; i < end; i++)
    stream->write_char(string->char_at(i));
  return arg2;
}

// ### %stream-write-char stream char => char
Value SYS_stream_write_char_internal(Value arg1, Value arg2)
{
  check_stream(arg1)->write_char(char_value(arg2));
  return arg2;
}

// ### write-char character &optional output-stream => character
Value CL_write_char(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_write_char, numargs, 1, 2);
  char c = char_value(args[0]);
  Stream * stream;
  if (numargs == 2)
    stream = check_stream(SYS_designator_output_stream(args[1]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_output));
  stream->write_char(c);
  return args[0];
}

// ### %write-8-bits byte stream => byte
Value SYS_xwrite_8_bits(Value byte, Value stream)
{
  BYTE n = (BYTE) xlong(byte);
  the_stream(stream)->write_byte(n);
  return byte;
}

// ### write-8-bits byte stream => byte
Value SYS_write_8_bits(Value byte, Value stream)
{
  BYTE n = (BYTE) check_index(byte, 0, 255);
  check_stream(stream)->write_byte(n);
  return byte;
}

// ### read-8-bits stream eof-error-p eof-value => byte
Value SYS_read_8_bits(Value arg1, Value arg2, Value arg3)
{
  Stream * stream = check_stream(arg1);
  long n = stream->read_byte();
  if (n >= 0)
    return make_fixnum(n);
  // eof
  if (arg2 != NIL)
    return signal_lisp_error(new EndOfFile(stream));
  return arg3;
}

Value SYS_stream_read_byte_function(Value arg)
{
  return check_stream(arg)->read_byte_function();
}

// ### clear-input &optional input-stream => nil
Value CL_clear_input(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_clear_input, numargs, 0, 1);
  Stream * stream;
  if (numargs == 1)
    stream = check_stream(SYS_designator_input_stream(args[0]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_input));
  stream->clear_input();
  return NIL;
}

// ### clear-output &optional output-stream => nil
Value CL_clear_output(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_clear_output, numargs, 0, 1);
  if (numargs == 1)
    check_stream(SYS_designator_output_stream(args[0]));
  else
    check_stream(current_thread()->symbol_value(S_standard_output));
  return NIL;
}

// ### file-position
// file-position stream => position
// file-position stream position-spec => success-p
Value CL_file_position(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return check_stream(args[0])->file_position();
    case 2:
      return check_stream(args[0])->set_file_position(args[1]);
    default:
      return wrong_number_of_arguments(S_file_position, numargs, 1, 2);
    }
}

// ### file-length stream => length
Value CL_file_length(Value arg)
{
  return check_stream(arg)->file_length();
}

// ### file-string-length stream object => length
Value CL_file_string_length(Value arg1, Value arg2)
{
  return check_stream(arg1)->file_string_length(arg2);
}

// ### finish-output &optional output-stream => nil
Value CL_finish_output(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_finish_output, numargs, 0, 1);
  if (numargs == 1)
    check_stream(SYS_designator_output_stream(args[0]))->finish_output();
  else
    check_stream(current_thread()->symbol_value(S_standard_output))->finish_output();
  return NIL;
}

// ### force-output &optional output-stream => nil
// REVIEW identical to CL_finish_output()
Value CL_force_output(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_force_output, numargs, 0, 1);
  if (numargs == 1)
    check_stream(SYS_designator_output_stream(args[0]))->finish_output();
  else
    check_stream(current_thread()->symbol_value(S_standard_output))->finish_output();
  return NIL;
}

// ### %stream-close stream => result
Value SYS_stream_close_internal(Value arg)
{
  return check_stream(arg)->close();
}

// ### close stream &key abort => result
Value CL_close(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return check_stream(args[0])->close();
    case 2:
      return signal_lisp_error(new ProgramError("Odd number of keyword arguments."));
    case 3:
      {
        if (args[1] != K_abort)
          {
            String * message = new String("Unrecognized keyword argument ");
            message->append(::prin1_to_string(args[1]));
            return signal_lisp_error(message);
          }
        return check_stream(args[0])->close();
      }
    default:
      return wrong_number_of_arguments(S_close, numargs, 1, 3);
    }
}
