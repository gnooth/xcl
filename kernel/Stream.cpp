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

AnsiStream * STANDARD_INPUT;
AnsiStream * STANDARD_OUTPUT;
AnsiStream * ERROR_OUTPUT;

AnsiStream::AnsiStream(long widetag)
  : Stream(widetag), _last_char(-1), _direction(DIRECTION_INVALID),
    _element_type(S_character), _fd(-1), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

AnsiStream::AnsiStream(long widetag, Direction direction)
  : Stream(widetag), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(-1), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

AnsiStream::AnsiStream(long widetag, Direction direction, int fd)
  : Stream(widetag), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(fd), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

AnsiStream::AnsiStream(Direction direction, int fd)
  : Stream(WIDETAG_ANSI_STREAM), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(fd), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

#ifdef WIN32
AnsiStream::AnsiStream(Direction direction, HANDLE h)
  : Stream(WIDETAG_ANSI_STREAM), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(-1), _h(h), _offset(0), _charpos(0), _open(true),
    _read_byte_function(NIL)
{
}
#endif


Value AnsiStream::class_of() const
{
  return C_ansi_stream;
}

bool AnsiStream::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_ansi_stream || type == S_stream || type == S_atom || type == T);
  else
    return (type == C_ansi_stream || type == C_stream || type == C_t);
}

bool AnsiStream::is_char_ready()
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

int AnsiStream::read_char()
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

void AnsiStream::unread_char(BASE_CHAR c)
{
  _last_char = c;
}

long AnsiStream::read_byte()
{
  unsigned char c;
  long n = READ(_fd, &c, 1);
  return n <= 0 ? -1 : c;
}

Value AnsiStream::read_char(bool eof_error_p, Value eof_value)
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
Value AnsiStream::read_line(bool eof_error_p, Value eof_value)
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

void AnsiStream::skip_balanced_comment()
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

void AnsiStream::write_string(const char * s)
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

void AnsiStream::write_string(AbstractString * string)
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

void AnsiStream::clear_input()
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

Value AnsiStream::file_string_length(Value arg) const
{
  if (characterp(arg))
    return FIXNUM_ONE;
  if (stringp(arg))
    return make_unsigned_integer(the_string(arg)->length());
  return signal_type_error(arg, list3(S_or, S_string, S_character));
}

Value AnsiStream::close()
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

Value AnsiStream::prin1(Value arg)
{
  write_string(::prin1_to_string(arg));
  return arg;
}

Value AnsiStream::princ(Value arg)
{
  write_string(::princ_to_string(arg));
  return arg;
}

Value AnsiStream::fresh_line()
{
  if (_charpos == 0)
    return NIL;
  else
    {
      write_char('\n');
      return T;
    }
}
