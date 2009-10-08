// peek-char.cpp
//
// Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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
#include "primitives.hpp"
#include "EchoStream.hpp"
#include "Readtable.hpp"

// ### peek-char &optional peek-type input-stream eof-error-p eof-value recursive-p => char
Value CL_peek_char(unsigned int numargs, Value args[])
{
  if (numargs > 5)
    return wrong_number_of_arguments(S_peek_char, numargs, 0, 5);
  Value peek_type = numargs > 0 ? args[0] : NIL;
  Stream * stream;
  if (numargs > 1)
    stream = the_stream(SYS_designator_input_stream(args[1]));
  else
    stream = check_stream(current_thread()->symbol_value(S_standard_input));
  bool eof_error_p = numargs > 2 ? (args[2] != NIL) : true;
  Value eof_value = numargs > 3 ? args[3] : NIL;
  // recursive-p is ignored
  // bool recursive = numargs > 4 ? (args[4] != NIL) : false;
  if (peek_type == NIL)
    {
      // "If PEEK-TYPE is not supplied or NIL, PEEK-CHAR returns the next
      // character to be read from INPUT-STREAM, without actually removing
      // it from INPUT-STREAM."
      Stream * in;
      if (stream->widetag() == WIDETAG_ECHO_STREAM)
        // "When INPUT-STREAM is an echo stream, characters that are
        // only peeked at are not echoed." Read from the echo stream's
        // input stream to bypass the echo.
        in = reinterpret_cast<EchoStream *>(stream)->input_stream();
      else
        in = stream;
      Value result = in->read_char(eof_error_p, eof_value);
      if (characterp(result))
        in->unread_char(xchar(result));
      return result;
    }
  if (peek_type == T)
    {
      // "If PEEK-TYPE is T, then PEEK-CHAR skips over whitespace[2]
      // characters, but not comments, and then performs the peeking
      // operation on the next character."
      Thread * thread = current_thread();
      Readtable * rt = check_readtable(thread->symbol_value(S_current_readtable));
      while (true)
        {
          Value result = stream->read_char(eof_error_p, eof_value);
          if (characterp(result))
            {
              char c = xchar(result);
              if (!rt->is_whitespace(c))
                {
                  stream->unread_char(c);
                  return result;
                }
            }
          else
            return result;
        }
    }
  if (characterp(peek_type))
    {
      // "If PEEK-TYPE is a character, then PEEK-CHAR skips over input
      // characters until a character that is CHAR= to that character is
      // found; that character is left in INPUT-STREAM."
      char c = xchar(peek_type);
      while (true)
        {
          Value result = stream->read_char(eof_error_p, eof_value);
          if (characterp(result))
            {
              if (xchar(result) == c)
                {
                  stream->unread_char(c);
                  return result;
                }
            }
          else
            return result;
        }
    }
  String * s = new String("The value ");
  s->append(::prin1_to_string(peek_type));
  s->append(" is an illegal peek-type.");
  return signal_lisp_error(s);
}
