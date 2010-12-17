// reader.cpp
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

Value stream_read(Value streamarg, bool eof_error_p,
                  Value eof_value, bool recursive_p,
                  Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      Value result = stream_read_preserving_whitespace(streamarg, eof_error_p, eof_value,
                                                       recursive_p, thread, rt);
      if (result != eof_value && !recursive_p)
        {
          int c = stream->read_char();
          if (c >= 0)
            {
              if (!rt->is_whitespace(c))
                stream->unread_char(c);
            }
        }
      if (thread->symbol_value(S_read_suppress) != NIL)
        return NIL;
      else
        return result;
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read needs code!");
    }
}

Value stream_read_preserving_whitespace(Value streamarg, bool eof_error_p,
                                        Value eof_value, bool recursive_p,
                                        Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      void * last_special_binding = NULL;
      if (!recursive_p)
        {
          last_special_binding = thread->last_special_binding();
          thread->bind_special(S_sharp_equal_alist, NIL);
        }
      Value value;
      while (true)
        {
          int n = stream->read_char();
          if (n < 0)
            {
              if (eof_error_p)
                return signal_lisp_error(new EndOfFile(stream));
              else
                return eof_value;
            }
          BASE_CHAR c = (BASE_CHAR) n;
          if (!rt->is_whitespace(c))
            {
              value = stream->process_char(c, rt, thread);
              if (thread->values_length() != 0)
                break;
              // process_char() returned no values
              thread->clear_values();
            }
        }
      if (!recursive_p)
        thread->set_last_special_binding(last_special_binding);
      return value;
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_preserving_whitespace needs code!");
    }
}

static bool is_whitespace(long c)
{
  switch (c)
    {
    case 9:     // tab
    case 10:    // linefeed
    case 12:    // form feed
    case 13:    // return
    case ' ':   // space
      return true;
    default:
      return false;
    }
}

static bool is_delimiter(long c)
{
  switch (c)
    {
    case '"':
    case '\'':
    case '(':
    case ')':
    case ',':
    case ';':
    case '`':
      return true;
    default:
      return is_whitespace(c);
    }
}

Value stream_read_list(Value streamarg, bool require_proper_list,
                       Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      Value first = NULL_VALUE;
      Value last = NULL_VALUE;
      while (true)
        {
          long n = stream->read_char();
          if (n < 0)
            return signal_lisp_error(new EndOfFile(stream));
          BASE_CHAR c = (BASE_CHAR) n;
          if (rt->is_whitespace(c))
            continue;
          if (c == ')')
            return first == NULL_VALUE ? NIL : first;
          if (c == '.')
            {
              int n2 = stream->read_char();
              if (n2 < 0)
                return signal_lisp_error(new EndOfFile(stream));
              if (is_delimiter(n2))
                {
                  if (last == NULL_VALUE)
                    {
                      if (thread->symbol_value(S_read_suppress) != NIL)
                        return NIL;
                      else
                        // FIXME reader error
                        return signal_lisp_error(new ReaderError(stream, "Nothing appears before . in list."));
                    }
                  stream->unread_char(n2);
                  Value obj = stream_read(streamarg, true, NIL, true, thread, rt);
                  if (require_proper_list)
                    {
                      if (!listp(obj))
                        {
                          String * string = new String("The value ");
                          string->append(::prin1_to_string(obj));
                          string->append(" is not of type ");
                          string->append(the_symbol(S_list)->prin1_to_string());
                          string->append_char('.');
                          return signal_lisp_error(new ReaderError(stream, string));
                        }
                    }
                  setcdr(last, obj);
                  continue;
                }
              // normal token beginning with '.'
              stream->unread_char(n2);
            }
          Value obj = stream->process_char(c, rt, thread);
          if (thread->values_length() == 0)
            {
              // process_char() returned no values
              thread->clear_values();
              continue;
            }
          if (first == NULL_VALUE)
            {
              first = make_cons(obj);
              last = first;
            }
          else
            {
              Value cons = make_cons(obj);
              setcdr(last, cons);
              last = cons;
            }
        }
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_list needs code!");
    }
}

Value stream_read_vector(Value streamarg, INDEX size, Thread * thread, Readtable * rt)
{
  // "If an unsigned decimal integer appears between the # and (, it specifies
  // explicitly the length of the vector. The consequences are undefined if the
  // number of objects specified before the closing ) exceeds the unsigned
  // decimal integer. If the number of objects supplied before the closing ) is
  // less than the unsigned decimal integer but greater than zero, the last
  // object is used to fill all remaining elements of the vector. The
  // consequences are undefined if the unsigned decimal integer is non-zero and
  // number of objects supplied before the closing ) is zero." 2.4.8.3
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      SimpleVector * vector = new_simple_vector(size);
      INDEX index = 0;
      Value last = NIL;
      while (true)
        {
          long n = stream->read_char();
          if (n < 0)
            return signal_lisp_error(new EndOfFile(stream));
          BASE_CHAR c = (BASE_CHAR) n;
          if (rt->is_whitespace(c))
            continue;
          if (c == ')')
            break;
          if (c == '.')
            {
              int n2 = stream->read_char();
              if (n2 < 0)
                return signal_lisp_error(new EndOfFile(stream));
              if (is_delimiter(n2))
                return signal_lisp_error(new ReaderError(stream, "The token \".\" is not allowed here."));
              // normal token beginning with '.'
              stream->unread_char(n2);
            }
          Value obj = stream->process_char(c, rt, thread);
          if (thread->values_length() == 0)
            {
              // process_char() returned no values
              thread->clear_values();
              continue;
            }
          last = obj;
          vector->aset(index++, obj);
        }
      while (index < size)
        vector->aset(index++, last);
      return make_value(vector);
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_vector needs code!");
    }
}
