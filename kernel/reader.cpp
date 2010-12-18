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

static Value stream_process_char(Value streamarg, BASE_CHAR c, Readtable * rt, Thread * thread)
{
  if (ansi_stream_p(streamarg))
    {
      Value handler = rt->reader_macro_function(c);
      if (handler != NULL_VALUE)
        {
          // REVIEW error handling
          TypedObject * function;
          if (symbolp(handler))
            function = the_symbol(handler)->function();
          else if (typed_object_p(handler))
            function = the_typed_object(handler);
          else
            return signal_type_error(handler, S_function_designator);
          // REVIEW
          return thread->execute(function, streamarg, make_character(c));
        }
      // no handler
      return stream_read_atom(streamarg, c, rt, thread);
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_process_char needs code!");
    }
}

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
              value = stream_process_char(streamarg, c, rt, thread);
              if (thread->values_length() != 0)
                break;
              // stream_process_char() returned no values
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
                        return signal_lisp_error(new ReaderError(streamarg, "Nothing appears before . in list."));
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
                          return signal_lisp_error(new ReaderError(streamarg, string));
                        }
                    }
                  setcdr(last, obj);
                  continue;
                }
              // normal token beginning with '.'
              stream->unread_char(n2);
            }
          Value obj = stream_process_char(streamarg, c, rt, thread);
          if (thread->values_length() == 0)
            {
              // stream_process_char() returned no values
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
                return signal_lisp_error(new ReaderError(streamarg, "The token \".\" is not allowed here."));
              // normal token beginning with '.'
              stream->unread_char(n2);
            }
          Value obj = stream_process_char(streamarg, c, rt, thread);
          if (thread->values_length() == 0)
            {
              // stream_process_char() returned no values
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

static String * stream_read_multiple_escape(Value streamarg, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      String * string = new String();
      while (true)
        {
          long n = stream->read_char();
          if (n < 0)
            {
              signal_lisp_error(new EndOfFile(stream));
              // not reached
              return NULL;
            }
          BASE_CHAR c = (BASE_CHAR) n;
          unsigned int syntax = rt->syntax(c);
          if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
            {
              n = stream->read_char();
              if (n < 0)
                {
                  signal_lisp_error(new EndOfFile(stream));
                  // not reached
                  return NULL;
                }
              string->append_char((BASE_CHAR)n);
              continue;
            }
          if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
            break;
          string->append_char(c);
        }
      return string;
    }
  else
    {
      // fundamental-stream
      signal_lisp_error("stream_read_multiple_escape needs code!");
      return NULL;
    }
}

static void invert(String * string, BitVector * escapes)
{
  // Section 23.1.2: "When the readtable case is :INVERT, then if all of the
  // unescaped letters in the extended token are of the same case, those
  // (unescaped) letters are converted to the opposite case."
  INDEX len = string->length();
  const int LOWER = 1;
  const int UPPER = 2;
  int state = 0;
  for (INDEX i = 0; i < len; i++)
    {
      // we only care about unescaped characters
      if (escapes != NULL && escapes->get_bit(i))
        continue;
      BASE_CHAR c = string->fast_char_at(i);
      if (isupper(c))
        {
          if (state == LOWER)
            return; // mixed case
          state = UPPER;
        }
      if (islower(c))
        {
          if (state == UPPER)
            return; // mixed case
          state = LOWER;
        }
    }
  // all of the unescaped letters are of the same case
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = string->fast_char_at(i);
      if (escapes != NULL && escapes->get_bit(i))
        ; // escaped, no change
      else if (isupper(c))
        string->fast_set_char_at(i, tolower(c));
      else if (islower(c))
        string->fast_set_char_at(i, toupper(c));
    }
}

static Value intern(AbstractString * prefix, AbstractString * suffix, bool external)
{
  Package * package = find_package(prefix);
  if (!package)
    {
      String * s = new String("Package \"");
      s->append(prefix);
      s->append("\" not found.");
      return signal_lisp_error(s);
    }
  return package->intern(suffix, external);
}

Value stream_read_symbol(Value streamarg, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      Value readtable_case = rt->readtable_case();
      String * string = new String();
      while (1)
        {
          int n = stream->read_char();
          if (n < 0)
            break;
          BASE_CHAR c = (BASE_CHAR) n;
          if (rt->is_whitespace(c))
            {
              stream->unread_char(c);
              break;
            }
          unsigned int syntax = rt->syntax(c);
          if (syntax == SYNTAX_TYPE_TERMINATING_MACRO)
            {
              stream->unread_char(c);
              break;
            }
          if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
            {
              n = stream->read_char();
              if (n < 0)
                return signal_lisp_error(new EndOfFile(stream));
              string->append_char((BASE_CHAR)n);
              continue;
            }
          if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
            {
              string->append(stream_read_multiple_escape(streamarg, rt));
              continue;
            }
          if (readtable_case == K_upcase)
            string->append_char(toupper(c));
          else if (readtable_case == K_downcase)
            string->append_char(tolower(c));
          else if (readtable_case == K_invert)
            {
              if (islower(c))
                string->append_char(toupper(c));
              else if (isupper(c))
                string->append_char(tolower(c));
              else
                string->append_char(c);
            }
          else
            string->append_char(c);
        }
      return make_symbol(string);
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_symbol needs code!");
    }
}

static BitVector * stream_read_token(Value streamarg, unsigned char c1,
                                     Readtable * rt, Thread * thread,
                                     String * string)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      BitVector * escapes = NULL;
      Value readtable_case = rt->readtable_case();
      unsigned int syntax = rt->syntax(c1);
      if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
        {
          string->append(stream_read_multiple_escape(streamarg, rt));
          escapes = new BitVector(string->length(), false);
          escapes->fill(FIXNUM_ONE);
        }
      else if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
        {
          int n = stream->read_char();
          if (n < 0)
            {
              signal_lisp_error(new EndOfFile(stream));
              // not reached
              return NULL;
            }
          string->append_char((BASE_CHAR)n);
          escapes = new BitVector(1, false);
          escapes->set_bit(0);
        }
      else
        {
          rt->check_invalid(c1, stream);
          if (readtable_case == K_upcase)
            string->append_char(toupper(c1));
          else if (readtable_case == K_downcase)
            string->append_char(tolower(c1));
          else
            string->append_char(c1);
        }
      while (true)
        {
          int n = stream->read_char();
          if (n < 0)
            break;
          BASE_CHAR c = (BASE_CHAR) n;
          if (rt->is_whitespace(c))
            {
              stream->unread_char(c);
              break;
            }
          syntax = rt->syntax(c);
          if (syntax == SYNTAX_TYPE_TERMINATING_MACRO)
            {
              stream->unread_char(c);
              break;
            }
          if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
            {
              INDEX begin = string->length();
              string->append(stream_read_multiple_escape(streamarg, rt));
              INDEX end = string->length();
              if (escapes == NULL)
                escapes = new BitVector(end, false);
              else
                escapes->ensure_capacity(string->length());
              for (INDEX i = begin; i < end; i++)
                escapes->set_bit(i);
              continue;
            }
          if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
            {
              n = stream->read_char();
              if (n < 0)
                {
                  signal_lisp_error(new EndOfFile(stream));
                  // not reached
                  return NULL;
                }
              string->append_char((BASE_CHAR)n);
              if (escapes == NULL)
                escapes = new BitVector(string->length(), false);
              else
                escapes->ensure_capacity(string->length());
              escapes->set_bit(string->length() - 1);
              continue;
            }
          rt->check_invalid(c, stream);
          if (readtable_case == K_upcase)
            string->append_char(toupper(c));
          else if (readtable_case == K_downcase)
            string->append_char(tolower(c));
          else
            string->append_char(c);
        }
      if (escapes != NULL)
        escapes->ensure_capacity(string->length());

      if (readtable_case == K_invert)
        {
          // "When the readtable case is :INVERT, then if all of the unescaped
          // letters in the extended token are of the same case, those (unescaped)
          // letters are converted to the opposite case." (23.1.2)
          invert(string, escapes);
        }

      return escapes;
    }
  else
    {
      // fundamental-stream
      signal_lisp_error("stream_read_token needs code!");
      // not reached
      return NULL;
    }
}

Value stream_read_atom(Value streamarg, BASE_CHAR c1,
                       Readtable * rt, Thread * thread)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      String * string = new String();
      BitVector * escapes = stream_read_token(streamarg, c1, rt, thread, string);
      if (thread->symbol_value(S_read_suppress) != NIL)
        return NIL;

      // "A potential number cannot contain any escape characters." (2.3.1.1.1)
      if (escapes == NULL)
        {
          long read_base = fixnum_value(thread->symbol_value(S_read_base));
          if (read_base < 2 || read_base > 36)
            {
              String * s = new String("The value of ");
              s->append(the_symbol(S_read_base)->prin1_to_string());
              s->append(" is not of type ");
              s->append(::prin1_to_string(list3(S_integer, FIXNUM_TWO, make_fixnum(36))));
              s->append_char('.');
              return signal_lisp_error(s);
            }
          bool maybe_numeric = false;
          if (c1 == '+' || c1 == '-' || c1 == '.')
            maybe_numeric = true;
          else if (c1 >= '0' && c1 <= '9')
            maybe_numeric = true;
          else if (read_base != 10)
            maybe_numeric = digit_char_p(c1, (int) read_base);
          if (maybe_numeric)
            {
              Value value = make_number(string, (int) read_base, stream);
              if (value != NULL_VALUE)
                return value;
            }
        }

      // not a number
      INDEX len = string->length();
      BASE_CHAR * const data = string->data();
      if (c1 == '.' && escapes == NULL)
        {
          // "If a token consists solely of dots (with no escape characters), then
          // an error of type READER-ERROR is signaled, except in one circumstance:
          // if the token is a single dot and appears in a situation where dotted
          // pair notation permits a dot, then it is accepted as part of such
          // syntax and no error is signaled." 2.3.3
          bool ok = false;
          for (INDEX i = len; i-- > 1;)
            {
              if (data[i] != '.')
                {
                  ok = true;
                  break;
                }
            }
          if (!ok)
            {
              const char * message = (len > 1) ? "Too many dots." : "Dot context error.";
              return signal_lisp_error(new ReaderError(streamarg, message));
            }
        }
      for (INDEX i = 0; i < len; i++)
        {
          BASE_CHAR c = data[i];
          if (c == ':' && (escapes == NULL || escapes->get_bit(i) == 0))
            {
              // unescaped colon
              if (i == 0)
                {
                  if (len == 1)
                    // ":"
                    return current_package(thread)->intern(string, false);
                  else
                    return PACKAGE_KEYWORD->intern(string->substring(1), true);
                }
              if (i < len - 1 && data[i + 1] == ':' && (escapes == NULL || escapes->get_bit(i + 1) == 0))
                {
                  // double colon
                  SimpleString * package_name = string->substring(0, i);
                  SimpleString * symbol_name = string->substring(i + 2);
                  return intern(package_name, symbol_name, false);
                }
              // single colon
              SimpleString * package_name = string->substring(0, i);
              Package * package = find_package(package_name);
              if (!package)
                {
                  String * s = new String("Package \"");
                  s->append(package_name);
                  s->append("\" not found.");
                  return signal_lisp_error(s); // FIXME package error
                }
              SimpleString * symbol_name = string->substring(i + 1);
              Symbol * symbol = package->find_external_symbol(symbol_name);
              if (symbol)
                return make_value(symbol);
              // error!
              if (package->find_internal_symbol(symbol_name))
                {
                  String * s = new String("The symbol ");
                  s->append(symbol_name);
                  s->append(" is not external in package ");
                  s->append(package->name());
                  s->append_char('.');
                  return signal_lisp_error(s); // FIXME reader error
                }
              else
                {
                  String * s = new String("The symbol ");
                  s->append(symbol_name);
                  s->append(" was not found in package ");
                  s->append(package->name());
                  s->append_char('.');
                  return signal_lisp_error(s); // FIXME reader error
                }
            }
        }
      // no colons
      return current_package(thread)->intern(string, false);
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_atom needs code!");
    }
}

BASE_CHAR stream_flush_whitespace(Value streamarg, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      while (true)
        {
          int n = stream->read_char();
          if (n < 0)
            {
              signal_lisp_error(new EndOfFile(stream));
              // not reached
              return 0;
            }
          BASE_CHAR c = (BASE_CHAR) n;
          if (!rt->is_whitespace(c))
            return c;
        }
    }
  else
    {
      // fundamental-stream
      signal_lisp_error("stream_read_delimited_list needs code!");
      // not reached
      return 0;
    }
}

Value stream_read_delimited_list(Value streamarg, BASE_CHAR delimiter, Thread * thread)
{
  Value result = NIL;
  while (true)
    {
      Readtable * rt = current_readtable(thread);
      BASE_CHAR c = stream_flush_whitespace(streamarg, rt);
      if (c == delimiter)
        break;
      Value value = stream_process_char(streamarg, c, rt, thread);
      if (thread->values_length() != 0)
        result = make_cons(value, result);
      else
        thread->clear_values();
    }
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  else
    return CL_nreverse(result);
}

Value stream_read_string(Value streamarg, BASE_CHAR terminator, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      String * string = new String();
      while (true)
        {
          int n = stream->read_char();
          if (n < 0)
            return signal_lisp_error(new EndOfFile(stream));
          BASE_CHAR c = (BASE_CHAR) n;
          unsigned int syntax = rt->syntax(c);
          if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
            {
              n = stream->read_char();
              if (n < 0)
                return signal_lisp_error(new EndOfFile(stream));
              string->append_char((BASE_CHAR)n);
              continue;
            }
          if (c == terminator)
            return make_value(new_simple_string(string));
          // otherwise...
          string->append_char(c);
        }
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_string needs code!");
    }
}

Value stream_read_dispatch_char(Value streamarg, BASE_CHAR dispatch_char,
                                Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      long numarg = -1;
      BASE_CHAR c;
      while (true)
        {
          long n = stream->read_char();
          if (n < 0)
            return signal_lisp_error(new EndOfFile(stream));
          c = (BASE_CHAR) n;
          if (c < '0' || c > '9')
            break;
          if (numarg < 0)
            numarg = 0;
          numarg = numarg * 10 + c - '0';
        }
      Value handler = rt->get_dispatch_macro_character(dispatch_char, c);
      if (handler != NIL)
        {
          TypedObject * function = NULL;
          if (symbolp(handler))
            function = the_symbol(handler)->function();
          else if (typed_object_p(handler))
            function = the_typed_object(handler);
          if (function)
            // REVIEW
            return thread->execute(function, streamarg, make_character(c),
                                   numarg >= 0 ? make_integer(numarg) : NIL);
        }
      // no handler, fall through...
      if (thread->symbol_value(S_read_suppress) != NIL)
        return thread->set_values();
      String * string = new String("No dispatch function defined for #\\");
      string->append_char(c);
      return signal_lisp_error(new ReaderError(streamarg, string));
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_dispatch_char needs code!");
    }
}

Value stream_read_complex(Value streamarg, Thread * thread, Readtable * rt)
{
  Value obj = stream_read(streamarg, true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (consp(obj) && length(obj) == 2)
    return make_complex(xcar(obj), car(xcdr(obj)));
  // error
  String * s = new String("Invalid complex number format");
  //   if (this instanceof FileStream)
  //     {
  //       Pathname p = ((FileStream)this).getPathname();
  //       if (p != null)
  //         {
  //           String namestring = p.getNamestring();
  //           if (namestring != null)
  //             {
  //               string->append(" in #P\"");
  //               string->append(namestring);
  //               string->append_char('"');
  //             }
  //         }
  //       string->append(" at offset ");
  //       string->append(_getFilePosition());
  //     }
  s->append(": #C");
  s->append(::prin1_to_string(obj));
  return signal_lisp_error(new ReaderError(streamarg, s));
}

Value stream_read_array(Value streamarg, Value numarg, Thread * thread, Readtable * rt)
{
  Value obj = stream_read(streamarg, true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (numarg == NIL)
    return signal_lisp_error(new ReaderError(streamarg, "Missing dimensions argument for #A."));
  long rank = fixnum_value(numarg);
  switch (rank)
    {
    case 0:
      return make_value(new ZeroRankArray(T, obj));
    case 1:
      {
        if (listp(obj))
          return make_value(new_simple_vector(obj));
        if (vectorp(obj))
          {
            AbstractVector * v = the_vector(obj);
            const unsigned long len = v->length();
            SimpleVector * sv = new_simple_vector(len);
            for (unsigned long i = 0; i < len; i++)
              sv->aset(i, v->aref(i));
            return make_value(sv);
          }
        String * s = new String("The value ");
        s->append(::write_to_string(obj));
        s->append(" is not a sequence.");
        return signal_lisp_error(new ReaderError(streamarg, s));
      }
    default:
      return make_value(new SimpleArray_T(rank, obj));
    }
}

Value stream_read_bit_vector(Value streamarg, long n, Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      bool suppress = (thread->symbol_value(S_read_suppress) != NIL);
      String * s = new String();
      while (true)
        {
          int n = stream->read_char();
          if (n < 0)
            break;
          BASE_CHAR c = (BASE_CHAR) n;
          if (c == '0' || c == '1')
            s->append_char(c);
          else
            {
              unsigned int syntax = rt->syntax(c);
              if (syntax == SYNTAX_TYPE_WHITESPACE || syntax == SYNTAX_TYPE_TERMINATING_MACRO)
                {
                  stream->unread_char(c);
                  break;
                }
              else if (!suppress)
                {
                  String * message = new String("Illegal element for bit-vector: #\\");
                  Value name = CL_char_name(make_character(c));
                  if (stringp(name))
                    message->append(the_string(name));
                  else
                    message->append_char(c);
                  return signal_lisp_error(new ReaderError(streamarg, message));
                }
            }
        }
      if (suppress)
        return NIL;
      if (n >= 0)
        {
          // numeric arg was supplied
          const long len = s->length();
          if (len == 0)
            {
              if (n > 0)
                {
                  String * message = new String("No element specified for bit vector of length ");
                  message->append_long(n);
                  message->append_char('.');
                  return signal_lisp_error(new ReaderError(streamarg, message));
                }
            }
          if (n > len)
            {
              const BASE_CHAR c = s->char_at(len - 1);
              for (long i = len; i < n; i++)
                s->append_char(c);
            }
          else if (n < len)
            {
              String * message = new String("Bit vector is longer than specified length: #");
              message->append_long(n);
              message->append_char('*');
              message->append(s);
              return signal_lisp_error(new ReaderError(streamarg, message));
            }
        }
      return make_value(new_simple_bit_vector(s));
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_bit_vector needs code!");
    }
}

Value stream_read_radix(Value streamarg, long base, Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      String * string = new String();
      while (true)
        {
          int n = stream->read_char();
          if (n < 0)
            break;
          BASE_CHAR c = (BASE_CHAR) n;
          unsigned int syntax = rt->syntax(c);
          if (syntax == SYNTAX_TYPE_WHITESPACE || syntax == SYNTAX_TYPE_TERMINATING_MACRO)
            {
              stream->unread_char(c);
              break;
            }
          string->append_char(c);
        }
      if (thread->symbol_value(S_read_suppress) != NIL)
        return NIL;
      Value value = make_number(string, base, stream);
      if (value != NULL_VALUE)
        return value;
      return signal_lisp_error(new Error("not a number"));
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_radix needs code!");
    }
}

Value stream_read_pathname(Value streamarg, Thread * thread, Readtable * rt)
{
  Value obj = stream_read(streamarg, true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (stringp(obj))
    return parse_namestring(the_string(obj));
  if (listp(obj))
    return make_pathname_from_list(obj);
  return signal_lisp_error("#P requires a string or list argument.");
}

Value stream_read_structure(Value streamarg, Thread * thread, Readtable * rt)
{
  Value obj = stream_read(streamarg, true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (!listp(obj))
    {
      String * s = new String("Non-list following #S: ");
      s->append(::prin1_to_string(obj));
      return signal_lisp_error(new ReaderError(streamarg, s));
    }
  Value structure_name = car(obj);
  if (!symbolp(structure_name))
    return signal_type_error(structure_name, S_symbol);
  Value structure_class = find_class(structure_name);
  if (!typed_object_p(structure_class) || !the_typed_object(structure_class)->typep(S_structure_class))
    {
      String * s = new String(the_symbol(structure_name)->prin1_to_string());
      s->append(" is not a defined structure type.");
      return signal_lisp_error(new ReaderError(streamarg, s));
    }
  Value constructor = thread->execute(the_symbol(S_defstruct_default_constructor)->function(),
                                      structure_name);
  TypedObject * constructor_function = coerce_to_function(constructor);
  Value tail = xcdr(obj);
  INDEX numargs = length(tail);
  if ((numargs % 2) != 0)
    {
      String * s = new String("Odd number of keyword arguments following #S: ");
      s->append(::prin1_to_string(obj));
      return signal_lisp_error(new ReaderError(streamarg, s));
    }
  Value * args = (Value *) GC_malloc(numargs * sizeof(Value));
  for (INDEX i = 0; i < numargs; i += 2)
    {
      Value key = car(tail);
      if (!keywordp(key))
        key = PACKAGE_KEYWORD->intern(string(key)); // returns symbol, status
      args[i] = key;
      tail = xcdr(tail);
      args[i + 1] = xcar(tail);
      tail = xcdr(tail);
    }
  return funcall(constructor_function, numargs, args, thread);
}

Value stream_read_comma(Value streamarg, Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      long n = stream->read_char();
      if (n < 0)
        return signal_lisp_error(new EndOfFile(stream));
      switch (n)
        {
        case '@':
          return make_cons(S_comma_atsign,
                           make_cons(stream_read(streamarg, true, NIL, true, thread, rt)));
        case '.':
          return make_cons(S_comma_dot, make_cons(stream_read(streamarg, true, NIL, true, thread, rt)));
        default:
          stream->unread_char(n);
          return make_cons(S_comma, make_cons(stream_read(streamarg, true, NIL, true, thread, rt)));
        }
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_comma needs code!");
    }
}

Value stream_read_character_literal(Value streamarg, Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      long n = stream->read_char();
      if (n < 0)
        return signal_lisp_error(new EndOfFile(stream));
      String * string = new String();
      string->append_char((unsigned char)n);
      while (true)
        {
          n = stream->read_char();
          if (n < 0)
            break;
          unsigned char c = (unsigned char) n;
          if (rt->is_whitespace(c))
            {
              stream->unread_char(c);
              break;
            }
          if (c == '(' || c == ')')
            {
              stream->unread_char(c);
              break;
            }
          string->append_char(c);
        }
      if (thread->symbol_value(S_read_suppress) != NIL)
        return NIL;
      if (string->length() == 1)
        return make_character(string->fast_char_at(0));
      Value value = name_to_char(string);
      if (value != NIL)
        return value;
      String * message = new String("Unrecognized character name: \"");
      message->append(string);
      message->append_char('"');
      return signal_lisp_error(message);
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_character_literal needs code!");
    }
}

Value stream_read_binary_data(Value streamarg, INDEX length)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      SimpleArray_UB8_1 * array = new_simple_array_ub8_1(length);
      unsigned char * data = array->data();
      for (INDEX i = 0; i < length; i++)
        data[i] = stream->read_byte();
      return make_value(array);
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("stream_read_binary_data needs code!");
    }
}