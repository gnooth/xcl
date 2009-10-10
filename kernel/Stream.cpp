// Stream.cpp
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

#include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
#include "runtime.h"
#include "primitives.hpp"
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
    _element_type(S_character), _fd(-1), _offset(0), _charpos(0), _open(true)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

Stream::Stream(long widetag, Direction direction)
  : TypedObject(widetag), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(-1), _offset(0), _charpos(0), _open(true)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

Stream::Stream(long widetag, Direction direction, int fd)
  : TypedObject(widetag), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(fd), _offset(0), _charpos(0), _open(true)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

Stream::Stream(Direction direction, int fd)
  : TypedObject(WIDETAG_STREAM), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(fd), _offset(0), _charpos(0), _open(true)
{
#ifdef WIN32
  _h = INVALID_HANDLE_VALUE;
#endif
}

#ifdef WIN32
Stream::Stream(Direction direction, HANDLE h)
  : TypedObject(WIDETAG_STREAM), _last_char(-1), _direction(direction),
    _element_type(S_character), _fd(-1), _h(h), _offset(0), _charpos(0), _open(true)
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

Value Stream::process_char(BASE_CHAR c, Readtable * rt, Thread * thread)
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
      return thread->execute(function, make_value(this), make_character(c));
    }
  // no handler
  return read_atom(c, rt, thread);
}

Value Stream::read(bool eof_error_p, Value eof_value, bool recursive_p, Thread * thread,
                   Readtable * rt)
{
  Value result = read_preserving_whitespace(eof_error_p, eof_value, recursive_p, thread, rt);
  if (result != eof_value && !recursive_p)
    {
      int c = read_char();
      if (c >= 0)
        {
          if (!rt->is_whitespace(c))
            unread_char(c);
        }
    }
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  else
    return result;
}

Value Stream::read_preserving_whitespace(bool eof_error_p, Value eof_value,
                                         bool recursive_p, Thread * thread,
                                         Readtable * rt)
{
  void * last_special_binding = NULL;
  if (!recursive_p)
    {
      last_special_binding = thread->last_special_binding();
      thread->bind_special(S_sharp_equal_alist, NIL);
    }
  Value value;
  while (true)
    {
      int n = read_char();
      if (n < 0)
        {
          if (eof_error_p)
            return signal_lisp_error(new EndOfFile(this));
          else
            return eof_value;
        }
      BASE_CHAR c = (BASE_CHAR) n;
      if (!rt->is_whitespace(c))
        {
          value = process_char(c, rt, thread);
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

Value Stream::read_list(bool require_proper_list, Thread * thread, Readtable * rt)
{
  Value first = NULL_VALUE;
  Value last = NULL_VALUE;
  while (true)
    {
      long n = read_char();
      if (n < 0)
        return signal_lisp_error(new EndOfFile(this));
      BASE_CHAR c = (BASE_CHAR) n;
      if (rt->is_whitespace(c))
        continue;
      if (c == ')')
        return first == NULL_VALUE ? NIL : first;
      if (c == '.')
        {
          int n2 = read_char();
          if (n2 < 0)
            return signal_lisp_error(new EndOfFile(this));
          if (is_delimiter(n2))
            {
              if (last == NULL_VALUE)
                {
                  if (thread->symbol_value(S_read_suppress) != NIL)
                    return NIL;
                  else
                    // FIXME reader error
                    return signal_lisp_error(new ReaderError(this, "Nothing appears before . in list."));
                }
              unread_char(n2);
              Value obj = read(true, NIL, true, thread, rt);
              if (require_proper_list)
                {
                  if (!listp(obj))
                    {
                      String * string = new String("The value ");
                      string->append(::prin1_to_string(obj));
                      string->append(" is not of type ");
                      string->append(the_symbol(S_list)->prin1_to_string());
                      string->append_char('.');
                      return signal_lisp_error(new ReaderError(this, string));
                    }
                }
              setcdr(last, obj);
              continue;
            }
          // normal token beginning with '.'
          unread_char(n2);
        }
      Value obj = process_char(c, rt, thread);
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

Value Stream::read_vector(INDEX size, Thread * thread, Readtable * rt)
{
  // "If an unsigned decimal integer appears between the # and (, it specifies
  // explicitly the length of the vector. The consequences are undefined if the
  // number of objects specified before the closing ) exceeds the unsigned
  // decimal integer. If the number of objects supplied before the closing ) is
  // less than the unsigned decimal integer but greater than zero, the last
  // object is used to fill all remaining elements of the vector. The
  // consequences are undefined if the unsigned decimal integer is non-zero and
  // number of objects supplied before the closing ) is zero." 2.4.8.3
  SimpleVector * vector = new_simple_vector(size);
  INDEX index = 0;
  Value last = NIL;
  while (true)
    {
      long n = read_char();
      if (n < 0)
        return signal_lisp_error(new EndOfFile(this));
      BASE_CHAR c = (BASE_CHAR) n;
      if (rt->is_whitespace(c))
        continue;
      if (c == ')')
        break;
      if (c == '.')
        {
          int n2 = read_char();
          if (n2 < 0)
            return signal_lisp_error(new EndOfFile(this));
          if (is_delimiter(n2))
            return signal_lisp_error(new ReaderError(this, "The token \".\" is not allowed here."));
          // normal token beginning with '.'
          unread_char(n2);
        }
      Value obj = process_char(c, rt, thread);
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

Value Stream::read_symbol(Readtable * rt)
{
  Value readtable_case = rt->readtable_case();
  String * string = new String();
  while (1)
    {
      int n = read_char();
      if (n < 0)
        break;
      BASE_CHAR c = (BASE_CHAR) n;
      if (rt->is_whitespace(c))
        {
          unread_char(c);
          break;
        }
      unsigned int syntax = rt->syntax(c);
      if (syntax == SYNTAX_TYPE_TERMINATING_MACRO)
        {
          unread_char(c);
          break;
        }
      if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
        {
          n = read_char();
          if (n < 0)
            return signal_lisp_error(new EndOfFile(this));
          string->append_char((BASE_CHAR)n);
          continue;
        }
      if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
        {
          string->append(read_multiple_escape(rt));
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

Value Stream::read_complex(Thread * thread, Readtable * rt)
{
  Value obj = read(true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (consp(obj) && length(obj) == 2)
    return make_complex(xcar(obj), car(xcdr(obj)));
  // Error.
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
  return signal_lisp_error(new ReaderError(this, s));
}

Value Stream::read_array(Value numarg, Thread * thread, Readtable * rt)
{
  Value obj = read(true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (numarg == NIL)
    return signal_lisp_error(new ReaderError(this, "Missing dimensions argument for #A."));
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
        return signal_lisp_error(new ReaderError(this, s));
      }
    default:
      return make_value(new SimpleArray_T(rank, obj));
    }
}

String * Stream::read_multiple_escape(Readtable * rt)
{
  String * string = new String();
  while (true)
    {
      long n = read_char();
      if (n < 0)
        {
          signal_lisp_error(new EndOfFile(this));
          // Not reached.
          return NULL;
        }
      BASE_CHAR c = (BASE_CHAR) n;
      unsigned int syntax = rt->syntax(c);
      if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
        {
          n = read_char();
          if (n < 0)
            {
              signal_lisp_error(new EndOfFile(this));
              // Not reached.
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

BitVector * Stream::read_token(unsigned char c1, Readtable * rt, Thread * thread, String * string)
{
  BitVector * escapes = NULL;
  Value readtable_case = rt->readtable_case();
  unsigned int syntax = rt->syntax(c1);
  if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
    {
      string->append(read_multiple_escape(rt));
      escapes = new BitVector(string->length(), false);
      escapes->fill(FIXNUM_ONE);
    }
  else if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
    {
      int n = read_char();
      if (n < 0)
        {
          signal_lisp_error(new EndOfFile(this));
          // not reached
          return NULL;
        }
      string->append_char((BASE_CHAR)n);
      escapes = new BitVector(1, false);
      escapes->set_bit(0);
    }
  else
    {
      rt->check_invalid(c1, this);
      if (readtable_case == K_upcase)
        string->append_char(toupper(c1));
      else if (readtable_case == K_downcase)
        string->append_char(tolower(c1));
      else
        string->append_char(c1);
    }
  while (true)
    {
      int n = read_char();
      if (n < 0)
        break;
      BASE_CHAR c = (BASE_CHAR) n;
      if (rt->is_whitespace(c))
        {
          unread_char(c);
          break;
        }
      syntax = rt->syntax(c);
      if (syntax == SYNTAX_TYPE_TERMINATING_MACRO)
        {
          unread_char(c);
          break;
        }
      if (syntax == SYNTAX_TYPE_MULTIPLE_ESCAPE)
        {
          INDEX begin = string->length();
          string->append(read_multiple_escape(rt));
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
          n = read_char();
          if (n < 0)
            {
              signal_lisp_error(new EndOfFile(this));
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
      rt->check_invalid(c, this);
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

Value Stream::read_atom(BASE_CHAR c1, Readtable * rt, Thread * thread)
{
  String * string = new String();
  BitVector * escapes = read_token(c1, rt, thread, string);
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
          Value value = make_number(string, (int) read_base, this);
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
          return signal_lisp_error(new ReaderError(this, message));
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

Value Stream::read_bit_vector(long n, Thread * thread, Readtable * rt)
{
  bool suppress = (thread->symbol_value(S_read_suppress) != NIL);
  String * s = new String();
  while (true)
    {
      int n = read_char();
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
              unread_char(c);
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
              return signal_lisp_error(new ReaderError(this, message));
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
              return signal_lisp_error(new ReaderError(this, message));
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
          return signal_lisp_error(new ReaderError(this, message));
        }
    }
  return make_value(new SimpleBitVector(s));
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

AbstractString * Stream::read_string(BASE_CHAR terminator, Readtable * rt)
{
  String * string = new String();
  while (true)
    {
      int n = read_char();
      if (n < 0)
        {
          signal_lisp_error(new EndOfFile(this));
          // not reached
          return NULL;
        }
      BASE_CHAR c = (BASE_CHAR) n;
      unsigned int syntax = rt->syntax(c);
      if (syntax == SYNTAX_TYPE_SINGLE_ESCAPE)
        {
          n = read_char();
          if (n < 0)
            {
              signal_lisp_error(new EndOfFile(this));
              // Not reached.
              return NULL;
            }
          string->append_char((BASE_CHAR)n);
          continue;
        }
      if (c == terminator)
        return new_simple_string(string);
      // otherwise...
      string->append_char(c);
    }
}

Value Stream::read_dispatch_char(BASE_CHAR dispatch_char, Thread * thread, Readtable * rt)
{
  long numarg = -1;
  BASE_CHAR c;
  while (true)
    {
      long n = read_char();
      if (n < 0)
        return signal_lisp_error(new EndOfFile(this));
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
          return thread->execute(function, make_value(this), make_character(c),
                                 numarg >= 0 ? make_number(numarg) : NIL);
    }
  // no handler, fall through...
  if (thread->symbol_value(S_read_suppress) != NIL)
    return thread->set_values();
  String * string = new String("No dispatch function defined for #\\");
  string->append_char(c);
  return signal_lisp_error(new ReaderError(this, string));
}

Value Stream::read_radix(long base, Thread * thread, Readtable * rt)
{
  String * string = new String();
  while (true)
    {
      int n = read_char();
      if (n < 0)
        break;
      BASE_CHAR c = (BASE_CHAR) n;
      unsigned int syntax = rt->syntax(c);
      if (syntax == SYNTAX_TYPE_WHITESPACE || syntax == SYNTAX_TYPE_TERMINATING_MACRO)
        {
          unread_char(c);
          break;
        }
      string->append_char(c);
    }
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  Value value = make_number(string, base, this);
  if (value != NULL_VALUE)
    return value;
  return signal_lisp_error(new Error("not a number"));
}

Value Stream::read_pathname(Thread * thread, Readtable * rt)
{
  Value obj = read(true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (stringp(obj))
    return parse_namestring(the_string(obj));
  return signal_lisp_error("#P requires a string argument.");
}

Value Stream::read_structure(Thread * thread, Readtable * rt)
{
  Value obj = read(true, NIL, true, thread, rt);
  if (thread->symbol_value(S_read_suppress) != NIL)
    return NIL;
  if (!listp(obj))
    {
      String * s = new String("Non-list following #S: ");
      s->append(::prin1_to_string(obj));
      return signal_lisp_error(new ReaderError(this, s));
    }
  Value structure_name = car(obj);
  if (!symbolp(structure_name))
    return signal_type_error(structure_name, S_symbol);
  Value structure_class = find_class(structure_name);
  if (!typed_object_p(structure_class) || !the_typed_object(structure_class)->typep(S_structure_class))
    {
      String * s = new String(the_symbol(structure_name)->prin1_to_string());
      s->append(" is not a defined structure type.");
      return signal_lisp_error(new ReaderError(this, s));
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
      return signal_lisp_error(new ReaderError(this, s));
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

Value Stream::read_comma(Thread * thread, Readtable * rt)
{
  long n = read_char();
  if (n < 0)
    return signal_lisp_error(new EndOfFile(this));
  switch (n)
    {
    case '@':
      return make_cons(S_comma_atsign,
                       make_cons(read(true, NIL, true, thread, rt)));
    case '.':
      return make_cons(S_comma_dot, make_cons(read(true, NIL, true, thread, rt)));
    default:
      unread_char(n);
      return make_cons(S_comma, make_cons(read(true, NIL, true, thread, rt)));
    }
}

Value Stream::read_character_literal(Thread * thread, Readtable * rt)
{
  long n = read_char();
  if (n < 0)
    return signal_lisp_error(new EndOfFile(this));
  String * string = new String();
  string->append_char((unsigned char)n);
  while (true)
    {
      n = read_char();
      if (n < 0)
        break;
      unsigned char c = (unsigned char) n;
      if (rt->is_whitespace(c))
        {
          unread_char(c);
          break;
        }
      if (c == '(' || c == ')')
        {
          unread_char(c);
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

BASE_CHAR Stream::flush_whitespace(Readtable * rt)
{
  while (true)
    {
      int n = read_char();
      if (n < 0)
        {
          signal_lisp_error(new EndOfFile(this));
          // not reached
          return 0;
        }
      BASE_CHAR c = (BASE_CHAR) n;
      if (!rt->is_whitespace(c))
        return c;
    }
}

Value Stream::read_delimited_list(BASE_CHAR delimiter, Thread * thread)
{
  Value result = NIL;
  while (true)
    {
      Readtable * rt = check_readtable(thread->symbol_value(S_current_readtable));
      BASE_CHAR c = flush_whitespace(rt);
      if (c == delimiter)
        break;
      Value value = process_char(c, rt, thread);
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
    return make_number(the_string(arg)->length());
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
  return make_number(check_stream(stream)->charpos());
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
  Readtable * rt = check_readtable(thread->symbol_value(S_current_readtable));
  return stream->read(eof_error_p, eof_value, recursive_p, thread, rt);
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
  Readtable * rt = check_readtable(thread->symbol_value(S_current_readtable));
  return stream->read_preserving_whitespace(eof_error_p, eof_value, recursive_p, thread, rt);
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
  return stream->read_delimited_list(c, thread);
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

// ### write-8-bits byte stream => byte
Value SYS_write_8_bits(Value byte, Value stream)
{
  BYTE n = (BYTE) check_index(byte, 0, 255);
  check_stream(stream)->write_byte(n);
  return byte;
}

// ### read-8-bits stream &optional eof-error-p eof-value => byte
Value SYS_read_8_bits(unsigned int numargs, Value args[])
{
  if (numargs >= 1 && numargs <= 3)
    {
      Stream * stream = check_stream(args[0]);
      long n = stream->read_byte();
      if (n >= 0)
        return make_fixnum(n);
      // eof
      switch (numargs)
        {
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
  return wrong_number_of_arguments(S_read_8_bits, numargs, 1, 3);
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
