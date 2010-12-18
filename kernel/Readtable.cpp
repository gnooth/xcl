// Readtable.cpp
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

#include <ctype.h>      // toupper
#include "lisp.hpp"
#include "primitives.hpp"
#include "reader.hpp"
#include "Environment.hpp"
#include "Package.hpp"
#include "ReaderError.hpp"
#include "Readtable.hpp"

void Readtable::initialize()
{
  _syntax = (unsigned char *) GC_malloc_atomic(CHAR_CODE_LIMIT * sizeof(unsigned char));
  _reader_macro_functions = (Value *) GC_malloc(CHAR_CODE_LIMIT * sizeof(Value));
  _dispatch_tables = (DispatchTable * *) GC_malloc(CHAR_CODE_LIMIT * sizeof(DispatchTable *));

  for (int i = 0; i < CHAR_CODE_LIMIT; i++)
    {
      _syntax[i] = 0;
      _reader_macro_functions[i] = NULL_VALUE;
      _dispatch_tables[i] = NULL;
    }

  _syntax[9]    = SYNTAX_TYPE_WHITESPACE; // tab
  _syntax[10]   = SYNTAX_TYPE_WHITESPACE; // linefeed
  _syntax[12]   = SYNTAX_TYPE_WHITESPACE; // form feed
  _syntax[13]   = SYNTAX_TYPE_WHITESPACE; // return
  _syntax[' ']  = SYNTAX_TYPE_WHITESPACE;

  _syntax['"']  = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax['\''] = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax['(']  = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax[')']  = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax[',']  = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax[';']  = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax['`']  = SYNTAX_TYPE_TERMINATING_MACRO;

  _syntax['#']  = SYNTAX_TYPE_NON_TERMINATING_MACRO;

  _syntax['\\'] = SYNTAX_TYPE_SINGLE_ESCAPE;
  _syntax['|']  = SYNTAX_TYPE_MULTIPLE_ESCAPE;

  _reader_macro_functions[';']  = S_read_comment;
  _reader_macro_functions['"']  = S_read_string;
  _reader_macro_functions['(']  = S_read_list;
  _reader_macro_functions[')']  = S_read_right_paren;
  _reader_macro_functions['\''] = S_read_quote;
  _reader_macro_functions['#']  = S_read_dispatch_char;

  // BACKQUOTE-MACRO and COMMA-MACRO are defined in backquote.lisp.
  _reader_macro_functions['`']  = S_backquote_macro;
  _reader_macro_functions[',']  = S_comma_macro;
//   _reader_macro_functions['`']  = S_read_backquote;
//   _reader_macro_functions[',']  = S_read_comma;

  DispatchTable * dt = new DispatchTable();

  dt->_functions['(']  = S_sharp_left_paren;
  dt->_functions['*']  = S_sharp_star;
  dt->_functions['.']  = S_sharp_dot;
  dt->_functions[':']  = S_sharp_colon;
  dt->_functions['A']  = S_sharp_a;
  dt->_functions['B']  = S_sharp_b;
  dt->_functions['C']  = S_sharp_c;
  dt->_functions['O']  = S_sharp_o;
  dt->_functions['P']  = S_sharp_p;
  dt->_functions['R']  = S_sharp_r;
  dt->_functions['S']  = S_sharp_s;
  dt->_functions['X']  = S_sharp_x;
  dt->_functions['\''] = S_sharp_quote;
  dt->_functions['\\'] = S_sharp_backslash;
  dt->_functions['|']  = S_sharp_vertical_bar;
  dt->_functions[')']  = S_sharp_illegal;
  dt->_functions['<']  = S_sharp_illegal;
  dt->_functions[' ']  = S_sharp_illegal;
  dt->_functions[8]    = S_sharp_illegal; // backspace
  dt->_functions[9]    = S_sharp_illegal; // tab
  dt->_functions[10]   = S_sharp_illegal; // newline, linefeed
  dt->_functions[12]   = S_sharp_illegal; // page
  dt->_functions[13]   = S_sharp_illegal; // return

  dt->_functions['+']  = S_read_conditional;
  dt->_functions['-']  = S_read_conditional;

  _dispatch_tables['#'] = dt;

  _readtable_case = K_upcase;
}

Readtable::Readtable(Readtable * rt) : TypedObject(WIDETAG_READTABLE)
{
  _syntax = (unsigned char *) GC_malloc_atomic(CHAR_CODE_LIMIT * sizeof(unsigned char));
  _reader_macro_functions = (Value *) GC_malloc(CHAR_CODE_LIMIT * sizeof(Value));
  _dispatch_tables = (DispatchTable * *) GC_malloc(CHAR_CODE_LIMIT * sizeof(DispatchTable *));
  for (int i = 0; i < CHAR_CODE_LIMIT; i++)
    {
      _syntax[i] = rt->_syntax[i];
      _reader_macro_functions[i] = rt->_reader_macro_functions[i];
      DispatchTable * dt = rt->_dispatch_tables[i];
      if (dt != NULL)
        _dispatch_tables[i] = new DispatchTable(dt);
      else
        _dispatch_tables[i] = NULL;
    }
  _readtable_case = rt->_readtable_case;
}

void Readtable::copy_readtable(Readtable * from, Readtable * to)
{
  for (int i = 0; i < CHAR_CODE_LIMIT; i++)
    {
      to->_syntax[i] = from->_syntax[i];
      to->_reader_macro_functions[i] = from->_reader_macro_functions[i];
      DispatchTable * dt = from->_dispatch_tables[i];
      if (dt != NULL)
        to->_dispatch_tables[i] = new DispatchTable(dt);
      else
        to->_dispatch_tables[i] = NULL;
    }
  to->_readtable_case = from->_readtable_case;
}

bool Readtable::typep(Value type) const
{
  return (type == S_readtable || type == S_atom || type == T
          || type == C_readtable || type == C_t);
}

Value Readtable::get_macro_character(BASE_CHAR c)
{
  Value function = _reader_macro_functions[c];
  Value non_terminating_p;
  if (function != NULL_VALUE)
    {
      if (_syntax[c] == SYNTAX_TYPE_NON_TERMINATING_MACRO)
        non_terminating_p = T;
      else
        non_terminating_p = NIL;
    }
  else
    {
      function = NIL;
      non_terminating_p = NIL;
    }
  return current_thread()->set_values(function, non_terminating_p);
}

void Readtable::set_macro_character(BASE_CHAR c, Value function, Value non_terminating_p)
{
  _reader_macro_functions[c] = function;
  unsigned char syntax;
  if (non_terminating_p != NIL)
    syntax = SYNTAX_TYPE_NON_TERMINATING_MACRO;
  else
    syntax = SYNTAX_TYPE_TERMINATING_MACRO;
  _syntax[c] = syntax;
}

void Readtable::make_dispatch_macro_character(BASE_CHAR c, Value non_terminating_p)
{
  unsigned char syntax;
  if (non_terminating_p != NIL)
    syntax = SYNTAX_TYPE_NON_TERMINATING_MACRO;
  else
    syntax = SYNTAX_TYPE_TERMINATING_MACRO;
  // REVIEW synchronization
  _syntax[c] = syntax;
  _reader_macro_functions[c] = S_read_dispatch_char;
  _dispatch_tables[c] = new DispatchTable();
}

Value Readtable::get_dispatch_macro_character(BASE_CHAR dispchar, BASE_CHAR subchar)
{
  DispatchTable * dt = _dispatch_tables[dispchar];
  if (!dt)
    {
      String * s = new String(::prin1_to_string(make_character(dispchar)));
      s->append(" is not a dispatch macro character.");
      return signal_lisp_error(s);
    }
  Value function = dt->_functions[toupper(subchar)];
  return (function != NULL_VALUE) ? function : NIL;
}

void Readtable::set_dispatch_macro_character(BASE_CHAR dispchar, BASE_CHAR subchar, Value function)
{
  DispatchTable * dt = _dispatch_tables[dispchar];
  if (!dt)
    {
      String * s = new String(::prin1_to_string(make_character(dispchar)));
      s->append(" is not a dispatch character.");
      signal_lisp_error(s);
      // not reached
      return;
    }
  dt->_functions[toupper(subchar)] = function;
}

void Readtable::check_invalid(BASE_CHAR c, Stream * stream)
{
  // "... no mechanism is provided for changing the constituent trait of a
  // character." (2.1.4.2)
  switch (c)
    {
    case 8:
    case 9:
    case 10:
    case 12:
    case 13:
    case 32:
    case 127:
      {
        String * s = new String("Invalid character");
        Value name = CL_char_name(make_character(c));
        if (stringp(name))
          {
            s->append(" #\\");
            s->append(the_string(name));
          }
        signal_lisp_error(new ReaderError(stream, s));
      }
    }
}

// ### readtablep object => generalized-boolean
Value CL_readtablep(Value arg)
{
  return readtablep(arg) ? T : NIL;
}

// ### copy-readtable &optional from-readtable to-readtable => readtable
Value CL_copy_readtable(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return make_value(new Readtable(check_readtable(current_thread()->symbol_value(S_current_readtable))));
    case 1:
      {
        Readtable * rt;
        if (args[0] == NIL)
          rt = check_readtable(current_thread()->symbol_value(S_standard_readtable));
        else
          rt = check_readtable(args[0]);
        return make_value(new Readtable(rt));
      }
    case 2:
      {
        Readtable * from;
        if (args[0] == NIL)
          from = check_readtable(current_thread()->symbol_value(S_standard_readtable));
        else
          from = check_readtable(args[0]);
        if (args[1] == NIL)
          return make_value(new Readtable(from));
        Readtable * to = check_readtable(args[1]);
        Readtable::copy_readtable(from, to);
        return make_value(to);
      }
    default:
      return wrong_number_of_arguments(S_copy_readtable, numargs, 0, 2);
    }
}

// ### readtable-case readtable => mode
Value CL_readtable_case(Value arg)
{
  return check_readtable(arg)->readtable_case();
}

// ### set-readtable-case readtable new-mode => new-mode
Value SYS_set_readtable_case(Value arg1, Value arg2)
{
  Readtable * rt = check_readtable(arg1);
  if (arg2 == K_upcase || arg2 == K_downcase || arg2 == K_invert || arg2 == K_preserve)
    {
      rt->set_readtable_case(arg2);
      return arg2;
    }
  return signal_type_error(arg2, list5(S_member, K_upcase, K_downcase, K_invert, K_preserve));
}

// ### get-macro-character char &optional readtable => function, non-terminating-p
Value CL_get_macro_character(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      {
        unsigned char c = char_value(args[0]);
        Readtable * rt = current_readtable(current_thread());
        return rt->get_macro_character(c);
      }
    case 2:
      {
        unsigned char c = char_value(args[0]);
        Readtable * rt;
        if (args[1] == NIL)
          rt = check_readtable(current_thread()->symbol_value(S_standard_readtable));
        else
          rt = check_readtable(args[1]);
        return rt->get_macro_character(c);
      }
    default:
      return wrong_number_of_arguments(S_get_macro_character, numargs, 1, 2);
    }
}

// ### set-macro-character char new-function &optional non-terminating-p readtable => t
Value CL_set_macro_character(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 4)
    wrong_number_of_arguments(S_set_macro_character, numargs, 2, 4);
  unsigned char c = char_value(args[0]);
  Value designator = args[1];
  if (!functionp(designator) && !symbolp(designator))
    return signal_type_error(designator, S_function_designator);
  Value non_terminating_p = (numargs > 2) ? args[2] : NIL;
  Readtable * rt;
  if (numargs > 3)
    rt = check_readtable(args[3]);
  else
    rt = current_readtable(current_thread());
  rt->set_macro_character(c, designator, non_terminating_p);
  return T;
}

// ### make-dispatch-macro-character char &optional non-terminating-p readtable => t
Value CL_make_dispatch_macro_character(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 3)
    wrong_number_of_arguments(S_make_dispatch_macro_character, numargs, 2, 3);
  BASE_CHAR c = char_value(args[0]);
  Value non_terminating_p = (numargs > 1) ? args[1] : NIL;
  Readtable * rt;
  if (numargs == 3)
    rt = check_readtable(args[2]);
  else
    rt = current_readtable(current_thread());
  rt->make_dispatch_macro_character(c, non_terminating_p);
  return T;
}

// ### get-dispatch-macro-character disp-char sub-char &optional readtable => function
Value CL_get_dispatch_macro_character(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 3)
    wrong_number_of_arguments(S_get_dispatch_macro_character, numargs, 2, 3);
  BASE_CHAR dispchar = char_value(args[0]);
  BASE_CHAR subchar = char_value(args[1]);
  Readtable * rt;
  if (numargs == 3 && args[2] != NIL)
    rt = check_readtable(args[2]);
  else
    rt = current_readtable(current_thread());
  return rt->get_dispatch_macro_character(dispchar, subchar);
}

// ### set-dispatch-macro-character disp-char sub-char new-function &optional readtable => t
Value CL_set_dispatch_macro_character(unsigned int numargs, Value args[])
{
  if (numargs < 3 || numargs > 4)
    wrong_number_of_arguments(S_set_dispatch_macro_character, numargs, 2, 3);
  BASE_CHAR dispchar = char_value(args[0]);
  BASE_CHAR subchar = char_value(args[1]);
  // REVIEW validate args[2]?
  Readtable * rt;
  if (numargs == 4 && args[3] != NIL)
    rt = check_readtable(args[3]);
  else
    rt = current_readtable(current_thread());
  rt->set_dispatch_macro_character(dispchar, subchar, args[2]);
  return T;
}

void Readtable::set_syntax_from_char(BASE_CHAR from_char, BASE_CHAR to_char, Readtable * rt)
{
  set_syntax(to_char, rt->syntax(from_char));
  set_reader_macro_function(to_char, rt->reader_macro_function(from_char));
  // "If the character is a dispatching macro character, its entire dispatch
  // table of reader macro functions is copied."
  if (rt->_dispatch_tables[from_char] != NULL)
    _dispatch_tables[to_char] = new DispatchTable(rt->_dispatch_tables[from_char]);
  else
    _dispatch_tables[to_char] = NULL;
}

// ### set-syntax-from-char to-char from-char &optional to-readtable from-readtable => t
Value CL_set_syntax_from_char(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 4)
    wrong_number_of_arguments(S_set_syntax_from_char, numargs, 2, 4);
  BASE_CHAR to_char = char_value(args[0]);
  BASE_CHAR from_char = char_value(args[1]);
  Readtable * to_rt;
  if (numargs > 2)
    to_rt = check_readtable(args[2]);
  else
    to_rt = current_readtable(current_thread());
  Readtable * from_rt;
  if (numargs == 4 && args[3] != NIL)
    from_rt = check_readtable(args[3]);
  else
    from_rt = check_readtable(current_thread()->symbol_value(S_standard_readtable));
  // FIXME synchronization
  to_rt->set_syntax_from_char(from_char, to_char, from_rt);
  return T;
}

// ### read-list stream character => value
Value SYS_read_list(Value streamarg, Value character)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_list(streamarg, false, thread, rt);
}

// ### read-comment stream character => no values
Value SYS_read_comment(Value streamarg, Value character)
{
  while (true)
    {
      int n = check_stream(streamarg)->read_char();
      if (n < 0)
        break;
      if (n == '\n')
        break;
    }
  return current_thread()->set_values();
}

// ### read-backquote stream character => value
Value SYS_read_backquote(Value streamarg, Value ignored)
{
//   Stream * stream = check_stream(streamarg);
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return make_cons(S_backquote, make_cons(stream_read(streamarg, true, NIL, true, thread, rt)));
}

// ### read-comma stream character => value
Value SYS_read_comma(Value streamarg, Value ignored)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_comma(streamarg, thread, rt);
}

// ### read-dispatch-char stream character => value
Value SYS_read_dispatch_char(Value streamarg, Value character)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_dispatch_char(streamarg, char_value(character), thread, rt);
}

// ### read-quote stream character => value
Value SYS_read_quote(Value streamarg, Value ignored)
{
  Thread * thread = current_thread();
//   Readtable * rt = current_readtable(thread);
  Readtable * rt = current_readtable(thread);
  return make_cons(S_quote,
                   make_cons(stream_read(streamarg, true, NIL, true, thread, rt)));
}

// ### read-right-paren stream character => value
Value SYS_read_right_paren(Value streamarg, Value ignored)
{
  Stream * stream = check_stream(streamarg);
  Value position = stream->file_position();
  String * message = new String("Unmatched right parenthesis");
  if (position != NIL)
    {
      message->append(" at offset ");
      message->append(::write_to_string(position));
    }
  message->append(".");
  return signal_lisp_error(new ReaderError(check_stream(streamarg),
                                           message));
}

// ### read-string stream character => value
Value SYS_read_string(Value streamarg, Value character)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_string(streamarg, char_value(character), rt);
}

// ### sharp-a stream sub-char numarg => value
Value SYS_sharp_a(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_array(streamarg, numarg, thread, rt);
}

// ### sharp-b stream sub-char numarg => value
Value SYS_sharp_b(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_radix(streamarg, 2, thread, rt);
}

// ### sharp-backslash stream sub-char numarg => value
Value SYS_sharp_backslash(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_character_literal(streamarg, thread, rt);
}

// ### sharp-c stream sub-char numarg => value
Value SYS_sharp_c(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_complex(streamarg, thread, rt);
}

// ### sharp-colon stream sub-char numarg => value
Value SYS_sharp_colon(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_symbol(streamarg, rt);
}

// ### sharp-dot stream sub-char numarg => value
Value SYS_sharp_dot(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_read_eval) != NIL)
    {
      Readtable * rt = current_readtable(thread);
      return eval(stream_read(streamarg, true, NIL, true, thread, rt), new Environment(), thread);
    }
  Stream * stream = check_stream(streamarg);
  String * s = new String("Can't read #. when ");
  s->append(the_symbol(S_read_eval)->prin1_to_string());
  s->append(" is false.");
  return signal_lisp_error(new ReaderError(stream, s->as_c_string()));
}

// ### sharp-illegal stream sub-char numarg => value
Value SYS_sharp_illegal(Value streamarg, Value subchar, Value numarg)
{
  Stream * stream = check_stream(streamarg);
  String * s = new String("Illegal # macro character: #\\");
  Value name = CL_char_name(subchar);
  if (stringp(name))
    s->append(the_string(name));
  else
    s->append_char(char_value(subchar));
  return signal_lisp_error(new ReaderError(stream, s));
}

// ### sharp-left-paren stream sub-char numarg => value
Value SYS_sharp_left_paren(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_read_suppress) != NIL)
    {
      Readtable * rt = current_readtable(thread);
      stream_read_list(streamarg, true, thread, rt);
      return NIL;
    }
  Readtable * rt = current_readtable(thread);
  if (numarg != NIL && thread->symbol_value(S_backquote_count) == FIXNUM_ZERO)
    return stream_read_vector(streamarg, check_index(numarg), thread, rt);
  Value list = stream_read_list(streamarg, true, thread, rt);
  if (thread->symbol_value(S_backquote_count) == FIXNUM_ZERO)
    {
      if (numarg != NIL)
        {
          INDEX len = check_index(numarg);
          SimpleVector * vector = new_simple_vector(len);
          for (INDEX i = 0; i < len; i++)
            {
              vector->inline_xaset(i, car(list));
              if (cdr(list) != NIL)
                list = xcdr(list);
            }
          return make_value(vector);
        }
      return make_value(new_simple_vector(list));
    }
  return make_cons(thread->symbol_value(S_backquote_vector_flag), list);
}

// ### sharp-o stream sub-char numarg => value
Value SYS_sharp_o(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_radix(streamarg, 8, thread, rt);
}

// ### sharp-p stream sub-char numarg => value
Value SYS_sharp_p(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_pathname(streamarg, thread, rt);
}

// ### sharp-quote stream sub-char numarg => value
Value SYS_sharp_quote(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
//   Readtable * rt = current_readtable(thread);
  Readtable * rt = current_readtable(thread);
  return make_cons(S_function,
                   make_cons(stream_read(streamarg, true, NIL, true, thread, rt)));
}

// ### sharp-r stream sub-char numarg => value
Value SYS_sharp_r(Value streamarg, Value subchar, Value numarg)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = check_stream(streamarg);
      Thread * thread = current_thread();
      Readtable * rt = current_readtable(thread);
      if (fixnump(numarg))
        {
          long radix = xlong(numarg);
          if (radix >= 2 && radix <= 36)
            return stream_read_radix(streamarg, radix, thread, rt);
        }
      // illegal radix
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
        }
      if (thread->symbol_value(S_read_suppress) != NIL)
        return NIL;
      String * string = new String("Illegal radix for #R: ");
      string->append(::prin1_to_string(numarg));
      return signal_lisp_error(new ReaderError(stream, string));
    }
  else
    {
      // fundamental-stream
      return signal_lisp_error("Readtable::SYS_sharp_r needs code!");
    }
}

// ### sharp-s stream sub-char numarg => value
Value SYS_sharp_s(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_structure(streamarg,thread, rt);
}

// ### sharp-star stream sub-char numarg => value
Value SYS_sharp_star(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_bit_vector(streamarg, numarg == NIL ? -1 : fixnum_value(numarg),
                                thread, rt);
}

// ### sharp-vertical-bar stream sub-char numarg => value
Value SYS_sharp_vertical_bar(Value streamarg, Value subchar, Value numarg)
{
  check_stream(streamarg)->skip_balanced_comment();
  return current_thread()->set_values();
}

// ### sharp-x stream sub-char numarg => value
Value SYS_sharp_x(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  Readtable * rt = current_readtable(thread);
  return stream_read_radix(streamarg, 16, thread, rt);
}
