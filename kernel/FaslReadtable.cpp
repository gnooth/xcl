// FaslReadtable.cpp
//
// Copyright (C) 2008-2010 Peter Graves <gnooth@gmail.com>
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
#include "reader.hpp"
#include "Environment.hpp"
#include "Package.hpp"
#include "FaslReadtable.hpp"
#include "ReaderError.hpp"

Readtable * FASL_READTABLE;

void FaslReadtable::initialize()
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

  _reader_macro_functions[';']  = S_fasl_read_comment;
  _reader_macro_functions['"']  = S_fasl_read_string;
  _reader_macro_functions['(']  = S_fasl_read_list;
  _reader_macro_functions[')']  = S_fasl_read_right_paren;
  _reader_macro_functions['\''] = S_fasl_read_quote;
  _reader_macro_functions['#']  = S_fasl_read_dispatch_char;

  // BACKQUOTE-MACRO and COMMA-MACRO are defined in backquote.lisp.
  _reader_macro_functions['`']  = S_backquote_macro;
  _reader_macro_functions[',']  = S_comma_macro;

  DispatchTable * dt = new DispatchTable();

  dt->_functions['$']  = S_fasl_sharp_dollar; // REVIEW fasl-read-uninterned-symbol
  dt->_functions['%']  = S_fasl_sharp_percent;
  dt->_functions['(']  = S_fasl_sharp_left_paren;
  dt->_functions['*']  = S_fasl_sharp_star;
  dt->_functions['.']  = S_fasl_sharp_dot;
  dt->_functions[':']  = S_fasl_sharp_colon;
  dt->_functions['A']  = S_fasl_sharp_a;
  dt->_functions['B']  = S_fasl_sharp_b;
  dt->_functions['C']  = S_fasl_sharp_c;
  dt->_functions['O']  = S_fasl_sharp_o;
  dt->_functions['P']  = S_fasl_sharp_p;
  dt->_functions['R']  = S_fasl_sharp_r;
  dt->_functions['S']  = S_fasl_sharp_s;
  dt->_functions['X']  = S_fasl_sharp_x;
  dt->_functions['\''] = S_fasl_sharp_quote;
  dt->_functions['\\'] = S_fasl_sharp_backslash;
  dt->_functions['|']  = S_fasl_sharp_vertical_bar;
  dt->_functions[')']  = S_fasl_sharp_illegal;
  dt->_functions['<']  = S_fasl_sharp_illegal;
  dt->_functions[' ']  = S_fasl_sharp_illegal;
  dt->_functions[8]    = S_fasl_sharp_illegal; // backspace
  dt->_functions[9]    = S_fasl_sharp_illegal; // tab
  dt->_functions[10]   = S_fasl_sharp_illegal; // newline, linefeed
  dt->_functions[12]   = S_fasl_sharp_illegal; // page
  dt->_functions[13]   = S_fasl_sharp_illegal; // return

  _dispatch_tables['#'] = dt;

  _readtable_case = K_upcase;
}

// ### fasl-read-list stream character => value
Value SYS_fasl_read_list(Value streamarg, Value character)
{
  return stream_read_list(streamarg, false, current_thread(), FASL_READTABLE);
}

// ### fasl-read-comment stream character => no values
Value SYS_fasl_read_comment(Value streamarg, Value character)
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

// ### fasl-read-backquote stream character => value
Value SYS_fasl_read_backquote(Value streamarg, Value ignored)
{
//   Stream * stream = check_stream(streamarg);
//   return make_cons(S_backquote,
//                    make_cons(stream->read(true, NIL, true, current_thread(), FASL_READTABLE)));
  return make_cons(S_backquote,
                   make_cons(stream_read(streamarg, true, NIL, true, current_thread(), FASL_READTABLE)));
}

// ### fasl-read-comma stream character => value
Value SYS_fasl_read_comma(Value streamarg, Value ignored)
{
  return check_stream(streamarg)->read_comma(current_thread(), FASL_READTABLE);
}

// ### fasl-read-dispatch-char stream character => value
Value SYS_fasl_read_dispatch_char(Value streamarg, Value character)
{
  return check_stream(streamarg)->read_dispatch_char(char_value(character), current_thread(),
                                                     FASL_READTABLE);
}

// ### fasl-read-quote stream character => value
Value SYS_fasl_read_quote(Value streamarg, Value ignored)
{
//   return make_cons(S_quote,
//                    make_cons(check_stream(streamarg)->read(true, NIL, true, current_thread(),
//                                                            FASL_READTABLE)));
  return make_cons(S_quote,
                   make_cons(stream_read(streamarg, true, NIL, true, current_thread(),
                                         FASL_READTABLE)));
}

// ### fasl-read-right-paren stream character => value
Value SYS_fasl_read_right_paren(Value streamarg, Value ignored)
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

// ### fasl-read-string stream character => value
Value SYS_fasl_read_string(Value streamarg, Value character)
{
  return make_value(check_stream(streamarg)->read_string(char_value(character), FASL_READTABLE));
}

// ### fasl-sharp-a stream sub-char numarg => value
Value SYS_fasl_sharp_a(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_array(numarg, current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-b stream sub-char numarg => value
Value SYS_fasl_sharp_b(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_radix(2, current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-backslash stream sub-char numarg => value
Value SYS_fasl_sharp_backslash(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_character_literal(current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-c stream sub-char numarg => value
Value SYS_fasl_sharp_c(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_complex(current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-colon stream sub-char numarg => value
Value SYS_fasl_sharp_colon(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_symbol(FASL_READTABLE);
}

// ### fasl-sharp-dollar stream sub-char numarg => value
Value SYS_fasl_sharp_dollar(Value streamarg, Value subchar, Value numarg)
{
  Symbol * symbol = check_symbol(check_stream(streamarg)->read_symbol(FASL_READTABLE));
  Thread * thread = current_thread();
  Value package = thread->symbol_value(S_fasl_anonymous_package);
  if (package == NIL)
    {
      package = make_value(new Package());
      thread->set_symbol_value(S_fasl_anonymous_package, package);
    }
  Value sym = check_package(package)->intern(symbol->name(), false);
  the_symbol(sym)->set_package(NIL);
  return sym;
}

// ### fasl-sharp-percent stream sub-char numarg => value
Value SYS_fasl_sharp_percent(Value streamarg, Value subchar, Value numarg)
{
//   Symbol * symbol = check_symbol(check_stream(streamarg)->read_symbol(FASL_READTABLE));
//   Thread * thread = current_thread();
//   Value package = thread->symbol_value(S_fasl_anonymous_package);
//   if (package == NIL)
//     {
//       package = make_value(new Package());
//       thread->set_symbol_value(S_fasl_anonymous_package, package);
//     }
//   Value sym = check_package(package)->intern(symbol->name(), false);
//   the_symbol(sym)->set_package(NIL);
//   return sym;
  INDEX length = check_index(numarg);
  return check_stream(streamarg)->read_binary_data(length);
}

// ### fasl-sharp-dot stream sub-char numarg => value
Value SYS_fasl_sharp_dot(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_read_eval) != NIL)
    return eval(stream_read(streamarg, true, NIL, true, thread, FASL_READTABLE),
                new Environment(), thread);
  Stream * stream = check_stream(streamarg);
  String * s = new String("Can't read #. when ");
  s->append(the_symbol(S_read_eval)->prin1_to_string());
  s->append(" is false.");
  return signal_lisp_error(new ReaderError(stream, s->as_c_string()));
}

// ### fasl-sharp-illegal stream sub-char numarg => value
Value SYS_fasl_sharp_illegal(Value streamarg, Value subchar, Value numarg)
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

// ### fasl-sharp-left-paren stream sub-char numarg => value
Value SYS_fasl_sharp_left_paren(Value streamarg, Value subchar, Value numarg)
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_read_suppress) != NIL)
    {
      stream_read_list(streamarg, true, thread, FASL_READTABLE);
      return NIL;
    }
  if (numarg != NIL && thread->symbol_value(S_backquote_count) == FIXNUM_ZERO)
    return stream_read_vector(streamarg, check_index(numarg), thread, FASL_READTABLE);
  Value list = stream_read_list(streamarg, true, thread, FASL_READTABLE);
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

// ### fasl-sharp-o stream sub-char numarg => value
Value SYS_fasl_sharp_o(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_radix(8, current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-p stream sub-char numarg => value
Value SYS_fasl_sharp_p(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_pathname(current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-quote stream sub-char numarg => value
Value SYS_fasl_sharp_quote(Value streamarg, Value subchar, Value numarg)
{
  return make_cons(S_function,
                   make_cons(stream_read(streamarg, true, NIL, true, current_thread(),
                                         FASL_READTABLE)));
}

// ### fasl-sharp-r stream sub-char numarg => value
Value SYS_fasl_sharp_r(Value streamarg, Value subchar, Value numarg)
{
  Stream * stream = check_stream(streamarg);
  Thread * thread = current_thread();
  if (fixnump(numarg))
    {
      long radix = xlong(numarg);
      if (radix >= 2 && radix <= 36)
        return stream->read_radix(radix, thread, FASL_READTABLE);
    }
  // illegal radix
  while (true)
    {
      int n = stream->read_char();
      if (n < 0)
        break;
      BASE_CHAR c = (BASE_CHAR) n;
      unsigned int syntax = FASL_READTABLE->syntax(c);
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

// ### fasl-sharp-s stream sub-char numarg => value
Value SYS_fasl_sharp_s(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_structure(current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-star stream sub-char numarg => value
Value SYS_fasl_sharp_star(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_bit_vector(numarg == NIL ? -1 : fixnum_value(numarg),
                                                  current_thread(), FASL_READTABLE);
}

// ### fasl-sharp-vertical-bar stream sub-char numarg => value
Value SYS_fasl_sharp_vertical_bar(Value streamarg, Value subchar, Value numarg)
{
  check_stream(streamarg)->skip_balanced_comment();
  return current_thread()->set_values();
}

// ### fasl-sharp-x stream sub-char numarg => value
Value SYS_fasl_sharp_x(Value streamarg, Value subchar, Value numarg)
{
  return check_stream(streamarg)->read_radix(16, current_thread(), FASL_READTABLE);
}
