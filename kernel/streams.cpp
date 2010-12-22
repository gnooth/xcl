// streams.cpp
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

// #include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
// #include "runtime.h"
#include "primitives.hpp"
#include "reader.hpp"
// #include "Complex.hpp"
#include "EndOfFile.hpp"
#include "Package.hpp"
// #include "Pathname.hpp"
#include "ProgramError.hpp"
// #include "ReaderError.hpp"
#include "Readtable.hpp"
// #include "SimpleArray_T.hpp"
// #include "SimpleArray_UB8_1.hpp"
// #include "ZeroRankArray.hpp"
// #include "keywordp.hpp"

// ### %stream-princ stream object => object
Value SYS_stream_princ_internal(Value stream, Value object)
{
  return check_ansi_stream(stream)->princ(object);
}

// ### %stream-write-object stream object => object
Value SYS_stream_write_object_internal(Value stream, Value object)
{
  check_ansi_stream(stream)->write_string(::write_to_string(object));
  return object;
}

// ### designator-input-stream
Value SYS_designator_input_stream(Value arg)
{
  if (ansi_stream_p(arg))
    return arg;
  Thread * thread = current_thread();
  if (arg == T)
    return thread->symbol_value(S_terminal_io);
  if (arg == NIL)
    return thread->symbol_value(S_standard_input);
  Function * function = reinterpret_cast<Function *>(the_symbol(S_streamp)->function());
  if (function->execute(arg))
    return arg;
  return signal_type_error(arg,
                           list3(S_or, S_stream,
                                 list3(S_member, T, NIL)));
}

// ### designator-output-stream
Value SYS_designator_output_stream(Value arg)
{
  if (ansi_stream_p(arg))
    return arg;
  Thread * thread = current_thread();
  if (arg == T)
    return thread->symbol_value(S_terminal_io);
  if (arg == NIL)
    return thread->symbol_value(S_standard_output);
  Function * function = reinterpret_cast<Function *>(the_symbol(S_streamp)->function());
  if (function->execute(arg))
    return arg;
  return signal_type_error(arg,
                           list3(S_or, S_stream,
                                 list3(S_member, T, NIL)));
}

// ### ansi-stream-p object => generalized-boolean
Value SYS_ansi_stream_p(Value arg)
{
  return ansi_stream_p(arg) ? T : NIL;
}

// ### streamp object => generalized-boolean
// redefined as a generic function in gray-streams.lisp
Value CL_streamp(Value arg)
{
  return SYS_ansi_stream_p(arg);
}

// ### ansi-stream-open-stream-p stream => generalized-boolean
Value SYS_ansi_stream_open_stream_p(Value arg)
{
  return check_ansi_stream(arg)->is_open() ? T : NIL;
}

// ### open-stream-p stream => generalized-boolean
// redefined as a generic function in gray-streams.lisp
Value CL_open_stream_p(Value arg)
{
  return SYS_ansi_stream_open_stream_p(arg);
}

// ### ansi-stream-element-type stream => typespec
Value SYS_ansi_stream_element_type(Value arg)
{
  return check_ansi_stream(arg)->element_type();
}

// ### stream-element-type stream => typespec
// redefined as a generic function in gray-streams.lisp
Value CL_stream_element_type(Value arg)
{
  return SYS_ansi_stream_element_type(arg);
}

// ### ansi-steam-input-stream-p stream => generalized-boolean
Value SYS_ansi_stream_input_stream_p(Value arg)
{
  return check_ansi_stream(arg)->is_input_stream() ? T : NIL;
}

// ### input-stream-p stream => generalized-boolean
// redefined as a generic function in gray-streams.lisp
Value CL_input_stream_p(Value arg)
{
  return SYS_ansi_stream_input_stream_p(arg);
}

// ### ansi-stream-output-stream-p stream => generalized-boolean
Value SYS_ansi_stream_output_stream_p(Value arg)
{
  return check_ansi_stream(arg)->is_output_stream() ? T : NIL;
}

// ### output-stream-p stream => generalized-boolean
// redefined as a generic function in gray-streams.lisp
Value CL_output_stream_p(Value arg)
{
  return SYS_ansi_stream_output_stream_p(arg);
}

// ### interactive-stream-p stream => generalized-boolean
Value CL_interactive_stream_p(Value arg)
{
  return check_ansi_stream(arg)->is_interactive() ? T : NIL;
}

// ### %stream-charpos stream => position
// returns the number of characters on the current line of output
Value SYS_stream_charpos_internal(Value stream)
{
  // REVIEW verify that the stream is a character output stream
  return make_integer(check_ansi_stream(stream)->charpos());
}

// ### %stream-set-charpos stream position => position
Value SYS_stream_set_charpos_internal(Value stream, Value position)
{
  // REVIEW verify that the stream is a character output stream
  check_ansi_stream(stream)->set_charpos(check_index(position));
  return position;
}

// ### %stream-terpri
Value SYS_stream_terpri_internal(Value arg)
{
  return check_ansi_stream(arg)->terpri();
}

// ### terpri &optional output-stream => nil
Value CL_terpri(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_terpri, numargs, 0, 1);
  AnsiStream * stream;
  if (numargs == 1)
    stream = check_ansi_stream(SYS_designator_output_stream(args[0]));
  else
    stream = check_ansi_stream(current_thread()->symbol_value(S_standard_output));
  return stream->terpri();
}

// ### %stream-fresh-line
Value SYS_stream_fresh_line_internal(Value arg)
{
  return check_ansi_stream(arg)->fresh_line();
}

// ### fresh-line &optional output-stream => generalized-boolean
Value CL_fresh_line(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_fresh_line, numargs, 0, 1);
  AnsiStream * stream;
  if (numargs == 1)
    stream = check_ansi_stream(SYS_designator_output_stream(args[0]));
  else
    stream = check_ansi_stream(current_thread()->symbol_value(S_standard_output));
  return stream->fresh_line();
}

// ### %stream-read input-stream eof-error-p eof-value recursive-p => object
Value SYS_stream_read_internal(Value arg1, Value arg2, Value arg3, Value arg4)
{
  Thread * thread = current_thread();
  //   Stream * stream = check_ansi_stream(arg1);
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
  AnsiStream * stream =
    check_ansi_stream(numargs == 0 ? thread->symbol_value(S_standard_input) : SYS_designator_input_stream(args[0]));
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
  AnsiStream * stream =
    check_ansi_stream(numargs == 0 ? thread->symbol_value(S_standard_input) : SYS_designator_input_stream(args[0]));
  bool eof_error_p = !(numargs > 1 && args[1] == NIL);
  Value eof_value = (numargs > 2) ? args[2] : NIL;
  bool recursive_p = (numargs > 3 && args[3] != NIL);
  Readtable * rt = current_readtable(thread);
  // REVIEW
  return stream_read_preserving_whitespace(make_value(stream), eof_error_p, eof_value,
                                           recursive_p, thread, rt);
}

// ### ansi-stream-read-line
Value SYS_ansi_stream_read_line(Value streamarg, Value eof_error_p, Value eof_value)
{
  return check_ansi_stream(streamarg)->read_line(eof_error_p != NIL, eof_value);
}

// ### read-char &optional input-stream eof-error-p eof-value recursive-p => char
Value CL_read_char(unsigned int numargs, Value args[])
{
  if (numargs <= 4)
    {
      AnsiStream * stream;
      if (numargs > 0)
        stream = check_ansi_stream(SYS_designator_input_stream(args[0]));
      else
        stream = check_ansi_stream(current_thread()->symbol_value(S_standard_input));
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
      AnsiStream * stream;
      if (numargs > 0)
        stream = check_ansi_stream(SYS_designator_input_stream(args[0]));
      else
        stream = check_ansi_stream(current_thread()->symbol_value(S_standard_input));
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
  AnsiStream * stream;
  if (numargs == 2)
    stream = check_ansi_stream(SYS_designator_input_stream(args[1]));
  else
    stream = check_ansi_stream(current_thread()->symbol_value(S_standard_input));
  stream->unread_char(c);
  return NIL;
}

// ### listen &optional input-stream => generalized-boolean
Value CL_listen(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_listen, numargs, 0, 1);
  AnsiStream * stream;
  if (numargs == 1)
    stream = check_ansi_stream(SYS_designator_input_stream(args[0]));
  else
    stream = check_ansi_stream(current_thread()->symbol_value(S_standard_input));
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
  AnsiStream * stream;
  if (numargs > 1)
    stream = check_ansi_stream(SYS_designator_input_stream(args[1]));
  else
    stream = check_ansi_stream(thread->symbol_value(S_standard_input));
  return stream_read_delimited_list(make_value(stream), c, thread);
}

// ### %stream-write-string stream string start end => string
Value SYS_stream_write_string_internal(Value arg1, Value arg2, Value arg3, Value arg4)
{
  AnsiStream * stream = check_ansi_stream(arg1);
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
  check_ansi_stream(arg1)->write_char(char_value(arg2));
  return arg2;
}

// ### write-char character &optional output-stream => character
Value CL_write_char(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_write_char, numargs, 1, 2);
  char c = char_value(args[0]);
  AnsiStream * stream;
  if (numargs == 2)
    stream = check_ansi_stream(SYS_designator_output_stream(args[1]));
  else
    stream = check_ansi_stream(current_thread()->symbol_value(S_standard_output));
  stream->write_char(c);
  return args[0];
}

// ### %write-8-bits byte stream => byte
Value SYS_xwrite_8_bits(Value byte, Value stream)
{
  BYTE n = (BYTE) xlong(byte);
  the_ansi_stream(stream)->write_byte(n);
  return byte;
}

// ### write-8-bits byte stream => byte
Value SYS_write_8_bits(Value byte, Value stream)
{
  BYTE n = (BYTE) check_index(byte, 0, 255);
  check_ansi_stream(stream)->write_byte(n);
  return byte;
}

// ### read-8-bits stream eof-error-p eof-value => byte
Value SYS_read_8_bits(Value arg1, Value arg2, Value arg3)
{
  AnsiStream * stream = check_ansi_stream(arg1);
  long n = stream->read_byte();
  if (n >= 0)
    return make_fixnum(n);
  // eof
  if (arg2 != NIL)
    return signal_lisp_error(new EndOfFile(stream));
  return arg3;
}

// ### stream-read-byte-function
Value SYS_stream_read_byte_function(Value arg)
{
  return check_ansi_stream(arg)->read_byte_function();
}

// ### clear-input &optional input-stream => nil
Value CL_clear_input(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_clear_input, numargs, 0, 1);
  AnsiStream * stream;
  if (numargs == 1)
    stream = check_ansi_stream(SYS_designator_input_stream(args[0]));
  else
    stream = check_ansi_stream(current_thread()->symbol_value(S_standard_input));
  stream->clear_input();
  return NIL;
}

// ### clear-output &optional output-stream => nil
Value CL_clear_output(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_clear_output, numargs, 0, 1);
  if (numargs == 1)
    check_ansi_stream(SYS_designator_output_stream(args[0]));
  else
    check_ansi_stream(current_thread()->symbol_value(S_standard_output));
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
      return check_ansi_stream(args[0])->file_position();
    case 2:
      return check_ansi_stream(args[0])->set_file_position(args[1]);
    default:
      return wrong_number_of_arguments(S_file_position, numargs, 1, 2);
    }
}

// ### file-length stream => length
Value CL_file_length(Value arg)
{
  return check_ansi_stream(arg)->file_length();
}

// ### file-string-length stream object => length
Value CL_file_string_length(Value arg1, Value arg2)
{
  return check_ansi_stream(arg1)->file_string_length(arg2);
}

// ### finish-output &optional output-stream => nil
Value CL_finish_output(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_finish_output, numargs, 0, 1);
  if (numargs == 1)
    check_ansi_stream(SYS_designator_output_stream(args[0]))->finish_output();
  else
    check_ansi_stream(current_thread()->symbol_value(S_standard_output))->finish_output();
  return NIL;
}

// ### force-output &optional output-stream => nil
// REVIEW identical to CL_finish_output()
Value CL_force_output(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_force_output, numargs, 0, 1);
  if (numargs == 1)
    check_ansi_stream(SYS_designator_output_stream(args[0]))->finish_output();
  else
    check_ansi_stream(current_thread()->symbol_value(S_standard_output))->finish_output();
  return NIL;
}

// ### ansi-stream-close stream => result
Value SYS_ansi_stream_close(Value arg)
{
  return check_ansi_stream(arg)->close();
}

// ### close stream &key abort => result
// redefined as a generic function in gray-streams.lisp
Value CL_close(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      return check_ansi_stream(args[0])->close();
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
        return check_ansi_stream(args[0])->close();
      }
    default:
      return wrong_number_of_arguments(S_close, numargs, 1, 3);
    }
}
