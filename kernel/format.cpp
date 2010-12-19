// format.cpp
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
#include "WrongNumberOfArgumentsError.hpp"

String * format_to_string(Value format_control, Value format_arguments)
{
  Thread * const thread = current_thread();
  AbstractString * const control = check_string(format_control);
  assert(listp(format_arguments));
  unsigned long numargs = length(format_arguments);
  Value * args = new (GC) Value[numargs];
  for (unsigned long i = 0; i < numargs; i++)
    {
      args[i] = car(format_arguments);
      format_arguments = xcdr(format_arguments);
    }

  String * result = new String();
  unsigned long limit = control->length();
  unsigned long j = 0;
  const unsigned long NEUTRAL = 0;
  const unsigned long TILDE = 1;
  unsigned long state = NEUTRAL;

  unsigned long mincol = 0;
  char padchar = ' ';

  for (unsigned long i = 0; i < limit; i++)
    {
      char c = control->fast_char_at(i);
      if (state == NEUTRAL)
        {
          if (c == '~')
            state = TILDE;
          else
            result->append_char(c);
        }
      else if (state == TILDE)
        {
          if (c >= '0' && c <= '9')
            {
              String * token = new String();
              token->append_char(c);
              ++i;
              while (i < limit && (c = control->char_at(i)) >= '0' && c <= '9')
                {
                  token->append_char(c);
                  ++i;
                }
              // "Prefix parameters are notated as signed (sign is optional)
              // decimal numbers..."
              Value number = make_number(token, 10, NULL);
              mincol = check_index(number);

              if (c == ',')
                {
                  ++i;
                  if (i >= limit)
                    signal_lisp_error("invalid format directive");
                  c = control->char_at(i);
                  if (c == '\'')
                    {
                      ++i;
                      if (i >= limit)
                        signal_lisp_error("invalid format directive");
                      padchar = control->char_at(i);
                      ++i;
                      if (i >= limit)
                        signal_lisp_error("invalid format directive");
                      c = control->char_at(i);
                    }
                }

              // Fall through...
            }
          if (c == 'A' || c == 'a')
            {
              if (j < numargs)
                {
                  Value obj = args[j++];
                  void * last_special_binding = thread->last_special_binding();
                  thread->bind_special(S_print_escape, NIL);
                  thread->bind_special(S_print_readably, NIL);
                  result->append(write_to_string(obj));
                  thread->set_last_special_binding(last_special_binding);
                }
            }
          else if (c == 'S' || c == 's')
            {
              if (j < numargs)
                {
                  Value obj = args[j++];
                  void * last_special_binding = thread->last_special_binding();
                  thread->bind_special(S_print_escape, T);
                  result->append(write_to_string(obj));
                  thread->set_last_special_binding(last_special_binding);
                }
            }
          else if (c == 'C' || c == 'c')
            {
              if (j < numargs)
                {
                  Value obj = args[j++];
                  void * last_special_binding = thread->last_special_binding();
                  result->append(princ_to_string(obj));
                  thread->set_last_special_binding(last_special_binding);
                }
            }
          else if (c == 'D' || c == 'd')
            {
              if (j < numargs)
                {
                  Value obj = args[j++];
                  void * last_special_binding = thread->last_special_binding();
                  thread->bind_special(S_print_base, make_integer(10));
                  AbstractString * s = write_to_string(obj);
                  if (s->length() < mincol)
                    {
                      unsigned long limit = mincol - s->length();
                      for (unsigned long k = 0; k < limit; k++)
                        result->append_char(padchar);
                    }
                  result->append(s);
                  thread->set_last_special_binding(last_special_binding);
                }
            }
          else if (c == 'X' || c == 'x')
            {
              if (j < numargs)
                {
                  Value obj = args[j++];
                  void * last_special_binding = thread->last_special_binding();
                  thread->bind_special(S_print_base, make_integer(16));
                  AbstractString * s = princ_to_string(obj);
                  if (s->length() < mincol)
                    {
                      unsigned long limit = mincol - s->length();
                      for (unsigned long k = 0; k < limit; k++)
                        result->append_char(padchar);
                    }
                  result->append(s);
                  thread->set_last_special_binding(last_special_binding);
                }
            }
          else if (c == 'B' || c == 'b')
            {
              if (j < numargs)
                {
                  Value obj = args[j++];
                  void * last_special_binding = thread->last_special_binding();
                  thread->bind_special(S_print_base, FIXNUM_TWO);
                  result->append(princ_to_string(obj));
                  thread->set_last_special_binding(last_special_binding);
                }
            }
          else if (c == '%')
            {
              result->append_char('\n');
            }
          state = NEUTRAL;
        }
      else
        {
          // There are no other valid states.
          assert(false);
        }
    }
  return result;
}

// ### primitive-format
Value SYS_primitive_format(unsigned int numargs, Value args[])
{
  if (numargs < 2)
    return wrong_number_of_arguments(S_format, numargs, 2, MANY);
  Value destination = args[0];
  Value format_control = args[1];
  Value format_arguments = NIL;
  for (long i = numargs; i-- > 2;)
    format_arguments = make_cons(args[i], format_arguments);
  String * string = format_to_string(format_control, format_arguments);
  if (destination == T)
    {
      AnsiStream * out = check_ansi_stream(current_thread()->symbol_value(S_standard_output));
      out->write_string(string);
      return NIL;
    }
  if (destination == NIL)
    return make_value(string);
  // REVIEW
  if (ansi_stream_p(destination))
    {
      the_ansi_stream(destination)->write_string(string);
      return NIL;
    }
  assert(false);
  return NIL;
}
