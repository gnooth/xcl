// print.cpp
//
// Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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

#include <float.h>

#include "lisp.hpp"
#include "primitives.hpp"

#include "../mpfr/mpfr.h"

static AbstractString * output_fixnum_to_string(Value arg)
{
  Thread * const thread = current_thread();
  const long base = fixnum_value(thread->symbol_value(S_print_base));
  mpz_t z;
  mpz_init_set_si(z, xlong(arg));
  char * buf = (char *) GC_malloc_atomic(mpz_sizeinbase(z, base) + 2);
  if (!buf)
    out_of_memory();
  mpz_get_str(buf, base, z);
  MPZ_CLEAR(z);
  SimpleString * s = new_simple_string(buf);
  if (base > 10)
    s->nupcase();
  if (thread->symbol_value(S_print_radix) == NIL)
    return s;
  String * s2 = new String();
  switch (base)
    {
    case 2:
      s2->append("#b");
      s2->append(s);
      break;
    case 8:
      s2->append("#o");
      s2->append(s);
      break;
    case 10:
      s2->append(s);
      s2->append_char('.');
      break;
    case 16:
      s2->append("#x");
      s2->append(s);
      break;
    default:
      s2->append_char('#');
      s2->append_long(base);
      s2->append_char('r');
      s2->append(s);
      break;
    }
  return s2;
}

AbstractString * princ_to_string(Value arg)
{
  Thread * const thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_print_escape, NIL);
  thread->bind_special(S_print_readably, NIL);
  AbstractString * s = write_to_string(arg);
  thread->set_last_special_binding(last_special_binding);
  return s;
}

AbstractString * prin1_to_string(Value arg)
{
  Thread * const thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_print_escape, T);
  AbstractString * s = write_to_string(arg);
  thread->set_last_special_binding(last_special_binding);
  return s;
}

// ### prin1-to-string object => string
// redefined in print.lisp
Value CL_prin1_to_string(Value arg)
{
  return make_value(prin1_to_string(arg));
}

// ### prin1 object &optional output-stream => object
// "PRIN1 produces output suitable for input to read. It binds *PRINT-ESCAPE*
// to TRUE."
// redefined in print.lisp
Value CL_prin1(unsigned int numargs, Value args[])
{
  Stream * stream;
  switch (numargs)
    {
    case 1:
      stream = check_stream(current_thread()->symbol_value(S_standard_output));
      break;
    case 2:
      stream = check_stream(SYS_designator_output_stream(args[1]));
      break;
    default:
      return wrong_number_of_arguments(S_print, numargs, 1, 2);
    }
  return stream->prin1(args[0]);
}

AbstractString * write_to_string(Value arg)
{
  if (characterp(arg))
    {
      String * s = new String();
      char c = xchar(arg);
      Thread * thread = current_thread();
      if (thread->symbol_value(S_print_escape) != NIL || thread->symbol_value(S_print_readably) != NIL)
        {
          s->append("#\\");
          switch (c)
            {
            case 0:
              s->append("Null");
              break;
            case 7:
              s->append("Bell");
              break;
            case '\b':
              s->append("Backspace");
              break;
            case '\t':
              s->append("Tab");
              break;
            case '\n':
              s->append("Newline");
              break;
            case '\f':
              s->append("Page");
              break;
            case '\r':
              s->append("Return");
              break;
            case 127:
              s->append("Rubout");
              break;
            default:
              s->append_char(c);
              break;
            }
        }
      else
          s->append_char(xchar(arg));
      return s;
    }
  if (symbolp(arg))
    return the_symbol(arg)->write_to_string();
  if (fixnump(arg))
    return output_fixnum_to_string(arg);
  if (consp(arg))
    return the_cons(arg)->write_to_string();

  return check_typed_object(arg)->write_to_string();
}

// ### princ-to-string object => string
// redefined in print.lisp
Value CL_princ_to_string(Value arg)
{
  return make_value(princ_to_string(arg));
}

// ### princ object &optional output-stream => object
// redefined in print.lisp
Value CL_princ(unsigned int numargs, Value args[])
{
  Stream * stream;
  switch (numargs)
    {
    case 1:
      stream = check_stream(current_thread()->symbol_value(S_standard_output));
      break;
    case 2:
      stream = check_stream(SYS_designator_output_stream(args[1]));
      break;
    default:
      return wrong_number_of_arguments(S_princ, numargs, 1, 2);
    }
  return stream->princ(args[0]);
}

// ### print object &optional output-stream => object
// redefined in print.lisp
Value CL_print(unsigned int numargs, Value args[])
{
  Stream * stream;
  switch (numargs)
    {
    case 1:
      stream = check_stream(current_thread()->symbol_value(S_standard_output));
      break;
    case 2:
      stream = check_stream(SYS_designator_output_stream(args[1]));
      break;
    default:
      return wrong_number_of_arguments(S_print, numargs, 1, 2);
    }
  stream->terpri();
  stream->prin1(args[0]);
  stream->write_char(' ');
  stream->finish_output();
  return args[0];
}

// ### %write-to-string object => string
Value SYS_write_to_string_internal(Value arg)
{
  return make_value(new_simple_string(::write_to_string(arg)));
}

// ### float-string float => string
Value SYS_float_string(Value arg)
{
  mpfr_t x;
  mp_exp_t exp; // signed long
  if (single_float_p(arg))
    {
      mpfr_init2(x, 24); // 24-bit precision
      mpfr_set_d(x, the_single_float(arg)->_f, GMP_RNDN);
    }
  else if (double_float_p(arg))
    {
      mpfr_init2(x, 53); // 53-bit precision
      mpfr_set_d(x, the_double_float(arg)->_d, GMP_RNDN);
    }
  else
    return signal_type_error(arg, S_float);
  AbstractString * temp = new String(mpfr_get_str(NULL, &exp, 10, 0, x, GMP_RNDN));
  mpfr_clear(x);
  // strip trailing zeros
  for (INDEX i = temp->length(); i-- > 0;)
    {
      BASE_CHAR c = temp->fast_char_at(i);
      if (c == '0')
        temp->set_length(i);
      else
        break;
    }
  String * s = new String();
  if (temp->char_at(0) == '-')
    {
      s->append_char('-');
      temp = temp->substring(1);
    }
  if (exp < 0)
    {
      s->append_char('.');
      INDEX limit = - exp;
      for (INDEX i = 0; i < limit; i++)
        s->append_char('0');
      s->append(temp);
    }
  else
    {
      INDEX i;
      INDEX limit = exp;
      INDEX temp_len = temp->length();
      for (i = 0; i < limit; i++)
        {
          if (i < temp_len)
            s->append_char(temp->fast_char_at(i));
          else
            s->append_char('0');
        }
      s->append_char('.');
      while (i < temp_len)
        s->append_char(temp->fast_char_at(i++));
    }
  return make_value(s);
}
