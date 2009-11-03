// characters.cpp
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
#include "primitives.hpp"

// ### character
Value CL_character(Value arg)
{
  if (characterp(arg))
    return arg;
  else if (stringp(arg))
    {
      AbstractString * s = the_string(arg);
      if (s->length() == 1)
        return make_character(s->fast_char_at(0));
    }
  else if (symbolp(arg))
    {
      SimpleString * s = the_symbol(arg)->name();
      if (s && s->length() == 1)
        return make_character(s->fast_char_at(0));
    }
  return signal_type_error(arg, S_character_designator);
}

// ### characterp
Value CL_characterp(Value arg)
{
  return characterp(arg) ? T : NIL;
}

// ### standard-char-p character => generalized-boolean
// must signal an error of type TYPE-ERROR if arg is not a character
Value CL_standard_char_p(Value arg)
{
  if (standard_char_p(arg))
    return T;
  if (characterp(arg))
    return NIL;
  return signal_type_error(arg, S_character);
}

// ### %standard-char-p character => boolean
// no error
Value SYS_xstandard_char_p(Value arg)
{
  return standard_char_p(arg) ? T : NIL;
}

// ### graphic-char-p character => generalized-boolean
Value CL_graphic_char_p(Value arg)
{
  BASE_CHAR c = char_value(arg);
  if (c >= 32 && c < 127)
    return T;
  if (c >= 160)
    return T;
  return NIL;
}

// ### two-arg-char=
Value SYS_two_arg_char_e(Value arg1, Value arg2)
{
  if (!characterp(arg1))
    return signal_type_error(arg1, S_character);
  if (arg1 == arg2)
    return T;
  if (!characterp(arg2))
    return signal_type_error(arg2, S_character);
  return NIL;
}

// ### char=
Value CL_char_e(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_e, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      if (!characterp(args[0]))
        return signal_type_error(args[0], S_character);
      if (args[0] == args[1])
        return T;
      if (!characterp(args[1]))
        return signal_type_error(args[1], S_character);
      return NIL;
    default:
      {
        BASE_CHAR c0 = char_value(args[0]);
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (c0 != char_value(args[i]))
              return NIL;
          }
        return T;
      }
    }
}

// ### two-arg-char-equal
Value SYS_two_arg_char_equal(Value arg1, Value arg2)
{
  if (!characterp(arg1))
    return signal_type_error(arg1, S_character);
  if (arg1 == arg2)
    return T;
  if (!characterp(arg2))
    return signal_type_error(arg2, S_character);
  BASE_CHAR c1 = xchar(arg1);
  BASE_CHAR c2 = xchar(arg2);
  if (toupper(c1) == toupper(c2))
    return T;
  return NIL;
}

// ### char-equal
Value CL_char_equal(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_equal, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      {
        if (!characterp(args[0]))
          return signal_type_error(args[0], S_character);
        if (args[0] == args[1])
          return T;
        if (!characterp(args[1]))
          return signal_type_error(args[1], S_character);
        BASE_CHAR c1 = xchar(args[0]);
        BASE_CHAR c2 = xchar(args[1]);
        if (toupper(c1) == toupper(c2))
          return T;
        return NIL;
      }
    default:
      {
        BASE_CHAR c0 = char_value(args[0]);
        for (INDEX i = 1; i < numargs; i++)
          {
            BASE_CHAR c1 = char_value(args[i]);
            if (c0 != c1 && toupper(c0) != toupper(c1))
              return NIL;
          }
        return T;
      }
    }
}

// ### two-arg-char/=
Value SYS_two_arg_char_ne(Value arg1, Value arg2)
{
  if (!characterp(arg1))
    return signal_type_error(arg1, S_character);
  if (arg1 == arg2)
    return NIL;
  if (!characterp(arg2))
    return signal_type_error(arg2, S_character);
  return T;
}

// ### char/=
Value CL_char_ne(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_ne, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      if (!characterp(args[0]))
        return signal_type_error(args[0], S_character);
      if (!characterp(args[1]))
        return signal_type_error(args[1], S_character);
      return args[0] != args[1] ? T : NIL;
    default:
      {
        for (unsigned int i = 0; i < numargs - 1; i++)
          {
            unsigned char c = char_value(args[i]);
            for (unsigned int j = i + 1; j < numargs; j++)
              {
                if (char_value(args[j]) == c)
                  return NIL;
              }
          }
        return T;
      }
    }
}

// ### char-not-equal
Value CL_char_not_equal(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_ne, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      {
        unsigned char c0 = char_value(args[0]);
        unsigned char c1 = char_value(args[1]);
        return (c0 == c1 || toupper(c0) == toupper(c1)) ? NIL : T;
      }
    default:
      {
        for (unsigned int i = 0; i < numargs; i++)
          args[i] = toupper(char_value(args[i]));
        for (unsigned int i = 0; i < numargs - 1; i++)
          {
            unsigned char c = args[i];
            for (unsigned int j = i + 1; j < numargs; j++)
              {
                if (args[j] == c)
                  return NIL;
              }
          }
        return T;
      }
    }
}

// ### two-arg-char<
Value SYS_two_arg_char_lt(Value arg1, Value arg2)
{
  return char_value(arg1) < char_value(arg2) ? T : NIL;
}

// ### char<
Value CL_char_lt(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_lt, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return char_value(args[0]) < char_value(args[1]) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (char_value(args[i - 1]) >= char_value(args[i]))
              return NIL;
          }
        return T;
      }
    }
}

// ### two-arg-char<=
Value SYS_two_arg_char_le(Value arg1, Value arg2)
{
  return char_value(arg1) <= char_value(arg2) ? T : NIL;
}

// ### char<=
Value CL_char_le(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_le, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return char_value(args[0]) <= char_value(args[1]) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (char_value(args[i - 1]) > char_value(args[i]))
              return NIL;
          }
        return T;
      }
    }
}

// ### two-arg-char>
Value SYS_two_arg_char_gt(Value arg1, Value arg2)
{
  return char_value(arg1) > char_value(arg2) ? T : NIL;
}

// ### char>
Value CL_char_gt(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_gt, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return char_value(args[0]) > char_value(args[1]) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (char_value(args[i - 1]) <= char_value(args[i]))
              return NIL;
          }
        return T;
      }
    }
}

// ### two-arg-char>=
Value SYS_two_arg_char_ge(Value arg1, Value arg2)
{
  return char_value(arg1) >= char_value(arg2) ? T : NIL;
}

// ### char>=
Value CL_char_ge(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_ge, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return char_value(args[0]) >= char_value(args[1]) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (char_value(args[i - 1]) < char_value(args[i]))
              return NIL;
          }
        return T;
      }
    }
}

// ### char-greaterp
Value CL_char_greaterp(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_greaterp, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return toupper(char_value(args[0])) > toupper(char_value(args[1])) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (toupper(char_value(args[i - 1])) <= toupper(char_value(args[i])))
              return NIL;
          }
        return T;
      }
    }
}

// ### char-not-greaterp
Value CL_char_not_greaterp(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_not_greaterp, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return toupper(char_value(args[0])) <= toupper(char_value(args[1])) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (toupper(char_value(args[i - 1])) > toupper(char_value(args[i])))
              return NIL;
          }
        return T;
      }
    }
}

// ### char-lessp
Value CL_char_lessp(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_lessp, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return toupper(char_value(args[0])) < toupper(char_value(args[1])) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (toupper(char_value(args[i - 1])) >= toupper(char_value(args[i])))
              return NIL;
          }
        return T;
      }
    }
}

// ### char-not-lessp
Value CL_char_not_lessp(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 0:
      return wrong_number_of_arguments(S_char_not_lessp, numargs, 1, MANY);
    case 1:
      if (characterp(args[0]))
        return T;
      else
        return signal_type_error(args[0], S_character);
    case 2:
      return toupper(char_value(args[0])) >= toupper(char_value(args[1])) ? T : NIL;
    default:
      {
        for (unsigned int i = 1; i < numargs; i++)
          {
            if (toupper(char_value(args[i - 1])) < toupper(char_value(args[i])))
              return NIL;
          }
        return T;
      }
    }
}

Value name_to_char(AbstractString * s)
{
  AbstractString * lower = s->downcase();
  if (lower->equal("null"))
    return make_character(0);
  if (lower->equal("bell"))
    return make_character(7);
  if (lower->equal("backspace"))
    return make_character('\b');
  if (lower->equal("tab"))
    return make_character('\t');
  if (lower->equal("linefeed"))
    return make_character('\n');
  if (lower->equal("newline"))
    return make_character('\n');
  if (lower->equal("page"))
    return make_character('\f');
  if (lower->equal("return"))
    return make_character('\r');
  if (lower->equal("space"))
    return make_character(' ');
  if (lower->equal("rubout"))
    return make_character(127);
  // Unknown.
  return NIL;
}

// ### name-char name => char-p
Value CL_name_char(Value arg)
{
  return name_to_char(string(arg));
}

// ### char-name character => name
Value CL_char_name(Value arg)
{
  const char * s = NULL;
  BASE_CHAR c = char_value(arg);
  switch (c)
    {
    case 0:
      s = "Null";
      break;
    case 7:
      s = "Bell";
      break;
    case '\b':
      s = "Backspace";
      break;
    case '\t':
      s = "Tab";
      break;
    case '\n':
      s = "Newline";
      break;
    case '\f':
      s = "Page";
      break;
    case '\r':
      s = "Return";
      break;
    case ' ':
      s = "Space";
      break;
    case 127:
      s = "Rubout";
      break;
    }
  return s ? make_simple_string(s) : NIL;
}

// ### char-code character => code
Value CL_char_code(Value arg)
{
  return make_fixnum(char_value(arg));
}

// ### char-int character => integer
Value CL_char_int(Value arg)
{
  return make_fixnum(char_value(arg));
}

// ### code-char code => char-p
Value CL_code_char(Value code)
{
  // "Returns a character with the code attribute given by CODE. If no such
  // character exists and one cannot be created, NIL is returned."
  long n = fixnum_value(code);
  if (n >= 0 && n < 256)
    return make_character(n);
  else
    return NIL;
}

// ### alpha-char-p character => generalized-boolean
Value CL_alpha_char_p(Value character)
{
  char c = char_value(character);
  if ('A' <= c && c <= 'Z')
    return T;
  if ('a' <= c && c <= 'z')
    return T;
  return NIL;
}

// ### alphanumericp character => generalized-boolean
Value CL_alphanumericp(Value character)
{
  char c = char_value(character);
  if ('A' <= c && c <= 'Z')
    return T;
  if ('a' <= c && c <= 'z')
    return T;
  if ('0' <= c && c <= '9')
    return T;
  return NIL;
}

// ### both-case-p character => generalized-boolean
Value CL_both_case_p(Value arg)
{
  char c = char_value(arg);
  if ('A' <= c && c <= 'Z')
    return T;
  if ('a' <= c && c <= 'z')
    return T;
  return NIL;
}

// ### lower-case-p character => generalized-boolean
Value CL_lower_case_p(Value arg)
{
  char c = char_value(arg);
  if ('a' <= c && c <= 'z')
    return T;
  return NIL;
}

// ### upper-case-p character => generalized-boolean
Value CL_upper_case_p(Value arg)
{
  char c = char_value(arg);
  if ('A' <= c && c <= 'Z')
    return T;
  return NIL;
}

// ### char-downcase
Value CL_char_downcase(Value arg)
{
  return make_character(tolower(char_value(arg)));
}

// ### char-upcase
Value CL_char_upcase(Value arg)
{
  return make_character(toupper(char_value(arg)));
}

// ### digit-char-p char &optional radix => weight
Value CL_digit_char_p(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_digit_char_p, numargs, 1, 2);
  BASE_CHAR c = char_value(args[0]);
  int radix;
  if (numargs == 2)
    radix = check_index(args[1], 2, 36);
  else
    radix = 10;
  if (c >= '0')
    {
      int n = c - '0';
      if (radix <= 10)
        return (n < radix) ? make_fixnum(n) : NIL;
      if (n < 10)
        return make_fixnum(n);
      if (c >= 'A')
        {
          // A-Z
          n -= 7;
          if (n >= 10 && n < radix)
            return make_fixnum(n);
          if (c >= 'a')
            {
              // a-z
              n -= 32;
              if (n >= 10 && n < radix)
                return make_fixnum(n);
            }
        }
    }
  return NIL;
}

// ### digit-char weight &optional radix => char
Value CL_digit_char(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_digit_char_p, numargs, 1, 2);
  if (indexp(args[0]))
    {
      unsigned long weight = xlong(args[0]);
      unsigned long radix;
      if (numargs == 2)
        radix = check_index(args[1], 2, 36);
      else
        radix = 10;
      if (weight >= radix)
        return NIL;
      else if (weight < 10)
        return make_character('0' + weight);
      else
        return make_character('A' + weight - 10);
    }
  if (bignump(args[0]) && !the_bignum(args[0])->minusp())
    return NIL;
  return signal_type_error(args[0], S_unsigned_byte);
}

// FIXME check the character's syntax type in *READTABLE*
// ### whitespacep character => T or NIL
Value SYS_whitespacep(Value arg)
{
  switch (char_value(arg))
    {
    case 9:     // tab
    case 10:    // linefeed
    case 12:    // form feed
    case 13:    // return
    case ' ':   // space
      return T;
    default:
      return NIL;
    }
}
