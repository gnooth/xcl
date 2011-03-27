// strings.cpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

// Adapted from OpenMCL.

#include <ctype.h>      // toupper
#include "lisp.hpp"

// ### string-cmp string1 string2 start1 end1 start2 end2 => result, mismatch-index
// Case-sensitive.
Value SYS_string_cmp(Value first, Value second, Value third, Value fourth,
                     Value fifth, Value sixth)
{
  AbstractString * s1 = string(first);
  AbstractString * s2 = string(second);
  const INDEX len1 = s1->length();
  const INDEX len2 = s2->length();
  INDEX start1, end1, start2, end2;
  if (third == NIL)
    start1 = 0;
  else
    start1 = check_index(third, 0, len1);
  if (fourth == NIL)
    end1 = len1;
  else
    end1 = check_index(fourth, 0, len1);
  if (start1 > end1)
    {
      String * message = new String("Start index (");
      message->append_unsigned_long(start1);
      message->append(") is greater than end index (");
      message->append_unsigned_long(end1);
      message->append(").");
      return signal_lisp_error(message);
    }
  if (fifth == NIL)
    start2 = 0;
  else
    start2 = check_index(fifth, 0, len2);
  if (sixth == NIL)
    end2 = len2;
  else
    end2 = check_index(sixth, 0, len2);
  if (start2 > end2)
    {
      String * message = new String("Start index (");
      message->append_unsigned_long(start2);
      message->append(") is greater than end index (");
      message->append_unsigned_long(end2);
      message->append(").");
      return signal_lisp_error(message);
    }

  // Result is 0 if strings are equal, -1 if string1 < string2, +1 if string1 >
  // string2.
  long result = 0;

  // "The inequality functions return a mismatch-index that is true if the
  // strings are not equal, or false otherwise. When the mismatch-index is true,
  // it is an integer representing the first character position at which the two
  // substrings differ, as an offset from the beginning of string1."
  long mismatch_index = end1;

  INDEX i = start1;
  INDEX j = start2;
  while (true)
    {
      if (i == end1)
        {
          if (j != end2)
            result = -1;
          break;
        }
      if (j == end2)
        {
          result = 1;
          mismatch_index = i;
          break;
        }
      BASE_CHAR c1 = s1->fast_char_at(i);
      BASE_CHAR c2 = s2->fast_char_at(j);
      if (c1 != c2)
        {
          result = (c1 < c2) ? -1 : 1;
          mismatch_index = i;
          break;
        }
      ++i;
      ++j;
    }

  return current_thread()->set_values(make_fixnum(result), make_fixnum(mismatch_index));
}

// ### string-compare string1 string2 start1 end1 start2 end2 => result, mismatch-index
// Case-insensitive.
Value SYS_string_compare(Value first, Value second, Value third,Value fourth,
                         Value fifth, Value sixth)
{
  AbstractString * s1 = string(first);
  AbstractString * s2 = string(second);
  const INDEX len1 = s1->length();
  const INDEX len2 = s2->length();
  INDEX start1, end1, start2, end2;
  if (third == NIL)
    start1 = 0;
  else
    start1 = check_index(third, 0, len1);
  if (fourth == NIL)
    end1 = len1;
  else
    end1 = check_index(fourth, 0, len1);
  if (start1 > end1)
    {
      String * message = new String("Start index (");
      message->append_unsigned_long(start1);
      message->append(") is greater than end index (");
      message->append_unsigned_long(end1);
      message->append(").");
      return signal_lisp_error(message);
    }
  if (fifth == NIL)
    start2 = 0;
  else
    start2 = check_index(fifth, 0, len2);
  if (sixth == NIL)
    end2 = len2;
  else
    end2 = check_index(sixth, 0, len2);
  if (start2 > end2)
    {
      String * message = new String("Start index (");
      message->append_unsigned_long(start2);
      message->append(") is greater than end index (");
      message->append_unsigned_long(end2);
      message->append(").");
      return signal_lisp_error(message);
    }

  // Result is 0 if strings are equal, -1 if string1 < string2, +1 if string1 >
  // string2.
  long result = 0;

  // "The inequality functions return a mismatch-index that is true if the
  // strings are not equal, or false otherwise. When the mismatch-index is true,
  // it is an integer representing the first character position at which the two
  // substrings differ, as an offset from the beginning of string1."
  long mismatch_index = end1;

  INDEX i = start1;
  INDEX j = start2;
  while (true)
    {
      if (i == end1)
        {
          if (j != end2)
            result = -1;
          break;
        }
      if (j == end2)
        {
          result = 1;
          mismatch_index = i;
          break;
        }
      BASE_CHAR c1 = toupper(s1->fast_char_at(i));
      BASE_CHAR c2 = toupper(s2->fast_char_at(j));
      if (c1 != c2)
        {
          result = (c1 < c2) ? -1 : 1;
          mismatch_index = i;
          break;
        }
      ++i;
      ++j;
    }

  return current_thread()->set_values(make_fixnum(result), make_fixnum(mismatch_index));
}

// ### two-arg-string=
Value SYS_two_arg_string_equals(Value arg1, Value arg2)
{
  AbstractString * s1 = string(arg1);
  AbstractString * s2 = string(arg2);
  const INDEX len = s1->length();
  if (s2->length() != len)
    return NIL;
  for (INDEX i = len; i-- > 0;)
    {
      if (s1->fast_char_at(i) != s2->fast_char_at(i))
        return NIL;
    }
  return T;
}
