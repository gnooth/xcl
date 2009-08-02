// AbstractString.cpp
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

Value AbstractString::class_of() const
{
  return C_string;
}

Value AbstractString::element_type() const
{
  return S_character;
}

bool AbstractString::equalp(Value value) const
{
  if (stringp(value))
    return equalp(the_string(value));
  if (vectorp(value))
    {
      AbstractVector * v = the_vector(value);
      INDEX len = length();
      if (len != v->length())
        return false;
      for (INDEX i = 0; i < len; i++)
        {
          Value element = v->aref(i);
          if (characterp(element))
            {
              BASE_CHAR c1 = xchar(element);
              BASE_CHAR c2 = fast_char_at(i);
              if (c1 == c2)
                continue;
              if (toupper(c1) == toupper(c2))
                continue;
              if (tolower(c1) == tolower(c2))
                continue;
            }
          return false;
        }
      return true;
    }
  return false;
}
