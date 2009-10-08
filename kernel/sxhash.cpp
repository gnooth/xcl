// HashTable.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"

// adapted from SBCL
long mix(long x, long y)
{
  long xy = x * 3 + y;
  return MOST_POSITIVE_FIXNUM & (441516657L ^ xy ^ (xy >> 5));
}

static unsigned long compute_hash(Value obj, long depth)
{
  if (consp(obj))
    {
      if (depth > 0)
        {
          unsigned long n1 = compute_hash(xcar(obj), depth - 1);
          unsigned long n2 = compute_hash(xcdr(obj), depth - 1);
          return n1 ^ n2;
        }
      else
        // REVIEW
        return 261835505;
    }
  else
    return hash(obj);
}

unsigned long hash(Value value)
{
  switch (lowtag_of(value))
    {
    case LOWTAG_EVEN_FIXNUM:
    case LOWTAG_ODD_FIXNUM:
      return xlong(value) & MOST_POSITIVE_FIXNUM;
    case LOWTAG_TYPED_OBJECT:
      return the_typed_object(value)->hash();
    case LOWTAG_SYMBOL:
      return the_symbol(value)->hash();
    case LOWTAG_LIST:
      if (value == NIL)
        return 0x12345678; // FIXME
      return compute_hash(value, 4);
    case LOWTAG_CHARACTER:
      return (unsigned long) xchar(value);
    default:
      printf("hash: unsupported case: %ld\n", lowtag_of(value));
      assert(false);
      return 0; // FIXME
    }
}

static unsigned long compute_equalp_hash(Value obj, long depth)
{
  if (consp(obj))
    {
      if (depth > 0)
        {
          unsigned long n1 = compute_equalp_hash(xcar(obj), depth - 1);
          unsigned long n2 = compute_equalp_hash(xcdr(obj), depth - 1);
          return n1 ^ n2;
        }
      else
        // REVIEW
        return 261835505;
    }
  else
    return equalp_hash(obj);
}

unsigned long equalp_hash(Value value)
{
  switch (lowtag_of(value))
    {
    case LOWTAG_EVEN_FIXNUM:
    case LOWTAG_ODD_FIXNUM:
      return xlong(value) & MOST_POSITIVE_FIXNUM;
    case LOWTAG_TYPED_OBJECT:
      return the_typed_object(value)->equalp_hash();
    case LOWTAG_SYMBOL:
      return the_symbol(value)->hash();
    case LOWTAG_LIST:
      if (value == NIL)
        return 0x12345678; // FIXME
      return compute_equalp_hash(value, 4);
    case LOWTAG_CHARACTER:
      return (unsigned long) xchar(value);
    default:
      return hash(value);
    }
}

// ### sxhash object => hash-code
Value CL_sxhash(Value arg)
{
  return make_fixnum(hash(arg));
}

// ### psxhash object => hash-code
Value SYS_psxhash(Value arg)
{
  return make_fixnum(equalp_hash(arg));
}
