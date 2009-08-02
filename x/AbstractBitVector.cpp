// AbstractBitVector.cpp
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

Value AbstractBitVector::class_of() const
{
  return C_bit_vector;
}

unsigned long AbstractBitVector::hash()
{
  long hashcode = 1;
  // Consider first 64 bits only.
  unsigned long limit = length();
  if (limit > 64)
    limit = 64;
  for (unsigned long i = 0; i < limit; i++)
    hashcode = hashcode * 31 + getbit(i);
  return hashcode & MOST_POSITIVE_FIXNUM;
}

Value AbstractBitVector::subseq(unsigned long start, unsigned long end) const
{
  assert(end >= start);
  SimpleBitVector * vector = new SimpleBitVector(end - start);
  unsigned long i = start, j = 0;
  while (i < end)
    if (getbit(i++) == 0)
      vector->clearbit(j++);
    else
      vector->setbit(j++);
  return make_value(vector);
}

bool AbstractBitVector::equal(Value value) const
{
  if (!bit_vector_p(value))
    return false;
  AbstractBitVector * bv = the_bit_vector(value);
  if (this == bv)
    return true;
  if (_capacity != bv->length())
    return false;
  unsigned long capacity = _capacity;
  for (unsigned long i = 0; i < capacity; i++)
    {
      if (getbit(i) != bv->getbit(i))
        return false;
    }
  return true;
}

AbstractString * AbstractBitVector::write_to_string()
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_readably) != NIL
      || thread->symbol_value(S_print_array) != NIL)
    {
      const INDEX len = length();
      SimpleString * s = new_simple_string(len + 2);
      s->fast_set_char_at(0, '#');
      s->fast_set_char_at(1, '*');
      for (INDEX i = 0; i < len; i++)
        s->fast_set_char_at(i + 2, getbit(i) ? '1' : '0');
      return s;
    }
  else
    return unreadable_string();
}
