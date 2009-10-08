// EqualpHashTable.cpp
//
// Copyright (C) 2006-2008 Peter Graves <peter@armedbear.org>
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
#include "HashTable.hpp"

EqualpHashTable::EqualpHashTable() : HashTable(S_equalp)
{
  init(16);
}

EqualpHashTable::EqualpHashTable(unsigned long size, Value rehash_size, Value rehash_threshold)
  : HashTable(S_equalp, rehash_size, rehash_threshold)
{
  init(size);
}

Value EqualpHashTable::get(Value key)
{
  unsigned long index = ::equalp_hash(key) & _mask;
  HashEntry * e = _buckets[index];
  while (e)
    {
      if (key == e->_key || ::equalp(key, e->_key))
        return e->_value;
      e = e->_next;
    }
  return NULL_VALUE;
}

void EqualpHashTable::put(Value key, Value value)
{
  unsigned long index = ::equalp_hash(key) & _mask;
  HashEntry * e = _buckets[index];
  while (e)
    {
      if (key == e->_key || ::equalp(key, e->_key))
        {
          e->_value = value;
          return;
        }
      e = e->_next;
    }
  // Not found. We need to add a new entry.
  if (++_count > _threshold)
    {
      rehash();
      // We need a new hash value to suit the bigger table.
      index = ::equalp_hash(key) & _mask;
    }
  e = new HashEntry(key, value);
  e->_next = _buckets[index];
  _buckets[index] = e;
}

Value EqualpHashTable::remove(Value key)
{
  unsigned long index = ::equalp_hash(key) & _mask;
  HashEntry * e = _buckets[index];
  HashEntry * last = NULL;
  while (e)
    {
      if (key == e->_key || ::equalp(key, e->_key))
        {
          if (!last)
            _buckets[index] = e->_next;
          else
            last->_next = e->_next;
          --_count;
          return e->_value;
        }
      last = e;
      e = e->_next;
    }
  return NULL_VALUE;
}

void EqualpHashTable::rehash()
{
  HashEntry * * old_buckets = _buckets;
  const INDEX new_capacity = _capacity * 2;
  _threshold = (INDEX) (new_capacity * HASH_TABLE_LOAD_FACTOR);
  _buckets = (HashEntry * *) GC_malloc_ignore_off_page(new_capacity * sizeof(HashEntry *));
  for (INDEX i = new_capacity; i-- > 0;)
    _buckets[i] = NULL;
  _mask = new_capacity - 1;
  for (INDEX i = _capacity; i-- > 0;)
    {
      HashEntry * e = old_buckets[i];
      while (e)
        {
          const INDEX index = ::equalp_hash(e->_key) & _mask;
          HashEntry * dest = _buckets[index];
          if (dest)
            {
              while (dest->_next)
                dest = dest->_next;
              dest->_next = e;
            }
          else
            _buckets[index] = e;
          HashEntry * next = e->_next;
          e->_next = NULL;
          e = next;
        }
    }
  _capacity = new_capacity;
  GC_free(old_buckets);
}
