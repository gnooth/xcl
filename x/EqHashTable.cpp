// EqHashTable.cpp
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
#include "HashTable.hpp"

EqHashTable::EqHashTable() : HashTable(S_eq)
{
  init(16);
}

EqHashTable::EqHashTable(INDEX size, Value rehash_size, Value rehash_threshold)
  : HashTable(S_eq, rehash_size, rehash_threshold)
{
  init(size);
}

Value EqHashTable::get(Value key)
{
  unsigned long index;
  if (key == _cached_key)
    {
      index = _cached_index;
    }
  else
    {
      index = ::hash(key) & _mask;
      _cached_key = key;
      _cached_index = index;
    }
  HashEntry * e = _buckets[index];
  while (e)
    {
      if (key == e->_key)
        return e->_value;
      e = e->_next;
    }
  return NULL_VALUE;
}

void EqHashTable::put(Value key, Value value)
{
  unsigned long index;
  if (key == _cached_key)
    {
      index = _cached_index;
    }
  else
    {
      index = ::hash(key) & _mask;
      _cached_key = key;
      _cached_index = index;
    }
  HashEntry * e = _buckets[index];
  while (e)
    {
    if (key == e->_key)
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
      // Need a new hash value to suit the bigger table.
      index = ::hash(key) & _mask;
      _cached_key = key;
      _cached_index = index;
    }
  e = new HashEntry(key, value);
  e->_next = _buckets[index];
  _buckets[index] = e;
}

Value EqHashTable::remove(Value key)
{
  unsigned long index;
  if (key == _cached_key)
    {
    index = _cached_index;
    }
  else
    {
      index = ::hash(key) & _mask;
      _cached_key = key;
      _cached_index = index;
    }
  HashEntry * e = _buckets[index];
  HashEntry * last = NULL;
  while (e)
    {
      if (key == e->_key)
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
