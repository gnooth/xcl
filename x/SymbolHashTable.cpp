// SymbolHashTable.cpp
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
#include "SymbolHashTable.hpp"

Symbol * DELETED = (Symbol *) -1;

INDEX SymbolHashTable::calculate_initial_capacity(INDEX size)
{
  INDEX capacity = 1;
  while (capacity < size)
    capacity <<= 1;
  return capacity;
}

SymbolHashTable::SymbolHashTable() : TypedObject(WIDETAG_HASH_TABLE)
{
  init(16);
}

SymbolHashTable::SymbolHashTable(INDEX size) : TypedObject(WIDETAG_HASH_TABLE)
{
  init(size);
}

void SymbolHashTable::init(INDEX size)
{
  _capacity = calculate_initial_capacity(size);
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  _buckets = (Symbol * *) GC_malloc_ignore_off_page(_capacity * sizeof(Symbol *));
  memset(_buckets, 0, _capacity * sizeof(Symbol *));
#else
  _buckets = (HashEntry * *) GC_malloc_ignore_off_page(_capacity * sizeof(HashEntry *));
  memset(_buckets, 0, _capacity * sizeof(HashEntry *));
#endif
  _threshold = (INDEX) (_capacity * SYMBOL_HASH_TABLE_LOAD_FACTOR);
  _mask = _capacity - 1;
}

Symbol * SymbolHashTable::get(AbstractString * key)
{
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  INDEX index = key->hash() & _mask;
  INDEX limit = _capacity;
  Symbol * * buckets = _buckets;
  for (INDEX i = 0; i < limit; i++)
    {
      INDEX j = (index + i) % limit;
      Symbol * sym = buckets[j];
      if (sym == NULL)
        return NULL;
      if (sym == DELETED)
        continue;
      if (key->equal(sym->name()))
        return sym;
    }
  return NULL;
#else
  HashEntry * e = _buckets[key->hash() & _mask];
  while (e)
    {
      if (key->equal(e->_symbol->name()))
        return e->_symbol; // return the symbol
      e = e->_next;
    }
  return NULL;
#endif
}

void SymbolHashTable::put(Symbol * symbol)
{
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  if (_count >= _threshold)
    rehash();
  INDEX index = symbol->hash() & _mask;
  INDEX limit = _capacity;
  Symbol * * buckets = _buckets;
  for (INDEX i = 0; i < limit; i++)
    {
      INDEX j = (index + i) % limit;
      Symbol * sym = buckets[j];
      if (sym == NULL || sym == DELETED)
        {
          buckets[j] = symbol;
          ++_count;
          return;
        }
      if (symbol->name()->equal(sym->name()))
        {
          if (sym != symbol)
            {
              // This shouldn't happen!
              printf("replacing existing key for %s\n", symbol->name()->as_c_string());
              fflush(stdout);
              buckets[j] = symbol; // replace existing key
            }
          return;
        }
    }
#else
  INDEX index = symbol->hash() & _mask;
  HashEntry * e = _buckets[index];
  while (e)
    {
      if (symbol->name()->equal(e->_symbol->name()))
        {
          if (e->_symbol != symbol)
            {
              // This shouldn't happen!
              printf("replacing existing key for %s\n", symbol->name()->as_c_string());
              fflush(stdout);
              e->_symbol = symbol; // replace existing key
            }
          return;
        }
      e = e->_next;
    }
  // Not found. We need to add a new entry.
  if (++_count > _threshold)
    {
      rehash();
      // We need a new index value for the bigger table.
      index = symbol->hash() & _mask;
    }
  e = new HashEntry(symbol);
  e->_next = _buckets[index];
  _buckets[index] = e;
#endif
}

Symbol * SymbolHashTable::remove(Symbol * symbol)
{
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  INDEX index = symbol->hash() & _mask;
  INDEX limit = _capacity;
  Symbol * * buckets = _buckets;
  for (INDEX i = 0; i < limit; i++)
    {
      INDEX j = (index + i) % limit;
      Symbol * sym = buckets[j];
      if (sym == NULL)
        return NULL;
      if (sym == DELETED)
        continue;
      if (symbol->name()->equal(sym->name()))
        {
          buckets[j] = DELETED;
          --_count;
          return sym;
        }
    }
  return NULL;
#else
  unsigned long index = symbol->hash() & _mask;
  HashEntry * e = _buckets[index];
  HashEntry * last = NULL;
  while (e)
    {
      if (symbol->name()->equal(e->_symbol->name()))
        {
          if (last)
            last->_next = e->_next;
          else
            _buckets[index] = e->_next;
          --_count;
          return e->_symbol; // The symbol is the value!
        }
      last = e;
      e = e->_next;
    }
  return NULL;
#endif
}

void SymbolHashTable::rehash()
{
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  Symbol * * old_buckets = _buckets;
  INDEX old_capacity = _capacity;
  INDEX new_capacity = _capacity * 2;
  Symbol * * new_buckets = (Symbol * *) GC_malloc_ignore_off_page(new_capacity * sizeof(Symbol *));
  memset(new_buckets, 0, new_capacity * sizeof(Symbol *));
  INDEX mask = new_capacity - 1;
  for (INDEX i = old_capacity; i-- > 0;)
    {
      Symbol * symbol = old_buckets[i];
      if (symbol != NULL && symbol != DELETED)
        {
          INDEX index = symbol->hash() & mask;
          for (INDEX j = 0; j < new_capacity; j++)
            {
              INDEX k = (index + j) % new_capacity;
              Symbol * sym = new_buckets[k];
              if (sym == NULL || sym == DELETED)
                {
                  new_buckets[k] = symbol;
                  break;
                }
            }
        }
    }
  _buckets = new_buckets;
  _capacity = new_capacity;
  _threshold = (INDEX) (new_capacity * SYMBOL_HASH_TABLE_LOAD_FACTOR);
  _mask = mask;
  GC_free(old_buckets);
#else
  HashEntry * * old_buckets = _buckets;
  const INDEX new_capacity = _capacity * 2;
  _threshold = (INDEX) (new_capacity * SYMBOL_HASH_TABLE_LOAD_FACTOR);
  _buckets = (HashEntry * *) GC_malloc_ignore_off_page(new_capacity * sizeof(HashEntry *));
  memset(_buckets, 0, new_capacity * sizeof(HashEntry *));
  _mask = new_capacity - 1;
  for (INDEX i = _capacity; i-- > 0;)
    {
      HashEntry * e = old_buckets[i];
      while (e)
        {
          const INDEX index = e->_symbol->hash() & _mask;
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
#endif
}

Value SymbolHashTable::symbols()
{
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  Value list = NIL;
  for (INDEX i = _capacity; i-- > 0;)
    {
      Symbol * sym = _buckets[i];
      if (sym != NULL && sym != DELETED)
        list = make_cons(make_value(sym), list);
    }
  return list;
#else
  Value list = NIL;
  Symbol * * buckets = _buckets;
  for (INDEX i = _capacity; i-- > 0;)
    {
      HashEntry * e = buckets[i];
      while (e)
        {
          list = make_cons(make_value(e->_symbol), list);
          e = e->_next;
        }
  }
  return list;
#endif
}
