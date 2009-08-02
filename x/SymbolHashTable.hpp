// SymbolHashTable.hpp
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

#ifndef __SYMBOL_HASH_TABLE_HPP
#define __SYMBOL_HASH_TABLE_HPP

#define SYMBOL_HASH_TABLE_OPEN_ADDRESSING

class SymbolHashTable : public TypedObject
{
private:
#ifndef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  struct HashEntry : public gc
  {
    Symbol * _symbol;
    HashEntry * _next;

    HashEntry(Symbol * symbol) : _symbol(symbol)
    {
    }
  };
#endif
  INDEX _threshold;
#ifdef SYMBOL_HASH_TABLE_OPEN_ADDRESSING
  Symbol * * _buckets;
#else
  HashEntry * * _buckets;
#endif
  INDEX _capacity;
  INDEX _count;
  INDEX _mask;

  static INDEX calculate_initial_capacity(INDEX size);

  void init(INDEX size);
  void rehash();

public:
  SymbolHashTable();

  SymbolHashTable(INDEX size);

  Symbol * get(AbstractString * key);

  Symbol * get(const char * key)
  {
    return get(new_simple_string(key));
  }

  void put(Symbol * symbol);

  Symbol * remove(Symbol * symbol);

  Value symbols();
};

static const float SYMBOL_HASH_TABLE_LOAD_FACTOR = 0.75f; // REVIEW

#endif
