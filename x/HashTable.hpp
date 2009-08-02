// HashTable.hpp
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

#ifndef __HASH_TABLE_HPP
#define __HASH_TABLE_HPP

class HashTable : public TypedObject
{
protected:
  struct HashEntry : public gc
  {
    Value _key;
    Value _value;
    HashEntry * _next;

    HashEntry(Value key, Value value)
    {
      _key = key;
      _value = value;
    }
  };

  const Value _test; // a symbol
  INDEX _capacity;
  INDEX _count;
  INDEX _threshold;
  HashEntry * * _buckets;
  INDEX _mask;

  Value _rehash_size;
  Value _rehash_threshold;

  HashTable(Value test)
    : TypedObject(WIDETAG_HASH_TABLE), _test(test)
  {
  }

  HashTable(Value test, Value rehash_size, Value rehash_threshold)
    : TypedObject(WIDETAG_HASH_TABLE), _test(test), _rehash_size(rehash_size),
      _rehash_threshold(rehash_threshold)
  {
  }

  void init(INDEX size);
  static INDEX calculate_initial_capacity(INDEX size);

  void rehash1();

public:
  Value test() const
  {
    return _test;
  }

  unsigned long size() const
  {
    return _capacity;
  }

  unsigned long count() const
  {
    return _count;
  }

  Value rehash_size() const
  {
    return _rehash_size;
  }

  Value rehash_threshold() const
  {
    return _rehash_threshold;
  }

  virtual Value type_of() const
  {
    return S_hash_table;
  }

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual Value parts();

  virtual AbstractString * write_to_string();

  void clear()
  {
    memset(_buckets, 0, _capacity *  sizeof(HashEntry *));
    _count = 0;
  }

  Value entries();
  void maphash(Function * function);

  virtual Value get(Value key) = 0;
  virtual void put(Value key, Value value) = 0;
  virtual Value remove(Value key) = 0;
};

inline bool hash_table_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_HASH_TABLE);
}

inline HashTable * the_hash_table(Value value)
{
  assert(hash_table_p(value));
  return reinterpret_cast<HashTable *>(value - LOWTAG_TYPED_OBJECT);
}

inline HashTable * check_hash_table(Value value)
{
  if (hash_table_p(value))
    return the_hash_table(value);
  signal_type_error(value, S_hash_table);
  // Not reached.
  return NULL;
}

class EqHashTable : public HashTable
{
private:
  Value _cached_key;
  unsigned long _cached_index;

  void rehash()
  {
    _cached_key = NULL_VALUE;
    rehash1();
  }

public:
  EqHashTable();
  EqHashTable(unsigned long size, Value rehash_size, Value rehash_threshold);

  virtual Value get(Value key);
  virtual void put(Value key, Value value);
  virtual Value remove(Value key);
};

class EqlHashTable : public HashTable
{
private:
  void rehash()
  {
    rehash1();
  }

public:
  EqlHashTable();
  EqlHashTable(unsigned long size, Value rehash_size, Value rehash_threshold);

  virtual Value get(Value key);
  virtual void put(Value key, Value value);
  virtual Value remove(Value key);
};

class EqualHashTable : public HashTable
{
private:
  void rehash()
  {
    rehash1();
  }

public:
  EqualHashTable();
  EqualHashTable(unsigned long size, Value rehash_size, Value rehash_threshold);

  virtual Value get(Value key);
  virtual void put(Value key, Value value);
  virtual Value remove(Value key);
};

class EqualpHashTable : public HashTable
{
private:
  void rehash();

public:
  EqualpHashTable();
  EqualpHashTable(unsigned long size, Value rehash_size, Value rehash_threshold);

  virtual Value get(Value key);
  virtual void put(Value key, Value value);
  virtual Value remove(Value key);
};

static const float HASH_TABLE_LOAD_FACTOR = 0.75f; // REVIEW

#endif // HashTable.hpp
