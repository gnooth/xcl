// HashTable.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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
#include "runtime.h"
#include "primitives.hpp"
#include "HashTable.hpp"

void HashTable::init(INDEX size)
{
  _capacity = calculate_initial_capacity(size);
  _buckets = (HashEntry * *) GC_malloc_ignore_off_page(_capacity * sizeof(HashEntry *));
  memset(_buckets, 0, _capacity *  sizeof(HashEntry *));
  _threshold = (size_t) (_capacity * HASH_TABLE_LOAD_FACTOR);
  _mask = _capacity - 1;
}

INDEX HashTable::calculate_initial_capacity(INDEX size)
{
  INDEX capacity = 1;
  while (capacity < size)
    capacity <<= 1; // FIXME check for overflow!
  return capacity;
}

Value HashTable::class_of() const
{
  return C_hash_table;
}

bool HashTable::typep(Value type) const
{
  return (type == S_hash_table || type == S_atom || type == T
          || type == C_hash_table || type == C_t);
}

Value HashTable::parts()
{
  String * description = new String(prin1_to_string());
  description->append_char('\n');
  Value value_string = make_simple_string("VALUE");
  Value elements = NIL;
  for (INDEX i = 0; i < _capacity; i++)
    {
      HashEntry * e = _buckets[i];
      while (e)
        {
          String * string = new String("KEY [bucket ");
          string->append_long(i);
          string->append_char(']');
          elements = make_cons(make_cons(make_value(string),
                                         e->_key),
                               elements);
          elements = make_cons(make_cons(value_string,
                                         e->_value),
                               elements);
          e = e->_next;
        }
    }
  return current_thread()->set_values(make_value(description), T, CL_nreverse(elements));
}

// returns a list of (key . value) pairs
Value HashTable::entries()
{
  Value list = NIL;
  for (INDEX i = _capacity; i-- > 0;)
    {
      HashEntry * e = _buckets[i];
      while (e)
        {
          list = make_cons(make_cons(e->_key, e->_value), list);
          e = e->_next;
        }
    }
  return list;
}

void HashTable::maphash(Function * function)
{
  for (INDEX i = _capacity; i-- > 0;)
    {
      HashEntry * e = _buckets[i];
      while (e)
        {
          function->execute(e->_key, e->_value);
          e = e->_next;
        }
    }
}

void HashTable::rehash1()
{
  HashEntry * * old_buckets = _buckets;
  const INDEX new_capacity = _capacity * 2;
  _threshold = (INDEX) (new_capacity * HASH_TABLE_LOAD_FACTOR);
  _buckets = (HashEntry * *) GC_malloc_ignore_off_page(new_capacity * sizeof(HashEntry *));
  memset(_buckets, 0, new_capacity *  sizeof(HashEntry *));
  _mask = new_capacity - 1;
  for (INDEX i = _capacity; i-- > 0;)
    {
      HashEntry * e = old_buckets[i];
      while (e)
        {
          const INDEX index = ::hash(e->_key) & _mask;
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

AbstractString * HashTable::write_to_string()
{
  char buf[256];
  SNPRINTF(buf, sizeof(buf), "#<%s hash table, %lu entries, %lu buckets {%lX}>",
           ::prin1_to_string(_test)->as_c_string(),
           _count, _capacity, (unsigned long) this);
  return new_simple_string(buf);
}

// ### %make-hash-table
Value SYS_make_hash_table_internal(Value test, Value size, Value rehash_size,
                                   Value rehash_threshold)
{
  INDEX n = check_index(size);
  if (functionp(test))
    {
      Function * function = the_function(test);
      if (function == the_symbol(S_eq)->function())
        test = S_eq;
      else if (function == the_symbol(S_eql)->function())
        test = S_eql;
      else if (function == the_symbol(S_equal)->function())
        test = S_equal;
      else if (function == the_symbol(S_equalp)->function())
        test = S_equalp;
    }
  if (symbolp(test))
    {
      HashTable * ht = NULL;
      if (test == S_eq)
        ht = new EqHashTable(n, rehash_size, rehash_threshold);
      else if (test == S_eql)
        ht = new EqlHashTable(n, rehash_size, rehash_threshold);
      else if (test == S_equal)
        ht = new EqualHashTable(n, rehash_size, rehash_threshold);
      else if (test == S_equalp)
        ht = new EqualpHashTable(n, rehash_size, rehash_threshold);
      if (ht)
        return make_value(ht);
    }
  String * string = new String("Unsupported test for MAKE-HASH-TABLE: ");
  string->append(::prin1_to_string(test));
  return signal_lisp_error(string);
}

// ### hash-table-p
Value CL_hash_table_p(Value arg)
{
  return hash_table_p(arg) ? T : NIL;
}

// ### hash-table-size
Value CL_hash_table_size(Value arg)
{
  return make_unsigned_integer(check_hash_table(arg)->size());
}

// ### hash-table-count
Value CL_hash_table_count(Value arg)
{
  return make_unsigned_integer(check_hash_table(arg)->count());
}

// ### hash-table-test
Value CL_hash_table_test(Value arg)
{
  return check_hash_table(arg)->test();
}

// ### hash-table-rehash-size
Value CL_hash_table_rehash_size(Value arg)
{
  return check_hash_table(arg)->rehash_size();
}

// ### hash-table-rehash-threshold
Value CL_hash_table_rehash_threshold(Value arg)
{
  return check_hash_table(arg)->rehash_threshold();
}

Value RT_gethash2(Thread * thread, Value arg1, Value arg2)
{
  Value value = the_hash_table(arg2)->get(arg1);
  Value present_p;
  if (value == NULL_VALUE)
    {
      value = NIL;
      present_p = NIL;
    }
  else
    present_p = T;
  return thread->set_values(value, present_p);
}

// ### gethash2 key hash-table => value, present-p
Value SYS_gethash2(Value arg1, Value arg2)
{
  Value value = check_hash_table(arg2)->get(arg1);
  Value present_p;
  if (value == NULL_VALUE)
    {
      value = NIL;
      present_p = NIL;
    }
  else
    present_p = T;
  return current_thread()->set_values(value, present_p);
}

Value RT_gethash3(Thread * thread, Value arg1, Value arg2, Value arg3)
{
  Value value = the_hash_table(arg2)->get(arg1);
  Value present_p;
  if (value == NULL_VALUE)
    {
      value = arg3;
      present_p = NIL;
    }
  else
    present_p = T;
  return thread->set_values(value, present_p);
}

// ### gethash3 key hash-table default => value, present-p
Value SYS_gethash3(Value arg1, Value arg2, Value arg3)
{
  Value value = check_hash_table(arg2)->get(arg1);
  Value present_p;
  if (value == NULL_VALUE)
    {
      value = arg3;
      present_p = NIL;
    }
  else
    present_p = T;
  return current_thread()->set_values(value, present_p);
}

// ### gethash2-1 key hash-table => value
Value SYS_gethash2_1(Value arg1, Value arg2)
{
  Value value = check_hash_table(arg2)->get(arg1);
  return value != NULL_VALUE ? value : NIL;
}

// ### %gethash2-1 key hash-table => value
Value SYS_xgethash2_1(Value arg1, Value arg2)
{
  Value value = the_hash_table(arg2)->get(arg1);
  return value != NULL_VALUE ? value : NIL;
}

// ### gethash key hash-table &optional default => value, present-p
Value CL_gethash(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 3)
    return wrong_number_of_arguments(S_gethash, numargs, 2, 3);
  Value value = check_hash_table(args[1])->get(args[0]);
  Value present_p;
  if (value == NULL_VALUE)
    {
      value = (numargs == 3) ? args[2] : NIL;
      present_p = NIL;
    }
  else
    present_p = T;
  return current_thread()->set_values(value, present_p);
}

// ### puthash3 key hash-table new-value => new-value
Value SYS_puthash3(Value arg1, Value arg2, Value arg3)
{
  check_hash_table(arg2)->put(arg1, arg3);
  return arg3;
}

// ### puthash4 key hash-table default new-value => new-value
Value SYS_puthash4(Value arg1, Value arg2, Value arg3, Value arg4)
{
  check_hash_table(arg2)->put(arg1, arg4);
  return arg4;
}

// ### puthash
// (setf (gethash key hash-table &optional default) new-value) => new-value
Value SYS_puthash(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 3:
      check_hash_table(args[1])->put(args[0], args[2]);
      return args[2];
    case 4:
      check_hash_table(args[1])->put(args[0], args[3]);
      return args[3];
    default:
      return wrong_number_of_arguments(S_puthash, numargs, 3, 4);
    }
}

// ### hash-table-entries hash-table => entries
Value SYS_hash_table_entries(Value arg)
{
  return check_hash_table(arg)->entries();
}

// ### remhash key hash-table => generalized-boolean
Value CL_remhash(Value key, Value hash_table)
{
  return (check_hash_table(hash_table)->remove(key) != NULL_VALUE) ? T : NIL;
}

// ### clrhash hash-table => hash-table
Value CL_clrhash(Value hash_table)
{
  check_hash_table(hash_table)->clear();
  return hash_table;
}

// ### maphash function hash-table => nil
Value CL_maphash(Value function, Value hash_table)
{
  check_hash_table(hash_table)->maphash(check_function(function));
  return NIL;
}
