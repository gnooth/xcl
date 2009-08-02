// Mutex.hpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

#ifndef __MUTEX_HPP
#define __MUTEX_HPP

class Mutex : public TypedObject
{
private:
#ifdef WIN32
  HANDLE _mutex;
#else
  pthread_mutex_t * _mutex;
#endif
  Value _name;
  Value _owner;

public:
  Mutex();

  Mutex(Value name);

#ifdef WIN32
#else
  pthread_mutex_t * mutex()
  {
    return _mutex;
  }
#endif

  Value name() const
  {
    return _name;
  }

  Value owner() const
  {
    return _owner;
  }

  void set_owner(Value owner)
  {
    _owner = owner;
  }

  Value type_of() const
  {
    return S_mutex;
  }

  bool lock();

  bool trylock();

  bool unlock();

  virtual AbstractString * write_to_string();
};

inline bool mutexp(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_MUTEX);
}

inline Mutex * the_mutex(Value value)
{
  assert(mutexp(value));
  return reinterpret_cast<Mutex *>(value - LOWTAG_TYPED_OBJECT);
}

inline Mutex * check_mutex(Value value)
{
  if (mutexp(value))
    return the_mutex(value);
  signal_type_error(value, S_mutex);
  // Not reached.
  return NULL;
}

#endif
