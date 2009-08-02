// Mutex.cpp
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

#include "lisp.hpp"
#include "Mutex.hpp"

Mutex::Mutex()
  : TypedObject(WIDETAG_MUTEX), _name(NIL), _owner(NIL)
{
#ifdef WIN32
  _mutex = CreateMutex(NULL, FALSE, NULL);
#else
  _mutex = (pthread_mutex_t *) GC_malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(_mutex, NULL);
#endif
}

Mutex::Mutex(Value name)
  : TypedObject(WIDETAG_MUTEX), _name(name), _owner(NIL)
{
#ifdef WIN32
  _mutex = CreateMutex(NULL, FALSE, NULL);
#else
  _mutex = (pthread_mutex_t *) GC_malloc(sizeof(pthread_mutex_t));
  pthread_mutex_init(_mutex, NULL);
#endif
}

bool Mutex::lock()
{
#ifdef WIN32
  return WaitForSingleObject(_mutex, INFINITE) == WAIT_OBJECT_0;
#else
  return pthread_mutex_lock(_mutex) == 0;
#endif
}

bool Mutex::trylock()
{
#ifdef WIN32
  return WaitForSingleObject(_mutex, 0) == WAIT_OBJECT_0;
#else
  return pthread_mutex_trylock(_mutex) == 0;
#endif
}

bool Mutex::unlock()
{
#ifdef WIN32
  return ReleaseMutex(_mutex) != 0;
#else
  return pthread_mutex_unlock(_mutex) == 0;
#endif
}

AbstractString * Mutex::write_to_string()
{
  String * s = new String(the_symbol(S_mutex)->prin1_to_string());
  if (_name != NIL)
    {
      s->append_char(' ');
      s->append(::prin1_to_string(_name));
    }
  return unreadable_string(s);
}

// ### %make-mutex name
Value SYS_make_mutex_internal(Value name)
{
  return make_value(new Mutex(name));
}

// ### get-mutex mutex &optional owner wait-p => boolean
// owner defaults to the current thread
// wait-p defaults to true
Value EXT_get_mutex(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 3)
    return wrong_number_of_arguments(S_get_mutex, numargs, 1, 3);
  Mutex * mutex = check_mutex(args[0]);
  Value owner = numargs >= 2 ? args[1] : NIL;
  if (owner == NIL)
    owner = make_value(current_thread());
  bool wait = (numargs < 3 || args[2] != NIL);
  if (wait)
    {
      if (mutex->lock())
        {
          mutex->set_owner(owner);
          return T;
        }
    }
  else
    {
      if (mutex->trylock())
        {
          mutex->set_owner(owner);
          return T;
        }
    }
  return NIL;
}

// ### release-mutex mutex => boolean
Value EXT_release_mutex(Value arg)
{
  return check_mutex(arg)->unlock() ? T : NIL;
}
