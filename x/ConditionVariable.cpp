// ConditionVariable.cpp
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

class ConditionVariable : public TypedObject
{
private:
#ifndef WIN32
  pthread_cond_t _cond;
#endif

public:
  ConditionVariable();

#ifndef WIN32
  pthread_cond_t * cond()
  {
    return &_cond;
  }
#endif
};

inline bool condition_variable_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_CONDITION_VARIABLE);
}

inline ConditionVariable * the_condition_variable(Value value)
{
  assert(condition_variable_p(value));
  return reinterpret_cast<ConditionVariable *>(value - LOWTAG_TYPED_OBJECT);
}

inline ConditionVariable * check_condition_variable(Value value)
{
  if (condition_variable_p(value))
    return the_condition_variable(value);
  signal_type_error(value, S_condition_variable);
  // Not reached.
  return NULL;
}

ConditionVariable::ConditionVariable()
  : TypedObject(WIDETAG_CONDITION_VARIABLE)
{
#ifdef WIN32
  signal_lisp_error("CONDITION-WAIT needs code!\n");
#else
  pthread_cond_init(&_cond, NULL);
#endif
}

// ### make-condition-variable
Value EXT_make_condition_variable()
{
#ifdef WIN32
  return signal_lisp_error("MAKE-CONDITION-VARIABLE needs code!\n");
#else
  return make_value(new ConditionVariable());
#endif
}

// ### condition-wait condition-variable mutex
Value EXT_condition_wait(Value arg1, Value arg2)
{
#ifdef WIN32
  return signal_lisp_error("CONDITION-WAIT needs code!\n");
#else
  pthread_cond_wait(check_condition_variable(arg1)->cond(), check_mutex(arg2)->mutex());
  return NIL; // REVIEW return value
#endif
}

// ### condition-notify condition-variable
Value EXT_condition_notify(Value arg)
{
#ifdef WIN32
  return signal_lisp_error("CONDITION-NOTIFY needs code!\n");
#else
  pthread_cond_signal(check_condition_variable(arg)->cond());
  return NIL; // REVIEW return value
#endif
}

// ### condition-broadcast condition-variable
Value EXT_condition_broadcast(Value arg)
{
#ifdef WIN32
  return signal_lisp_error("CONDITION-BROADCAST needs code!\n");
#else
  pthread_cond_broadcast(check_condition_variable(arg)->cond());
  return NIL; // REVIEW return value
#endif
}
