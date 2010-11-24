// Thread.hpp
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

#ifndef __THREAD_HPP
#define __THREAD_HPP

#include "call_depth_limit.hpp"

#define EXPERIMENTAL

#ifdef WIN32
typedef DWORD ThreadId;
#else
typedef pthread_t ThreadId;
#endif

class Frame;
class Block;
class Catch;
class Tagbody;
class UnwindProtect;
class Tag;

extern void handle_stack_overflow();

class Values : public gc
{
private:
  char _values_length;
  Value _values[MULTIPLE_VALUES_LIMIT];

public:
  Values(char values_length, Value * values)
    : _values_length(values_length)
  {
    for (int i = 0; i < MULTIPLE_VALUES_LIMIT; i++)
      _values[i] = values[i];
  }

  char values_length() const
  {
    return _values_length;
  }

  Value * values()
  {
    return _values;
  }
};

class Thread : public TypedObject
{
  friend Value SYS_make_thread_internal(Value arg1, Value arg2);

private:
  Function * _function;
  ThreadId _id;
  Value _name;

  Value * _thread_local_values;

#ifdef EXPERIMENTAL
  INDEX _binding_stack_index;
  INDEX _binding_stack_capacity;
#else
  int _binding_stack_index;
  int _binding_stack_capacity;
#endif

  int _num_bindings;
  int _max_bindings;

  Frame * _last_control_frame;

  char _values_length;
  Value _values[MULTIPLE_VALUES_LIMIT];

  class StackFrame * _stack;

  UnwindProtect * _unwind_protect;
  UnwindProtect * _uwp_in_cleanup;

  unsigned int _call_depth;

  Value * _binding_stack_base;
  Value * _binding_stack_end;

  Tag * _last_tag;

  class StackFramePool * _stack_frame_pool;

public:
  Thread(Function * function, Value name);

  virtual ~Thread();

  Value name() const
  {
    return _name;
  }

  virtual Value type_of() const
  {
    return S_thread;
  }

  Function * function()
  {
    return _function;
  }

  ThreadId id()
  {
    return _id;
  }

  void set_id(ThreadId id)
  {
    _id = id;
  }

  void * last_special_binding()
  {
    return (void *) _binding_stack_index;
  }

  void unbind_to(INDEX n);

  void set_last_special_binding(void * p)
  {
#ifdef EXPERIMENTAL
    unbind_to((INDEX)p);
#else
    _binding_stack_index = (int) (long) p;
#endif
  }

  Frame * last_control_frame() const
  {
    return _last_control_frame;
  }

  void set_last_control_frame(Frame * control_frame)
  {
    _last_control_frame = control_frame;
  }

  void add_frame(Frame * frame)
  {
    _last_control_frame = frame;
  }

  Block * add_block(Value name);

  Block * find_block(Value name);

  Block * find_block(Block * block);

  void dump_frames();

  Tag * last_tag() const
  {
    return _last_tag;
  }

  void set_last_tag(Tag * tag)
  {
    _last_tag = tag;
  }

  Tag * add_tag(Value name, Tagbody * tagbody, Value continuation, int index);

  Tag * add_tag(Value name, Tagbody * tagbody, Value continuation);

  Tag * add_tag(Value name, Tagbody * tagbody, int index);

  Tag * find_tag(Value name);

  Tag * find_tag(Tagbody * tagbody, int index);

  Catch * add_catch_frame(Value tag);

  Frame * find_catch_frame(Value tag);

  Value symbol_thread_local_value(Symbol * sym)
  {
    INDEX i = sym->binding_index();
    if (i == 0)
      return NO_THREAD_LOCAL_VALUE;
    return _thread_local_values[i];
  }

  void set_symbol_thread_local_value(Symbol * sym, Value value)
  {
    INDEX i = sym->binding_index();
    if (i == 0)
      i = sym->assign_binding_index();
    _thread_local_values[i] = value;
  }

  Value symbol_value(Value name)
  {
#ifdef EXPERIMENTAL
    Value value = symbol_thread_local_value(the_symbol(name));
    if (value != NO_THREAD_LOCAL_VALUE)
      return value;
#else
    int index = _binding_stack_index;
    while (index > 0)
      {
        if (_binding_stack_base[--index] == name)
          return _binding_stack_base[index - 1];
        else
          --index;
      }
#endif
    return the_symbol(name)->value();
  }

  Value set_symbol_value(Value name, Value value);

  void bind_special(Value name, Value value)
  {
#ifdef EXPERIMENTAL
    // the stack grows up from the base
    if (_binding_stack_index < _binding_stack_capacity)
      {
        Symbol * sym = the_symbol(name);
        INDEX i = sym->binding_index();
        Value old_value;
        if (i == 0)
          {
            i = sym->assign_binding_index();
            old_value = NO_THREAD_LOCAL_VALUE;
          }
        else
          old_value = _thread_local_values[i];
        _binding_stack_base[_binding_stack_index++] = old_value;
        _binding_stack_base[_binding_stack_index++] = name;
        _thread_local_values[i] = value;
      }
    else
      slow_bind_special(name, value);
#else
    // the stack grows up from the base
    if (_binding_stack_index < _binding_stack_capacity)
      {
        _binding_stack_base[_binding_stack_index++] = value;
        _binding_stack_base[_binding_stack_index++] = name;
      }
    else
      slow_bind_special(name, value);
#endif
  }

  void slow_bind_special(Value name, Value value);

  Value lookup_special(Value name);

  void bind_special_to_current_value(Value name);

  int values_length() const
  {
    return _values_length;
  }

  void set_values_length(int n)
  {
    _values_length = n;
  }

  long values_length_offset();

  long values_offset();

  long last_special_binding_offset();

  void clear_values()
  {
    _values_length = -1;
  }

  Value * values()
  {
    return _values;
  }

  Value * get_values(Value primary_value, int required);

  Value nth_value(int i)
  {
    assert(i >= 0);
    assert(i <= MULTIPLE_VALUES_LIMIT);
    return _values[i];
  }

  Value set_values()
  {
    _values_length = 0;
    return NIL;
  }

  Value set_values(Value value)
  {
    _values[0] = value;
    _values_length = 1;
    return value;
  }

  Value set_values(Value v1, Value v2)
  {
    _values[0] = v1;
    _values[1] = v2;
    _values_length = 2;
    return v1;
  }

  Value set_values(Value v1, Value v2, Value v3)
  {
    _values[0] = v1;
    _values[1] = v2;
    _values[2] = v3;
    _values_length = 3;
    return v1;
  }

  Value set_values(Value v1, Value v2, Value v3, Value v4)
  {
    _values[0] = v1;
    _values[1] = v2;
    _values[2] = v3;
    _values[3] = v4;
    _values_length = 4;
    return v1;
  }

  Value set_values(int n, Value * values)
  {
    _values_length = n;
    if (n > 0 && values)
      {
        for (int i = 0; i < n; i++)
          _values[i] = values[i];
        return values[0];
      }
    else
      return NIL;
  }

  Values * copy_values()
  {
    return new Values(_values_length, _values);
  }

  Values * copy_values(Value primary_value)
  {
    Values * v;
    if (_values_length < 0)
      {
        v = new Values(1, _values);
        Value * values = v->values();
        values[0] = primary_value;
      }
    else
      v = new Values(_values_length, _values);
    return v;
  }

  Value set_values(Values * v)
  {
    _values_length = v->values_length();
    Value * values = v->values();
    for (int i = MULTIPLE_VALUES_LIMIT; i-- > 0;)
      _values[i] = values[i];
    if (_values_length == 0)
      return NIL;
    if (_values_length == 1)
      clear_values();
    return values[0];
  }

  class StackFrame * stack() const
  {
    return _stack;
  }

  void set_stack(class StackFrame * stack)
  {
    _stack = stack;
  }

  unsigned int call_depth() const
  {
    return _call_depth;
  }

  void set_call_depth(unsigned int n)
  {
//     if (n > 4096)
//       asm("int3");
    if (n > call_depth_limit)
      handle_stack_overflow();
    _call_depth = n;
  }

  StackFrame * push_stack_frame(class StackFrame * frame);

  StackFrame * get_stack_frame();

  void release_stack_frame(class StackFrame * frame);

  UnwindProtect * unwind_protect() const
  {
    return _unwind_protect;
  }

  void set_unwind_protect(UnwindProtect * uwp)
  {
    _unwind_protect = uwp;
  }

  // used by RT_unwind_to() and RT_uwp_in_cleanup_p()
  UnwindProtect * uwp_in_cleanup() const
  {
    return _uwp_in_cleanup;
  }

  void set_uwp_in_cleanup(UnwindProtect * uwp)
  {
    _uwp_in_cleanup = uwp;
  }

  Value execute(TypedObject * function);
  Value execute(TypedObject * function, Value arg);
  Value execute(TypedObject * function, Value arg1, Value arg2);
  Value execute(TypedObject * function, Value arg1, Value arg2, Value arg3);
  Value execute(TypedObject * function, Value arg1, Value arg2, Value arg3,
                Value arg4);
  Value execute(TypedObject * function, Value arg1, Value arg2, Value arg3,
                Value arg4, Value arg5);
  Value execute(TypedObject * function, Value arg1, Value arg2, Value arg3,
                Value arg4, Value arg5, Value arg6);
  Value execute(TypedObject * function, unsigned int numargs, Value args[]);

  void increment_call_counts();

  Value backtrace_as_list(long limit);

  Value list_call_history();

  Value backtrace_internal();

  Value special_bindings();

  virtual AbstractString * write_to_string();
};

inline bool threadp(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_THREAD);
}

inline Thread * the_thread(Value value)
{
  assert(threadp(value));
  return reinterpret_cast<Thread *>(value - LOWTAG_TYPED_OBJECT);
}

inline Thread * check_thread(Value value)
{
  if (threadp(value))
    return the_thread(value);
  signal_type_error(value, S_thread);
  // not reached
  return NULL;
}

#ifdef WIN32
extern DWORD tls_index;
#else
extern pthread_key_t tls_key;
#endif

inline Thread * current_thread()
{
#if defined(WIN32)
  return (Thread *) TlsGetValue(tls_index);
#else
  return (Thread *) pthread_getspecific(tls_key);
#endif
}

#endif // Thread.hpp
