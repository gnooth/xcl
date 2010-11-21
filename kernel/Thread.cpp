// Thread.cpp
//
// Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

#if defined(WIN32)
#include <malloc.h>
#elif defined(__NetBSD__) || defined(__FreeBSD__)
#include <stdlib.h>
#else
#include <alloca.h>
#endif

#include <errno.h>

#include "lisp.hpp"
#include "primitives.hpp"
#include "Frame.hpp"
#include "Mutex.hpp"
#include "Ratio.hpp"
#include "StackFrame.hpp"
#include "StackFramePool.hpp"
#include "StorageCondition.hpp"
#include "UnboundVariable.hpp"
#include "profiler.hpp"

#include "../mpfr/mpfr.h"

unsigned int call_depth_limit = DEFAULT_CALL_DEPTH_LIMIT;

volatile int interrupted;

Thread * primordial_thread;

static Mutex * all_threads_mutex;

Mutex * binding_index_mutex;

#ifdef WIN32
DWORD tls_index;
#else
pthread_key_t tls_key;
#endif

#define USE_STACK_FRAME

#define MAX_THREAD_LOCAL_VALUES         4096

#define INITIAL_BINDING_STACK_SIZE      4096 // should be 16

Thread::Thread(Function * function, Value name)
  : TypedObject(WIDETAG_THREAD), _function(function), _id(0), _name(name)
{
  _stack = 0;

  _thread_local_values = (Value *) GC_malloc(MAX_THREAD_LOCAL_VALUES * sizeof(Value));
  for (unsigned long i = 0; i < MAX_THREAD_LOCAL_VALUES; i++)
    _thread_local_values[i] = NO_THREAD_LOCAL_VALUE;

  unsigned long size = INITIAL_BINDING_STACK_SIZE * sizeof(Value);
  _binding_stack_base = (Value *) GC_malloc(size);
  memset(_binding_stack_base, 0, size);
  _binding_stack_end = (Value *)(((char *)_binding_stack_base) + size);

  _binding_stack_index = 0;
  _binding_stack_capacity = INITIAL_BINDING_STACK_SIZE;

  _num_bindings = _max_bindings = 0;

  _stack_frame_pool = new StackFramePool();
}

Thread::~Thread()
{
  printf("Thread destructor called\n");
}

Value Thread::set_symbol_value(Value name, Value value)
{
  assert(symbolp(name));
#ifdef EXPERIMENTAL
  INDEX i = the_symbol(name)->binding_index();
  if (i != 0 && _thread_local_values[i] != NO_THREAD_LOCAL_VALUE)
    set_symbol_thread_local_value(the_symbol(name), value);
  else
    the_symbol(name)->set_value(value);
  return value;
#else
  int index = _binding_stack_index;
  while (index > 0)
    {
      if (_binding_stack_base[--index] == name)
        {
          _binding_stack_base[index - 1] = value;
          return value;
        }
      else
        --index;
    }
  the_symbol(name)->set_value(value);
  return value;
#endif
}

void Thread::slow_bind_special(Value name, Value value)
{
  if (_binding_stack_index >= _binding_stack_capacity)
    {
      INDEX new_capacity = _binding_stack_capacity * 2;
      INDEX old_size = _binding_stack_capacity * sizeof(Value);
      INDEX new_size = new_capacity * sizeof(Value);
      printf("\nresizing binding stack from %lu entries to %lu entries...\n",
             old_size / sizeof(Value), new_size / sizeof(Value));
      fflush(stdout);
      Value * new_base = (Value *) GC_malloc(new_size);
      memset(new_base, 0, new_size);
      memcpy(new_base, _binding_stack_base, old_size);
      _binding_stack_base = new_base;
      _binding_stack_end = (Value *) (((char *)_binding_stack_base) + new_size);

      _binding_stack_capacity = new_capacity;

      printf("resizing binding stack completed _binding_stack_capacity = %lu\n", _binding_stack_capacity);
    }
#ifdef EXPERIMENTAL
  _binding_stack_base[_binding_stack_index++] = symbol_thread_local_value(the_symbol(name));
  _binding_stack_base[_binding_stack_index++] = name;
  set_symbol_thread_local_value(the_symbol(name), value);
#else
  _binding_stack_base[_binding_stack_index++] = value;
  _binding_stack_base[_binding_stack_index++] = name;
#endif

//   _num_bindings = _binding_stack_index / 2;
//   if (_num_bindings > _max_bindings)
//     {
//       _max_bindings = _num_bindings;
//       if (this == primordial_thread)
//         {
//           printf("_max_bindings = %d\n", _max_bindings);
//           fflush(stdout);
//         }
//     }
}

Value Thread::lookup_special(Value name)
{
#ifdef EXPERIMENTAL
  Value value = symbol_thread_local_value(the_symbol(name));
  if (value != NO_THREAD_LOCAL_VALUE)
    return value;
  return NULL_VALUE;
#else
  int index = _binding_stack_index;
  while (index > 0)
    {
      if (_binding_stack_base[--index] == name)
        return _binding_stack_base[index - 1];
      else
        --index;
    }
  return NULL_VALUE;
#endif
}

void Thread::unbind_to(INDEX n)
{
  if (n >= _binding_stack_capacity)
    {
      printf("unbind_to n = %lu _binding_stack_capacity = %lu\n", n, _binding_stack_capacity);
      fflush(stdout);
      assert(n < _binding_stack_capacity);
      EXT_quit();
    }
  INDEX index = _binding_stack_index;
  while (index > n)
    {
      Value name = _binding_stack_base[--index];
      _binding_stack_base[index] = 0;
      Value value = _binding_stack_base[--index];
      _binding_stack_base[index] = 0;
      set_symbol_thread_local_value(the_symbol(name), value);
    }
  assert(index == n);
  _binding_stack_index = n;
}

void Thread::bind_special_to_current_value(Value name)
{
#ifdef EXPERIMENTAL
  Symbol * sym = the_symbol(name);
  Value value;
  INDEX i = sym->binding_index();
  if (i > 0)
    {
      value = _thread_local_values[i];
      if (value == NO_THREAD_LOCAL_VALUE)
        value = sym->value();
    }
  else
    {
      i = sym->assign_binding_index();
      value = sym->value();
    }
  if (value == NULL_VALUE)
    signal_lisp_error(new UnboundVariable(name));
  // the stack grows up from the base
  if (_binding_stack_index < _binding_stack_capacity)
    {
      _binding_stack_base[_binding_stack_index++] = value;
      _binding_stack_base[_binding_stack_index++] = name;
      _thread_local_values[i] = value;
    }
  else
    slow_bind_special(name, value);
#else
  int index = _binding_stack_index;
  while (index > 0)
    {
      if (_binding_stack_base[--index] == name)
        {
          bind_special(name, _binding_stack_base[index - 1]);
          return;
        }
      else
        --index;
    }
  bind_special(name, the_symbol(name)->value());
#endif
}

Value * Thread::get_values(Value primary_value, int required)
{
  if (_values_length < 0)
    {
      // one value
      _values[0] = primary_value;
      for (int i = 1; i < required; i++)
        _values[i] = NIL;
    }
  else
    {
      for (int i = _values_length; i < required; i++)
        _values[i] = NIL;
    }
  _values_length = required;
  return _values;
}

Block * Thread::add_block(Value name)
{
  _last_control_frame = new Block(name, _last_control_frame, this);
  return (Block *) _last_control_frame;
}

Block * Thread::find_block(Value name)
{
  Frame * frame = _last_control_frame;
  while (frame)
    {
      if (frame->type() == BLOCK && ((Block *)frame)->name() == name)
        return (Block *) frame;
      frame = frame->next();
    }
  return NULL;
}

Block * Thread::find_block(Block * block)
{
  Frame * frame = _last_control_frame;
  while (frame)
    {
      if (frame->type() == BLOCK && ((Block *)frame == block))
        return (Block *) frame;
      frame = frame->next();
    }
  return NULL;
}

void Thread::dump_frames()
{
  Frame * frame = _last_control_frame;
  while (frame)
    {
      const char * type_string;
      FrameType type = frame->type();
      switch (type)
        {
        case PRIMORDIAL:
          type_string = "PRIMORDIAL";
          printf("0x%lx %s\n", (unsigned long) frame, type_string);
          break;
        case TAGBODY:
          type_string = "TAGBODY";
          printf("0x%lx %s\n", (unsigned long) frame, type_string);
          break;
        case BLOCK:
          type_string = "BLOCK";
          printf("0x%lx %s %s\n", (unsigned long) frame, type_string,
                 ::write_to_string(((Block *) frame)->name())->as_c_string());
          break;
        case CATCH:
          type_string = "CATCH";
          printf("0x%lx %s\n", (unsigned long) frame, type_string);
          break;
        case UNWIND_PROTECT:
          type_string = "UNWIND_PROTECT";
          printf("0x%lx %s\n", (unsigned long) frame, type_string);
          break;
        }
      frame = frame->next();
    }
}


Tag * Thread::add_tag(Value name, Tagbody * tagbody, Value continuation, int index)
{
  return (_last_tag = new Tag(name, tagbody, continuation, index, _last_tag));
}

Tag * Thread::add_tag(Value name, Tagbody * tagbody, Value continuation)
{
  return (_last_tag = new Tag(name, tagbody, continuation, _last_tag));
}

Tag * Thread::add_tag(Value name, Tagbody * tagbody, int index)
{
  return (_last_tag = new Tag(name, tagbody, index, _last_tag));
}

Tag * Thread::find_tag(Value name)
{
  Tag * tag = _last_tag;
  while (tag)
    {
      // "Tags are compared with EQL."
      if (::eql(tag->name(), name))
        return tag;
      tag = tag->next();
    }
  return NULL;
}

Tag * Thread::find_tag(Tagbody * tagbody, int index)
{
  Tag * tag = _last_tag;
  while (tag)
    {
      if (tag->tagbody() == tagbody && tag->index() == index)
        return tag;
      tag = tag->next();
    }
  return NULL;
}

Catch * Thread::add_catch_frame(Value tag)
{
  _last_control_frame = new Catch(tag, _last_control_frame, this);
  return (Catch *) _last_control_frame;
}

Frame * Thread::find_catch_frame(Value tag)
{
  Frame * frame = _last_control_frame;
  while (frame)
    {
      if (frame->type() == CATCH && ((Catch *)frame)->tag() == tag)
        return frame;
      frame = frame->next();
    }
  return NULL;
}

void handle_stack_overflow()
{
//   // FIXME when things settle down
//   printf("stack overflow!\n");
//   fflush(stdout);
//   asm("int3");
//   if (call_depth_limit == DEFAULT_CALL_DEPTH_LIMIT)
//     {
//       call_depth_limit += 100;
//       Thread * thread = current_thread();
//       Value value = thread->symbol_value(S_standard_output);
//       if (streamp(value))
//         the_stream(value)->write_string("; Stack overflow, resetting...\n");
//       signal_lisp_error(new StorageCondition("Stack overflow."));
//     }
//   else
//     {
//       call_depth_limit += 100;
//       EXT_reset();
//     }
  asm("int3");
}

inline StackFrame * Thread::get_stack_frame()
{
  StackFrame * frame = _stack_frame_pool->get_stack_frame();
  if (frame)
    return frame;
  return new StackFrame();
}

inline void Thread::release_stack_frame(class StackFrame * frame)
{
  _stack_frame_pool->release_stack_frame(frame);
}

inline StackFrame * Thread::push_stack_frame(StackFrame * frame)
{
  assert(frame != NULL);
  assert(frame->function() != NULL);
//   if (++_call_depth > call_depth_limit)
//     handle_stack_overflow();
  frame->set_next(_stack);
  _stack = frame;
  if (profiling)
    {
      if (sample_now && this == profiled_thread)
        {
          increment_call_counts();
          sample_now = false;
        }
    }
  return frame;
}

Value Thread::execute(TypedObject * function)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_0(function);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute();
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute();
#endif
}

Value Thread::execute(TypedObject * function, Value arg)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_1(function, arg);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(arg);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(arg);
#endif
}

Value Thread::execute(TypedObject * function, Value arg1, Value arg2)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_2(function, arg1, arg2);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(arg1, arg2);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(arg1, arg2);
#endif
}

Value Thread::execute(TypedObject * function, Value arg1, Value arg2, Value arg3)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_3(function, arg1, arg2, arg3);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(arg1, arg2, arg3);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(arg1, arg2, arg3);
#endif
}

Value Thread::execute(TypedObject * function, Value arg1, Value arg2, Value arg3,
                      Value arg4)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_4(function, arg1, arg2, arg3, arg4);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(arg1, arg2, arg3, arg4);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(arg1, arg2, arg3, arg4);
#endif
}

Value Thread::execute(TypedObject * function, Value arg1, Value arg2, Value arg3,
                      Value arg4, Value arg5)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_5(function, arg1, arg2, arg3, arg4, arg5);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(arg1, arg2, arg3, arg4, arg5);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(arg1, arg2, arg3, arg4, arg5);
#endif
}

Value Thread::execute(TypedObject * function, Value arg1, Value arg2, Value arg3,
                      Value arg4, Value arg5, Value arg6)
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  frame->initialize_6(function, arg1, arg2, arg3, arg4, arg5, arg6);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(arg1, arg2, arg3, arg4, arg5, arg6);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(arg1, arg2, arg3, arg4, arg5, arg6);
#endif
}

Value Thread::execute(TypedObject * function, unsigned int numargs, Value args[])
{
  clear_values();
#ifdef USE_STACK_FRAME
  StackFrame * oldstack = _stack;
  StackFrame * frame = get_stack_frame();
  Value * temp = NULL;
  if (numargs > 6)
    temp = (Value *) alloca(numargs * sizeof(Value));
  frame->initialize(function, numargs, args, temp);
  push_stack_frame(frame);
  set_call_depth(call_depth() + 1);
  Value result = function->execute(numargs, args);
  set_call_depth(call_depth() - 1);
  _stack = oldstack;
  release_stack_frame(frame);
  return result;
#else
  return function->execute(numargs, args);
#endif
}

void Thread::increment_call_counts()
{
  StackFrame * s = _stack;
  INDEX max_depth = profiler_max_depth;
  INDEX depth = 0;
  while (s && depth < max_depth)
    {
      StackFrame * frame = s;
      TypedObject * function = frame->function();
      if (function)
        function->increment_call_count();
      s = s->next();
      ++depth;
    }
  ++profiler_sample_count;
}

Value Thread::backtrace_as_list(long limit)
{
  Value result = NIL;
  if (_stack)
    {
      long count = 0;
      StackFrame * s = _stack;
      while (s)
        {
          StackFrame * frame = s;
          if (frame)
            {
              result = make_cons(frame->to_list(), result);
              if (limit > 0 && ++count == limit)
                break;
            }
          s = s->next();
        }
    }
  return CL_nreverse(result);
}

AbstractString * Thread::write_to_string()
{
  String * s = new String(the_symbol(S_thread)->prin1_to_string());
  if (_name != NIL)
    {
      s->append_char(' ');
      s->append(::prin1_to_string(_name));
    }
  return unreadable_string(s);
}

// ### backtrace-as-list
Value EXT_backtrace_as_list(unsigned int numargs, Value args[])
{
  if (numargs > 1)
    return wrong_number_of_arguments(S_backtrace_as_list, numargs, 0, 1);
  const long limit = (numargs == 1) ? fixnum_value(args[0]) : 0;
  return current_thread()->backtrace_as_list(limit);
}

#ifdef WIN32
DWORD WINAPI ThreadProc(LPVOID arg)
{
  Thread * thread = (Thread*) arg;
  assert(thread != NULL);

  TlsSetValue(tls_index, thread);

  Symbol * sym = the_symbol(S_all_threads);
  if (all_threads_mutex->lock())
    {
      sym->set_value(make_cons(make_value(thread), sym->value()));
      all_threads_mutex->unlock();
    }

  thread->function()->execute();

  if (all_threads_mutex->lock())
    {
      sym->set_value(SYS_list_delete_eq(make_value(thread), sym->value()));
      all_threads_mutex->unlock();
    }

  TlsSetValue(tls_index, 0);

  return 0; // FIXME return result ?
}
#else
void * thread_run(void * arg)
{
  Thread * thread = (Thread*) arg;
  assert(thread != NULL);

  pthread_setspecific(tls_key, thread);

  Symbol * sym = the_symbol(S_all_threads);
  if (all_threads_mutex->lock())
    {
      sym->set_value(make_cons(make_value(thread), sym->value()));
      all_threads_mutex->unlock();
    }

  Value result = thread->function()->execute();

  if (all_threads_mutex->lock())
    {
      sym->set_value(SYS_list_delete_eq(make_value(thread), sym->value()));
      all_threads_mutex->unlock();
    }

  pthread_setspecific(tls_key, 0);

  return (void *) result;
}
#endif

// ### %make-thread function name
Value SYS_make_thread_internal(Value arg1, Value arg2)
{
  if (functionp(arg1))
    {
      Function * function = the_function(arg1);
      Thread * thread = new Thread(function, arg2);
#if defined(WIN32)
      HANDLE h = GC_CreateThread(NULL, // default security descriptor
                                 0,    // default stack size
                                 ThreadProc,
                                 thread,
                                 0,    // default creation flags
                                 &(thread->_id));
      if (h == NULL)
        return NIL;
#else
      long status = GC_pthread_create(&(thread->_id),
                                      NULL,
                                      thread_run,
                                      thread);
      if (status != 0)
        return NIL; // Error!
#endif
      return make_value(thread);
    }
  return signal_type_error(arg1, S_function);
}

// ### current-thread
Value EXT_current_thread()
{
#if defined(WIN32)
  return make_value((Thread*)TlsGetValue(tls_index));
#else
  return make_value((Thread*)pthread_getspecific(tls_key));
#endif
}

// ### thread-p
Value EXT_threadp(Value arg)
{
  return threadp(arg) ? T : NIL;
}

// ### thread-name
Value EXT_thread_name(Value arg)
{
  return check_thread(arg)->name();
}

// ### sleep
Value CL_sleep(Value arg)
{
  if (fixnump(arg))
    {
      long n = xlong(arg);
      if (n >= 0)
        {
#ifdef WIN32
          DWORD millis;
          if (n > (long) (((unsigned long) 0xffffffff) / 1000))
            // overflow
            millis = 0xffffffff;
          else
            millis = (DWORD) n * 1000;
          Sleep(millis);
#else
          struct timespec tv;
          tv.tv_sec = (time_t) n;
          tv.tv_nsec = 0;
          while (nanosleep(&tv, &tv) && errno == EINTR)
            ;
#endif
          return NIL;
        }
      else
        goto error;
    }
  if (realp(arg))
    {
      mpfr_t(x);
      mpfr_init2(x, 24); // 24-bit precision
      if (bignump(arg))
          mpfr_set_z(x, the_bignum(arg)->_z, GMP_RNDN);
      else if (ratiop(arg))
          mpfr_set_q(x, the_ratio(arg)->_q, GMP_RNDN);
      else if (single_float_p(arg))
        mpfr_set_d(x, the_single_float(arg)->_f, GMP_RNDN);
      else if (double_float_p(arg))
        mpfr_set_d(x, the_double_float(arg)->_d, GMP_RNDN);
      else
        {
          // shouldn't happen
          mpfr_clear(x);
          goto error;
        }
      if (mpfr_sgn(x) < 0)
        {
          mpfr_clear(x);
          goto error;
        }
#ifdef WIN32
      // convert to milliseconds
      mpfr_mul_ui(x, x, 1000, GMP_RNDN);
      DWORD millis = mpfr_get_ui(x, GMP_RNDN);
      mpfr_clear(x);
      // "If dwMilliseconds is less than the resolution of the system clock,
      // the thread may sleep for less than the specified length of time."
      Sleep(millis);
      return NIL;
#else
      struct timespec tv;
      tv.tv_sec = mpfr_get_si(x, GMP_RNDD);
      mpfr_frac(x, x, GMP_RNDN);
      mpfr_mul_ui(x, x, 1000000000UL, GMP_RNDN);
      tv.tv_nsec = mpfr_get_si(x, GMP_RNDN);
      mpfr_clear(x);
      while (nanosleep(&tv, &tv) && errno == EINTR)
        ;
      return NIL;
#endif
    }
 error:
  return signal_type_error(arg, list2(S_real, FIXNUM_ZERO));
}

long Thread::values_length_offset()
{
  return ((long)(&(this->_values_length))) - ((long)this);
}

long Thread::values_offset()
{
  return ((long)(&(this->_values))) - ((long)this);
}

long Thread::last_special_binding_offset()
{
  return ((long)(&(this->_binding_stack_index))) - ((long)this);
}

// ### interrupt-lisp
Value SYS_interrupt_lisp()
{
  interrupted = 1;
  return T;
}

Value Thread::special_bindings()
{
  Value result = NIL;
  int index = _binding_stack_index;
  while (index > 0)
    {
      Value name = _binding_stack_base[--index];
      Value value = _binding_stack_base[--index];
      result = make_cons(make_cons(name, value), result);
    }
  return result;
}

// ### bindings
Value SYS_special_bindings()
{
  return current_thread()->special_bindings();
}

void initialize_threads()
{
  the_symbol(S_all_threads)->initialize_special(NIL);

#ifdef WIN32
  tls_index = TlsAlloc();
#else
  long status = pthread_key_create(&tls_key, NULL);
  if (status != 0)
    printf("pthread_key_create failed\n");
#endif

  all_threads_mutex = new Mutex();

  binding_index_mutex = new Mutex();

  primordial_thread = new Thread(0, make_simple_string("primordial thread"));
#ifdef WIN32
  primordial_thread->set_id(GetCurrentThreadId());
#else
  primordial_thread->set_id(pthread_self());
#endif

#ifdef WIN32
  TlsSetValue(tls_index, primordial_thread);
#else
  pthread_setspecific(tls_key, primordial_thread);
#endif

  all_threads_mutex->lock();
  Symbol * sym = the_symbol(S_all_threads);
  sym->set_value(make_cons(make_value(primordial_thread), sym->value()));
  all_threads_mutex->unlock();

  the_symbol(S_values_length_offset)->initialize_constant(make_fixnum(primordial_thread->values_length_offset()));
  the_symbol(S_values_offset)->initialize_constant(make_fixnum(primordial_thread->values_offset()));
  the_symbol(S_last_special_binding_offset)->initialize_constant(make_fixnum(primordial_thread->last_special_binding_offset()));
}
