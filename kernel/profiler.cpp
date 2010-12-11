// profiler.cpp
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

#ifndef WIN32
#include <signal.h>
#include <sys/time.h>
#include <ucontext.h>
#endif

#ifdef WIN32
#define _WIN32_WINNT 0x0502
#endif

#include "lisp.hpp"
#include "primitives.hpp"
#include "Package.hpp"
#include "profiler.hpp"

bool volatile profiling;
bool volatile sample_now;
Thread * volatile profiled_thread;
#ifdef WIN32
HANDLE volatile profiled_thread_handle;
#endif
unsigned long volatile profiler_sample_count;
INDEX profiler_max_depth;

INDEX volatile * samples;
INDEX volatile samples_size;
INDEX samples_index;
INDEX sample_interval;
Value sampling_mode;

#ifdef WIN32
DWORD WINAPI profiler_thread_proc(LPVOID lpParam)
{
  printf("; Profiler started.\n");
  fflush(stdout);
  while (profiling)
    {
      sample_now = true;
      Sleep(1); // 1 ms
    }
  return 0;
}
#else
void * profiler_thread_proc(void * arg)
{
  printf("; Profiler started.\n");
  fflush(stdout);
  while (profiling)
    {
      sample_now = true;
      usleep(1000); // 1 ms
    }
  return 0;
}
#endif

inline void resize_samples_vector()
{
  INDEX new_size = samples_size * 2;
  INDEX * new_samples = (INDEX *) GC_malloc_atomic(new_size * sizeof(unsigned long *));
  for (INDEX i = 0; i < samples_size; i++)
    new_samples[i] = samples[i];
  for (INDEX i = samples_size; i < new_size; i++)
    new_samples[i] = 0;
  samples = new_samples;
  samples_size = new_size;
}

#ifdef WIN32
DWORD WINAPI sigprof_thread_proc(LPVOID lpParam)
{
  printf("; Profiler started. Sample interval is %lu milliseconds.\n", sample_interval);
  fflush(stdout);
  HANDLE h = OpenThread(THREAD_GET_CONTEXT | THREAD_SUSPEND_RESUME,
                        FALSE,
                        profiled_thread->id());
  profiled_thread_handle = h;
  if (profiled_thread_handle != NULL)
    {
      while (profiling)
        {
          SuspendThread(profiled_thread_handle);
          CONTEXT context;
          ZeroMemory(&context, sizeof(context));
#define CONTEXT_ALL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS | CONTEXT_FLOATING_POINT | CONTEXT_DEBUG_REGISTERS)
          context.ContextFlags = CONTEXT_ALL;
          if (GetThreadContext(profiled_thread_handle, &context))
            {
              if (samples_index >= samples_size)
                resize_samples_vector();
              samples[samples_index++] = (INDEX) context.Eip;

              ++profiler_sample_count;
            }
          ResumeThread(profiled_thread_handle);
          Sleep(sample_interval);
        }
    }
  if (h)
    CloseHandle(h);
  return 0;
}
#else
// Linux, FreeBSD
void * sigprof_thread_proc(void * arg)
{
  printf("; Profiler started.\n");
  fflush(stdout);
  while (profiling)
    {
      pthread_kill(profiled_thread->id(), SIGPROF);
      usleep(1000); // 1 ms
    }
  return 0;
}
#endif

#ifndef WIN32
// Linux, FreeBSD
void sigprof_handler(int sig, siginfo_t *si, void * context)
{
  if (profiling && current_thread() == profiled_thread)
    {
      ucontext_t * uc = (ucontext_t *) context;
      void * rip = (void *) uc->uc_mcontext.gregs[REG_RIP];
      if (sampling_mode == K_time)
        {
          if (samples_index >= samples_size)
            resize_samples_vector();
          samples[samples_index++] = (INDEX) rip;
        }
      else if (samples_index < samples_size)
        samples[samples_index++] = (INDEX) rip;
    }
}
#endif

static void zero_call_count(Symbol * symbol)
{
  TypedObject * function = symbol->function();
  if (function)
    function->set_call_count(0);
}

static void zero_call_counts()
{
  Value packages = CL_list_all_packages();
  while (packages != NIL)
    {
      Package * package = check_package(car(packages));
      Value internal_symbols = package->internal_symbols();
      while (internal_symbols != NIL)
        {
          Symbol * symbol = the_symbol(car(internal_symbols));
          zero_call_count(symbol);
          internal_symbols = xcdr(internal_symbols);
        }
      Value external_symbols = package->external_symbols();
      while (external_symbols != NIL)
        {
          Symbol * symbol = the_symbol(car(external_symbols));
          zero_call_count(symbol);
          external_symbols = xcdr(external_symbols);
        }
      packages = xcdr(packages);
    }
}

#ifdef WIN32
void create_profiler_thread(LPTHREAD_START_ROUTINE start)
{
  DWORD id;
  HANDLE h = GC_CreateThread(NULL, // default security descriptor
                             0,    // default stack size
                             start,
                             NULL,
                             0,    // default creation flags
                             &id);
  if (!h)
    signal_lisp_error("Unable to create profiler thread.");
}
#else
void create_profiler_thread(void * (* start)(void *))
{
  pthread_t id;
  long status = GC_pthread_create(&id, NULL, start, NULL);
  if (status != 0)
    {
      printf("GC_pthread_create status = %ld\n", status);
      fflush(stdout);
      signal_lisp_error("Unable to create profiler thread.");
      return;
    }
  status = GC_pthread_detach(id);
  if (status != 0)
    {
      printf("GC_pthread_detach status = %ld\n", status);
      fflush(stdout);
      signal_lisp_error("Unable to detach profiler thread.");
      return;
    }
  sched_yield();
}
#endif

// ### start-profiler
Value PROF_start_profiler(Value max_depth)
{
  if (profiling)
    {
      printf("; Profiler already started.\n");
      fflush(stdout);
      return T;
    }

  Thread * const thread = current_thread();
  sampling_mode = thread->symbol_value(S_sampling_mode);

  profiler_max_depth = check_index(max_depth);
  zero_call_counts();
  profiler_sample_count = 0;
  profiling = true;
  profiled_thread = thread;

  if (sampling_mode == K_time || sampling_mode == K_cpu)
    {
      INDEX max_samples = check_index(thread->symbol_value(S_max_samples));
      samples = (INDEX *) GC_malloc_atomic(max_samples * sizeof(INDEX));
      samples_size = max_samples;
      samples_index = 0;
      sample_interval = check_index(thread->symbol_value(S_sample_interval));
    }

#ifdef WIN32
  if (sampling_mode == K_time) // FIXME
    create_profiler_thread(sigprof_thread_proc);
  else
    create_profiler_thread(profiler_thread_proc);
#endif

#ifndef WIN32
  // Linux, FreeBSD
  if (sampling_mode == K_time || sampling_mode == K_cpu)
    {
      struct sigaction sact;
      memset(&sact, 0, sizeof(sact));
      sigemptyset(&sact.sa_mask);
      sact.sa_flags = SA_SIGINFO;
      sact.sa_sigaction = sigprof_handler;
      sigaction(SIGPROF, &sact, NULL);

      if (sampling_mode == K_cpu)
        {
          struct itimerval value;
          memset(&value, 0, sizeof(value));
          INDEX interval_sec = 0;
          INDEX interval_usec = sample_interval * 1000;
          if (interval_usec >= 1000000)
            {
              interval_sec = interval_usec / 1000000;
              interval_usec = interval_usec % 1000000;
            }
          value.it_interval.tv_sec = interval_sec;
          value.it_interval.tv_usec = interval_usec;
          value.it_value.tv_sec = interval_sec;
          value.it_value.tv_usec = interval_usec;
          setitimer(ITIMER_PROF, &value, NULL);
        }
      else
        create_profiler_thread(sigprof_thread_proc);
    }
  else
      create_profiler_thread(profiler_thread_proc);
#endif
  return T;
}

// ### stop-profiler
Value PROF_stop_profiler()
{
  if (profiling)
    {
      profiled_thread = NULL;
//       Value sampling_mode = current_thread()->symbol_value(S_sampling_mode);
#ifndef WIN32
      if (sampling_mode == K_cpu)
        {
          struct itimerval value;
//           getitimer(ITIMER_PROF, &value);
//           value.it_interval.tv_sec = 0;
//           value.it_interval.tv_usec = 0;
//           value.it_value.tv_sec = 0;
//           value.it_value.tv_usec = 0;
          memset(&value, 0, sizeof(value));
          setitimer(ITIMER_PROF, &value, NULL);
        }
#endif
      profiling = false;

      if (sampling_mode == K_time || sampling_mode == K_cpu)
        {
          SimpleVector * vector = the_simple_vector(SYS_make_simple_vector(make_number(samples_index)));
          for (INDEX i = 0; i < samples_index; i++)
            vector->aset(i, make_number(samples[i]));
          the_symbol(S_samples)->set_value(make_value(vector));
        }

      printf("; Profiler stopped.\n");
    }
  else
    printf("; Profiler was not started.\n");
  fflush(stdout);
  return T;
}

// ### sample-count
Value PROF_sample_count()
{
    return make_unsigned_integer(profiler_sample_count);
}
