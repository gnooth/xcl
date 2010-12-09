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

unsigned long * samples;
unsigned long max_samples;
unsigned long samples_index;
unsigned long sample_interval;

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

#ifdef WIN32
// WINBASEAPI HANDLE WINAPI OpenThread(DWORD,BOOL,DWORD);

DWORD WINAPI sigprof_thread_proc(LPVOID lpParam)
{
  printf("; Profiler started. Sample interval is %lu milliseconds.\n", sample_interval);
  fflush(stdout);
  HANDLE h = OpenThread(THREAD_GET_CONTEXT | THREAD_SUSPEND_RESUME,
//                         THREAD_ALL_ACCESS,
                        FALSE,
                        profiled_thread->id());
  profiled_thread_handle = h;
  printf("handle = 0x%08lx\n", (unsigned long) profiled_thread_handle);
  fflush(stdout);
  if (profiled_thread_handle != NULL)
    {
      while (profiling)
        {
//           printf("got to here 1\n");
//           fflush(stdout);
          SuspendThread(profiled_thread_handle);
//           printf("ret = 0x%08lx\n", ret);
//           fflush(stdout);
          CONTEXT context;
          ZeroMemory(&context, sizeof(context));
#define CONTEXT_ALL (CONTEXT_CONTROL | CONTEXT_INTEGER | CONTEXT_SEGMENTS | CONTEXT_FLOATING_POINT | CONTEXT_DEBUG_REGISTERS)
          context.ContextFlags = CONTEXT_ALL;
          BOOL ret2 = GetThreadContext(profiled_thread_handle, &context);
//           printf("ret2 = %d\n", ret2);
//           fflush(stdout);
          if (ret2 == 0)
            {
              printf("GetLastError() = %ld\n", GetLastError());
              fflush(stdout);
              break;
            }
//           printf("0x%08lx\n", context.Eip);
//           fflush(stdout);
          if (samples_index < max_samples)
            samples[samples_index++] = (unsigned long) context.Eip;

          ++profiler_sample_count;

          ResumeThread(profiled_thread_handle);
//           printf("got to here 2\n");
//           fflush(stdout);
          Sleep(sample_interval);
        }
    }
  if (h)
    CloseHandle(h);
  return 0;
}
#else
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
// void sigprof_handler(int n)
void sigprof_handler(int sig, siginfo_t *si, void * context)
{
  if (profiling && current_thread() == profiled_thread)
    {
      ucontext_t * uc = (ucontext_t *) context;
      void * rip = (void *) uc->uc_mcontext.gregs[REG_RIP];
//       printf("sigprof_handler rip = 0x%lx\n", (unsigned long) rip);
      if (samples_index < max_samples)
        samples[samples_index++] = (unsigned long) rip;
    }
}
#endif

static void zero_call_count(Symbol * symbol)
{
  TypedObject * function = symbol->function();
  if (function)
//     {
//       switch (function->widetag())
//         {
//         case WIDETAG_FUNCTION:
//         case WIDETAG_PRIMITIVE:
//         case WIDETAG_CLOSURE:
//           ((Function *)function)->set_call_count(0);
//           break;
//         default:
//           break;
//         }
//     }
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

// ### start-profiler
Value PROF_start_profiler(Value max_depth)
{
//   Thread * const thread = current_thread();
  if (profiling)
    {
      printf("; Profiler already started.\n");
      fflush(stdout);
      return T;
    }

  profiler_max_depth = check_index(max_depth);
  zero_call_counts();
  profiler_sample_count = 0;
  profiling = true;
  profiled_thread = current_thread();

#ifdef WIN32
  if (current_thread()->symbol_value(S_sampling_mode) != NIL) // FIXME
    {
      max_samples = 50000;
      samples = (unsigned  long *) GC_malloc_atomic(max_samples * sizeof(unsigned long *));
      samples_index = 0;
      sample_interval = fixnum_value(current_thread()->symbol_value(S_sample_interval));

      DWORD id;
      HANDLE h = GC_CreateThread(NULL, // default security descriptor
                                 0,    // default stack size
                                 sigprof_thread_proc,
                                 NULL,
                                 0,    // default creation flags
                                 &id);
      return (h != NULL) ? T : NIL;
    }
  else
    {

      DWORD id;
      HANDLE h = GC_CreateThread(NULL, // default security descriptor
                                 0,    // default stack size
                                 profiler_thread_proc,
                                 NULL,
                                 0,    // default creation flags
                                 &id);
      return (h != NULL) ? T : NIL;
    }
#endif

#ifndef WIN32
  // Linux, FreeBSD
  if (current_thread()->symbol_value(S_sampling_mode) != NIL) // FIXME
    {
      max_samples = 50000;
      samples = (unsigned  long *) GC_malloc_atomic(max_samples * sizeof(unsigned long *));
      samples_index = 0;
      sample_interval = fixnum_value(current_thread()->symbol_value(S_sample_interval));

      struct sigaction sact;
      sigemptyset(&sact.sa_mask);
      sact.sa_flags = SA_SIGINFO;
      sact.sa_sigaction = sigprof_handler;
      sigaction(SIGPROF, &sact, NULL);

#ifdef USE_ITIMER
      struct itimerval value, ovalue, pvalue;
      int which = ITIMER_PROF;
      //           struct sigaction sact;
      //           sigemptyset(&sact.sa_mask);
      // //           sact.sa_flags = 0;
      //           sact.sa_flags = SA_SIGINFO;
      // //           sact.sa_handler = sigprof_handler;
      //           sact.sa_sigaction = sigprof_handler;
      //           sigaction(SIGPROF, &sact, NULL);
      getitimer(which, &pvalue);
      value.it_interval.tv_sec = 0;
      value.it_interval.tv_usec = 1000; // 1 millisecond
      value.it_value.tv_sec = 0;
      value.it_value.tv_usec = 1000;
      setitimer(which, &value, &ovalue);
      if (ovalue.it_interval.tv_sec != pvalue.it_interval.tv_sec
          || ovalue.it_interval.tv_usec != pvalue.it_interval.tv_usec
          || ovalue.it_value.tv_sec != pvalue.it_value.tv_sec
          || ovalue.it_value.tv_usec != pvalue.it_value.tv_usec)
        return signal_lisp_error( "Real time interval timer mismatch.");
#else
      pthread_t id;
      long status = GC_pthread_create(&id,
                                      NULL,
                                      sigprof_thread_proc,
                                      NULL);
      if (status != 0)
        {
          printf("GC_pthread_create status = %ld\n", status);
          fflush(stdout);
          return NIL; // Error!
        }
      status = GC_pthread_detach(id);
      if (status != 0)
        {
          printf("GC_pthread_create status = %ld\n", status);
          fflush(stdout);
          return NIL; // Error!
        }
      sched_yield();
#endif
    }
  else
    {
      pthread_t id;
      long status = GC_pthread_create(&id,
                                      NULL,
                                      profiler_thread_proc,
                                      NULL);
      if (status != 0)
        {
          printf("GC_pthread_create status = %ld\n", status);
          fflush(stdout);
          return NIL; // Error!
        }
      status = GC_pthread_detach(id);
      if (status != 0)
        {
          printf("GC_pthread_create status = %ld\n", status);
          fflush(stdout);
          return NIL; // Error!
        }
      sched_yield();
    }
#endif
  return T;
}

// ### stop-profiler
Value PROF_stop_profiler()
{
  if (profiling)
    {
      profiled_thread = NULL;
#ifndef WIN32
#ifdef USE_ITIMER
      struct itimerval value;
      getitimer(ITIMER_PROF, &value);
      value.it_interval.tv_sec = 0;
      value.it_interval.tv_usec = 0;
      value.it_value.tv_sec = 0;
      value.it_value.tv_usec = 0;
      setitimer(ITIMER_PROF, &value, &value);
#endif
#endif
      profiling = false;

      SimpleVector * vector = the_simple_vector(SYS_make_simple_vector(make_number(samples_index)));
      for (INDEX i = 0; i < samples_index; i++)
        vector->aset(i, make_number(samples[i]));
      the_symbol(S_samples)->set_value(make_value(vector));

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
