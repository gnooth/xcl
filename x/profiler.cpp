// profiler.cpp
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

#ifndef WIN32
#include <signal.h>
#include <sys/time.h>
#endif

#include "lisp.hpp"
#include "primitives.hpp"
#include "Package.hpp"
#include "profiler.hpp"

bool volatile profiling;
bool volatile sample_now;
Thread * volatile profiled_thread;
unsigned long volatile profiler_sample_count;
INDEX profiler_max_depth;

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
//   printf("profiler_thread_proc starting\n");
//   fflush(stdout);
  printf("; Profiler started.\n");
  fflush(stdout);
  while (profiling)
    {
      sample_now = true;
      usleep(1000); // 1 ms
    }
//   printf("profiler_thread_proc returning\n");
//   fflush(stdout);
  return 0;
}
#endif

// #ifndef WIN32
// void sigprof_handler(int n)
// {
//   sample_now = true;
// }
// #endif

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

// ### start-profiler
Value SYS_start_profiler(Value max_depth)
{
//   Thread * const thread = current_thread();
  if (profiling)
    {
      printf("; Profiler already started.\n");
      fflush(stdout);
    }
  else
    {
      profiler_max_depth = check_index(max_depth);
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
      profiler_sample_count = 0;
      profiling = true;
      profiled_thread = current_thread();
#ifdef WIN32
      DWORD id;
      HANDLE h = GC_CreateThread(NULL, // default security descriptor
                                 0,    // default stack size
                                 profiler_thread_proc,
                                 NULL,
                                 0,    // default creation flags
                                 &id);
      if (h == NULL)
        return NIL;
#else
//       struct itimerval value, ovalue, pvalue;
//       int which = ITIMER_PROF;
//       struct sigaction sact;
//       sigemptyset(&sact.sa_mask);
//       sact.sa_flags = 0;
//       sact.sa_handler = sigprof_handler;
//       sigaction(SIGPROF, &sact, NULL);
//       getitimer(which, &pvalue);
//       value.it_interval.tv_sec = 0;
//       value.it_interval.tv_usec = 1000; // 1 millisecond
//       value.it_value.tv_sec = 0;
//       value.it_value.tv_usec = 1000;
//       setitimer(which, &value, &ovalue);
//       if (ovalue.it_interval.tv_sec != pvalue.it_interval.tv_sec
//           || ovalue.it_interval.tv_usec != pvalue.it_interval.tv_usec
//           || ovalue.it_value.tv_sec != pvalue.it_value.tv_sec
//           || ovalue.it_value.tv_usec != pvalue.it_value.tv_usec)
//         return signal_lisp_error( "Real time interval timer mismatch.");
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
#endif
//       printf("; Profiler started.\n");
//       fflush(stdout);
//       profiling = true;
//       profiled_thread = thread;
  }
  return T;
}

Value SYS_stop_profiler()
{
  if (profiling)
    {
      profiled_thread = NULL;
// #ifndef WIN32
//       struct itimerval value;
//       getitimer(ITIMER_PROF, &value);
//       value.it_interval.tv_sec = 0;
//       value.it_interval.tv_usec = 0;
//       value.it_value.tv_sec = 0;
//       value.it_value.tv_usec = 0;
//       setitimer(ITIMER_PROF, &value, &value);
// #endif
      profiling = false;
      printf("; Profiler stopped.\n");
    }
  else
    printf("; Profiler was not started.\n");
  fflush(stdout);
  return T;
}

Value SYS_profiler_sample_count()
{
    return make_unsigned_integer(profiler_sample_count);
}
