// main.cpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

#include <signal.h>
#include <stdlib.h>     // exit()

#include "lisp.hpp"
#include "Frame.hpp"
#include "primitives.hpp"
#include "call_depth_limit.hpp"
#include "xcl_home.hpp"
#include "Pathname.hpp"
#include "Readtable.hpp"

#ifdef WIN32
BOOL WINAPI control_c_handler(DWORD event)
{
  if (event == CTRL_C_EVENT)
    {
      interrupted = 1;
      return TRUE;
    }

  return FALSE;
}
#endif

void initialize_control_c_handler()
{
#ifdef WIN32
  if (SetConsoleCtrlHandler(control_c_handler, TRUE))
    {
      printf("Control-C handler installed\n");
      fflush(stdout);
    }
#endif
}

extern Frame * primordial_frame;

static SimpleString * build_date()
{
  String * s = new String(xcl_home_pathname()->namestring());
  s->append("build");
  long fd = OPEN(s->as_c_string(), O_RDONLY);
  if (fd >= 0)
    {
      char buf[256];
      long n = READ(fd, buf, sizeof(buf));
      CLOSE(fd);
      if (n > 0)
        {
          for (unsigned long i = 0; i < sizeof(buf); i++)
            {
              if (buf[i] < ' ')
                {
                  buf[i] = 0;
                  return new_simple_string(buf);
                }
            }
        }
    }
  return NULL;
}

static void print_version()
{
  String * s = new String();
  s->append(check_string(CL_lisp_implementation_type()));
  s->append_char(' ');
  s->append(check_string(CL_lisp_implementation_version()));
#ifdef __x86_64__
  s->append(" (x86-64)");
#else
  s->append(" (x86)");
#endif
  SimpleString * built = build_date();
  if (built)
    {
      s->append(" built ");
      s->append(built);
    }
  s->append_char('\n');
  STANDARD_OUTPUT->write_string(s);
}

static void print_copyright()
{
  printf("Copyright (C) 2006-2011 Peter Graves\n");
}

static void process_command_line_arguments(int argc, char * argv[])
{
  Value list = NIL;
  for (int i = argc; i-- > 0;)
    {
      if (!strcmp(argv[i], "--version"))
        {
          print_version();
          exit(0);
        }
      if (!strcmp(argv[i], "--help"))
        {
          printf("Usage: xcl [options]\n");
          printf("Options:\n");
          printf("  --help                  Print this message and exit.\n");
          printf("  --version               Print version information and exit.\n");
          printf("  --load <filename>       Load the specified file.\n");
          printf("  --eval <expression>     Evaluate the specified expression.\n");
          exit(0);
        }
      list = make_cons(make_simple_string(argv[i]), list);
    }
  the_symbol(S_argv)->set_value(list);
}

void gc_warn_proc(char * msg, GC_word arg)
{
  printf(msg, (unsigned long) arg);
  fflush(stdout);
}

#ifndef WIN32
static void sigsegv_handler(int sig, siginfo_t *si, void * context)
{
  SYS_set_symbol_global_value(S_saved_stack, SYS_current_stack_as_list());
  SYS_set_symbol_global_value(S_saved_backtrace, current_thread()->backtrace_as_list(MOST_POSITIVE_FIXNUM));
  printf("SIGSEGV at address 0x%lx\n", (unsigned long) si->si_addr);
  ucontext_t * uc = (ucontext_t *) context;
#ifdef __x86_64__
  // Linux
  void * rip = (void *) uc->uc_mcontext.gregs[REG_RIP];
  printf("RIP = 0x%lx\n", (unsigned long) rip);
#else
  // x86
#ifdef __FreeBSD__
  void * eip = (void *) uc->uc_mcontext.mc_eip;
#else
  // Linux
  void * eip = (void *) uc->uc_mcontext.gregs[REG_EIP];
#endif
  printf("EIP = 0x%lx\n", (unsigned long) eip);
#endif
  siglongjmp(*primordial_frame->jmp(), 101);
}

static void sigint_handler(int sig, siginfo_t *si, void * context)
{
  SYS_set_symbol_global_value(S_saved_stack, SYS_current_stack_as_list());
  SYS_set_symbol_global_value(S_saved_backtrace, current_thread()->backtrace_as_list(MOST_POSITIVE_FIXNUM));
  if (CL_fboundp(S_break))
    current_thread()->execute(the_symbol(S_break)->function());
}
#endif

volatile bool boot_loaded_p = false;

int __main(int argc, char * argv[])
{
  initialize_lisp();
  process_command_line_arguments(argc, argv);
  print_version();
  print_copyright();
  initialize_control_c_handler();

#ifndef WIN32
  struct sigaction sa;
  sa.sa_flags = SA_SIGINFO;
  sigemptyset(&sa.sa_mask);
  sa.sa_sigaction = sigsegv_handler;
  if (sigaction(SIGSEGV, &sa, NULL) == -1)
    perror("sigaction");
  if (sigaction(SIGABRT, &sa, NULL) == -1)
    perror("sigaction2");
  sa.sa_sigaction = sigint_handler;
  if (sigaction(SIGINT, &sa, NULL) == -1)
    perror("sigaction3");
#endif

  primordial_frame = new Frame();
  Thread * const thread = current_thread();
  while (true)
    {
#ifdef WIN32
      SETJMP(*primordial_frame->jmp());
#else
      int ret = SETJMP(*primordial_frame->jmp());
#endif

      thread->set_stack(0);
      thread->set_call_depth(0);
      thread->set_last_control_frame(NULL);
      thread->set_last_tag(NULL);

      call_depth_limit = DEFAULT_CALL_DEPTH_LIMIT;

#ifndef WIN32
      if (ret == 101)
        {
          if (CL_fboundp(S_invoke_debugger_internal))
            current_thread()->execute(the_symbol(S_invoke_debugger_internal)->function(), make_value(new Error("SIGSEGV")));
        }
#endif
      if (!boot_loaded_p)
        {
          boot_loaded_p = true; // only try once!
          SYS_load_system_file(make_simple_string("lisp/boot.lisp"));
        }

      Value repl = thread->symbol_value(S_top_level_read_eval_print_loop);
      if (functionp(repl))
        thread->execute(the_function(repl));
      else
        {
          while (true)
            {
              AnsiStream * out = check_ansi_stream(thread->symbol_value(S_standard_output));
              out->write_string("* ");
              out->finish_output();
              Value obj = SYS_stream_read_internal(thread->symbol_value(S_standard_output), true, NIL, false);
              Value result = CL_eval(obj);
              assert(thread->stack() == 0);
              long values_length = thread->values_length();
              if (values_length < 0)
                {
                  // single value
                  out->terpri();
                  out->prin1(result);
                }
              else
                {
                  // multiple values
                  Value * values = thread->values();
                  for (long i = 0; i < values_length; i++)
                    {
                      out->terpri();
                      out->prin1(values[i]);
                    }
                }
              out->terpri();
            }
        }
    }
  return 0;
}

#ifdef WIN32
struct arginfo
{
  int argc;
  char * * argv;
};

DWORD WINAPI main_thread_proc(LPVOID p)
{
  arginfo * args = (arginfo *) p;
  return __main(args->argc, args->argv);
}

int _main(int argc, char * argv[])
{
  arginfo args;
  args.argc = argc;
  args.argv = argv;

  DWORD thread_id;

#ifndef STACK_SIZE_PARAM_IS_A_RESERVATION
#define STACK_SIZE_PARAM_IS_A_RESERVATION 0x00010000
#endif

  HANDLE h = GC_CreateThread(NULL,                              // default security descriptor
                             4194304,                           // stack size
                             main_thread_proc,
                             &args,
                             STACK_SIZE_PARAM_IS_A_RESERVATION, // creation flags
                             &thread_id);
  WaitForSingleObject(h, INFINITE);
  return 0;
}
#endif

int main(int argc, char * argv[])
{
  initialize_uptime();

  stack_top = current_sp();

  GC_init();
  GC_set_warn_proc(gc_warn_proc);

#ifdef WIN32
  return _main(argc, argv);
#else
  return __main(argc, argv);
#endif
}
