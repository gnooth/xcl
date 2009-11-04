// run-program.cpp
//
// Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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
#include <stdlib.h>
#include <sys/wait.h>
#endif
#include "lisp.hpp"
#include "primitives.hpp"
#include "Pathname.hpp"

void check_set_current_directory(const char * dir)
{
#ifdef WIN32
  if (!SetCurrentDirectory(dir))
    {
      String * message = new String("Unable to set current directory to ");
      message->append(dir);
      signal_lisp_error(message);
    }
#endif
}

// ### run-program
Value EXT_run_program(Value program, Value args)
{
  AbstractString * namestring = NULL;
  if (pathnamep(program))
    namestring = the_pathname(program)->namestring();
  else if (stringp(program))
    namestring = the_string(program);
  if (!namestring)
    return signal_lisp_error("No namestring.");
  check_list(args);
#ifdef WIN32
  String * command = new String(namestring);
  while (args != NIL)
    {
      command->append_char(' ');
      command->append(check_string(car(args)));
      args = cdr(args);
    }
  // "Multithreaded applications and shared library code should not use the
  // SetCurrentDirectory function and should avoid using relative path names.
  // The current directory state written by the SetCurrentDirectory function is
  // stored as a global variable in each process, therefore multithreaded
  // applications cannot reliably use this value without possible data
  // corruption from other threads that may also be reading or setting this
  // value. This limitation also applies to the GetCurrentDirectory and
  // GetFullPathName functions."
  char old_current_directory[MAX_PATH];
  GetCurrentDirectory(sizeof(old_current_directory), old_current_directory);
  Pathname * p =
    the_pathname(coerce_to_pathname(current_thread()->symbol_value(S_default_pathname_defaults)));
  check_set_current_directory(p->namestring()->copy_to_c_string());
  int ret = system(command->copy_to_c_string());
  check_set_current_directory(old_current_directory);
  return make_number(ret);
#else
  args = make_cons(make_value(namestring), args);
  INDEX len = ::length(args);
  char * * argv = (char * *) GC_malloc((len + 1) * sizeof(char *));
  for (INDEX i = 0; i < len; i++)
    {
      argv[i] = (char *) check_string(xcar(args))->copy_to_c_string();
      args = xcdr(args);
    }
  argv[len] = NULL;
  pid_t pid = fork();
  if (pid < 0)
    signal_lisp_error("fork() error");
  if (pid == 0)
    {
      // child process
      execvp(namestring->copy_to_c_string(), argv);
      exit(EXIT_FAILURE);
    }
  else
    {
      // parent process
      int status;
      waitpid(pid, &status, 0);
      return make_fixnum(status);
    }
#endif
}
