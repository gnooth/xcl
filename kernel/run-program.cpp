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

// ### run-program
Value EXT_run_program(Value program, Value args)
{
#ifdef WIN32
  return signal_lisp_error("Not implemented.");
#else
  // REVIEW
  AbstractString * namestring = NULL;
  if (pathnamep(program))
    namestring = the_pathname(program)->namestring();
  else if (stringp(program))
    namestring = the_string(program);
  if (!namestring)
    return signal_lisp_error("No namestring.");
  check_list(args);
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
