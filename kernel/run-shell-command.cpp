// run-shell-command.cpp
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
#include <stdlib.h>
#endif
#include "lisp.hpp"
#include "primitives.hpp"
#include "Pathname.hpp"

void check_set_current_directory(const char * dir)
{
#ifdef WIN32
  if (!SetCurrentDirectory(dir))
#else
  if (chdir(dir) != 0)
#endif
    {
      String * message = new String("Unable to set current directory to ");
      message->append(dir);
      signal_lisp_error(message);
    }
}

// ### run-program
Value EXT_run_shell_command(Value arg)
{
  AbstractString * command = check_string(arg);
  char old_current_directory[1024];
#ifdef WIN32
  // "Multithreaded applications and shared library code should not use the
  // SetCurrentDirectory function and should avoid using relative path names.
  // The current directory state written by the SetCurrentDirectory function is
  // stored as a global variable in each process, therefore multithreaded
  // applications cannot reliably use this value without possible data
  // corruption from other threads that may also be reading or setting this
  // value. This limitation also applies to the GetCurrentDirectory and
  // GetFullPathName functions."
  GetCurrentDirectory(sizeof(old_current_directory), old_current_directory);
#else
  if (!getcwd(old_current_directory, sizeof(old_current_directory)))
    signal_lisp_error("Unable to determine current directory");
#endif
  Pathname * p =
    the_pathname(coerce_to_pathname(current_thread()->symbol_value(S_default_pathname_defaults)));
  check_set_current_directory(p->namestring()->copy_to_c_string());
  int ret = system(command->copy_to_c_string());
  check_set_current_directory(old_current_directory);
  return make_number(ret);
}

Value EXT_current_directory()
{
  char old_current_directory[1024];
#ifdef WIN32
  GetCurrentDirectory(sizeof(old_current_directory), old_current_directory);
#else
  if (!getcwd(old_current_directory, sizeof(old_current_directory)))
    signal_lisp_error("Unable to determine current directory");
#endif
  String * s = new String(old_current_directory);
  INDEX len = s->length();
  if (len > 0 && s->char_at(len - 1) != SEPARATOR_CHAR)
    s->append_char(SEPARATOR_CHAR);
  return parse_namestring(s);
}

Value EXT_chdir(Value arg)
{
//   AbstractString * dir = check_string(dir);
  Pathname * p = the_pathname(coerce_to_pathname(arg));
  SimpleString * namestring = p->namestring();
#ifdef WIN32
  if (!SetCurrentDirectory(namestring->copy_to_c_string()))
#else
  if (chdir(namestring->copy_to_c_string()) != 0)
#endif
    {
      String * message = new String("Unable to set current directory to ");
      message->append(namestring);
      signal_lisp_error(message);
    }
  char new_current_directory[1024];
#ifdef WIN32
  GetCurrentDirectory(sizeof(new_current_directory), new_current_directory);
#else
  if (!getcwd(new_current_directory, sizeof(new_current_directory)))
    signal_lisp_error("Unable to determine current directory");
#endif
  String * s = new String(new_current_directory);
  INDEX len = s->length();
  if (len > 0 && s->char_at(len - 1) != SEPARATOR_CHAR)
    s->append_char(SEPARATOR_CHAR);
  return parse_namestring(s);
}
