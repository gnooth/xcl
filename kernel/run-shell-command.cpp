// run-shell-command.cpp
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
#include <stdlib.h>
#endif
#include "lisp.hpp"
#include "primitives.hpp"
#include "Pathname.hpp"

// ### current-directory
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

// ### chdir
Value EXT_chdir(Value arg)
{
  Pathname * new_dir = the_pathname(coerce_to_pathname(arg));
  SimpleString * namestring = new_dir->namestring();
#ifdef WIN32
  if (!SetCurrentDirectory(namestring->copy_to_c_string()))
#else
  if (chdir(namestring->copy_to_c_string()) != 0)
#endif
    {
      String * message = new String();
      Pathname * current_dir = the_pathname(EXT_current_directory());
      Pathname * merged_pathname = merge_pathnames(new_dir, current_dir, K_newest);
      if (EXT_probe_directory(make_value(merged_pathname)) == NIL)
        {
          if (CL_probe_file(make_value(merged_pathname)) != NIL)
            message->append("Not a directory.");
          else
            message->append("No such file or directory.");
        }
      else
        {
          message->append("Unable to change current directory to ");
          message->append(::prin1_to_string(make_value(merged_pathname)));
          message->append_char('.');
        }
      return signal_lisp_error(message);
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

// ### %run-shell-command
// run-shell-command is defined in run-shell-command.lisp
Value SYS_run_shell_command_internal(Value arg)
{
  AbstractString * command = check_string(arg);
  int ret = system(command->copy_to_c_string());
  return make_integer(ret == -1 ? ret : WEXITSTATUS(ret));
}
