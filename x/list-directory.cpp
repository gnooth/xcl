// list-directory.cpp
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

#include <errno.h>
#include <dirent.h>

#include "lisp.hpp"
#include "primitives.hpp"
#include "Pathname.hpp"

// ### list-directory
Value SYS_list_directory(Value arg)
{
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  Pathname * pathname = the_pathname(arg);

#ifdef WIN32
  String * namestring = new String();
  Value device = pathname->device();
  if (stringp(device))
    {
      namestring->append(the_string(device));
      namestring->append_char(':');
    }
  namestring->append(pathname->directory_namestring());
#else
  String * namestring = pathname->directory_namestring();
#endif

  // Make sure namestring has a separator char at the end.
  unsigned long len = namestring->length();
  if (len)
    {
      if (namestring->fast_char_at(len - 1) != SEPARATOR_CHAR)
        namestring->append_char(SEPARATOR_CHAR);
    }

  DIR * dir = opendir(namestring->as_c_string());
  if (!dir)
    return signal_lisp_error("Can't open directory.");
  errno = 0;
  Value result = NIL;
  struct dirent * ent;
  while ((ent = readdir(dir)))
    {
      if (!strcmp(ent->d_name, ".") || !strcmp(ent->d_name, ".."))
        continue;
      String * s = new String(namestring);
      s->append(ent->d_name);
#ifdef _DIRENT_HAVE_D_TYPE
      bool is_directory = (ent->d_type == DT_DIR);
      if (is_directory)
        s->append_char(SEPARATOR_CHAR);
#else
      // FIXME
#endif
      result = make_cons(make_value(new Pathname(s)), result);
    }

  if (errno)
    {
#ifdef WIN32
      DWORD error = GetLastError();
      if (error != ERROR_SUCCESS && error != ERROR_NO_MORE_FILES)
        {
          closedir(dir);
          String * message = new String("Error ");
          message->append_long(error);
          message->append(" reading directory.");
          return signal_lisp_error(message);
        }
#else
      closedir(dir);
      String * message = new String("Error ");
      message->append_long(errno);
      message->append(" reading directory.");
      return signal_lisp_error(message);
#endif
    }

  closedir(dir);
  return CL_nreverse(result);
}
