// delete-file.cpp
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
#include <errno.h>
#endif

#include "lisp.hpp"
#include "primitives.hpp"
#include "FileError.hpp"
#include "Pathname.hpp"
#include "LogicalPathname.hpp"

// ### delete-file filespec => t
Value CL_delete_file(Value arg)
{
  // Don't follow symlinks! We want to delete the symlink itself, not the
  // linked-to file.
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  Pathname * pathname = the_pathname(arg);
//   if (arg instanceof Stream)
//     ((Stream)arg)._close();
//   if (pathname instanceof LogicalPathname)
//     pathname = LogicalPathname.translateLogicalPathname((LogicalPathname)pathname);
  if (logical_pathname_p(pathname))
    pathname = the_pathname(the_symbol(S_translate_logical_pathname)->function()->execute(make_value(pathname)));

  if (pathname->is_wild())
    return signal_lisp_error(new FileError("Bad place for a wild pathname.", arg));

//   final Pathname defaultedPathname =
//     Pathname.mergePathnames(pathname,
//                             coerceToPathname(Symbol.DEFAULT_PATHNAME_DEFAULTS.symbolValue()),
//                             NIL);
  Pathname * default_pathname =
    the_pathname(coerce_to_pathname(current_thread()->symbol_value(S_default_pathname_defaults)));
  Pathname * defaulted_pathname = merge_pathnames(pathname, default_pathname, NIL);

  AbstractString * namestring = defaulted_pathname->namestring();
  if (!namestring)
    {
      String * s = new String("Pathname has no namestring: ");
      s->append(defaulted_pathname->prin1_to_string());
      return signal_lisp_error(new FileError(s, make_value(defaulted_pathname)));
    }

//   final File file = new File(namestring);
//   if (file.exists()) {
//     // File exists.
//     for (long i = 0; i < 5; i++) {
//       if (file.delete())
//         return T;
//       System.gc();
//       Thread.yield();
//     }
//     Pathname truename = new Pathname(file.getAbsolutePath());
//     FastStringBuffer sb = new FastStringBuffer("Unable to delete ");
//     sb.append(file.isDirectory() ? "directory " : "file ");
//     sb.append(truename.writeToString());
//     sb.append('.');
//     return signal(new FileError(sb.toString(), truename));
//   } else {
//     // File does not exist.
//     return T;
//   }
#ifdef WIN32
  struct _stat statbuf;
  if (_stat(namestring->as_c_string(), &statbuf) == 0)
    {
//       bool is_directory = S_ISDIR(statbuf.st_mode);
//       if (is_directory || !require_directory_p)
//         return make_value(pathname);
      if (S_ISREG(statbuf.st_mode) && unlink(namestring->as_c_string()) == 0)
        return T;
    }
#else
  struct stat statbuf;
  // stat() follows symlinks; lstat() does not
  if (lstat(namestring->as_c_string(), &statbuf) == 0)
    {
      if (S_ISREG(statbuf.st_mode) && unlink(namestring->as_c_string()) == 0)
        return T;
    }
#endif
  else
    {
      String * s = new String("The file ");
      s->append(defaulted_pathname->prin1_to_string());
      s->append(" does not exist.");
      return signal_lisp_error(new FileError(s, make_value(defaulted_pathname)));
    }
  // "If the deletion operation is not successful, an error of type FILE-ERROR
  // is signaled."
  String * s = new String("Unable to delete ");
  s->append(defaulted_pathname->prin1_to_string());
  s->append(" (error ");
#ifdef WIN32
  s->append_long(GetLastError());
#else
  s->append_long(errno);
#endif
  s->append(").");
  return signal_lisp_error(new FileError(s, make_value(defaulted_pathname)));
}
