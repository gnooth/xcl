// probe-file.cpp
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

#if defined(__FreeBSD__)
#include <limits.h>     // PATH_MAX
#endif

#if !defined(WIN32)
#include <stdlib.h>     // canonicalize_file_name(), realpath()
#endif

#include <time.h>

#include "lisp.hpp"
#include "primitives.hpp"
#include "FileError.hpp"
#include "Pathname.hpp"
#include "LogicalPathname.hpp"

static Value truename(Value arg, bool require_directory_p, bool errorp)
{
  Thread * thread = current_thread();
  Pathname * default_pathname =
    the_pathname(coerce_to_pathname(thread->symbol_value(S_default_pathname_defaults)));
  Value default_version = K_newest;
  Pathname * pathname = the_pathname(coerce_to_pathname(arg));
  if (logical_pathname_p(pathname))
    pathname = the_pathname(the_symbol(S_translate_logical_pathname)->function()->execute(make_value(pathname)));
  pathname = merge_pathnames(pathname, default_pathname, default_version);
  if (pathname->is_wild())
    return signal_lisp_error(new FileError("Bad place for a wild pathname.",
                                           make_value(pathname)));
  AbstractString * namestring = pathname->namestring();
  if (namestring && namestring->length() > 0)
    {
#if defined(WIN32)
//       if (namestring->length() > 1)
//         {
//           char c = namestring->fast_char_at(namestring->length() - 1);
//           if (c == SEPARATOR_CHAR)
//             namestring = namestring->substring(0, namestring->length() - 1);
//         }
//       struct _stat statbuf;
//       if (_stat(namestring->as_c_string(), &statbuf) == 0)
//         {
//           if (S_ISDIR(statbuf.st_mode))
//             {
//               if (namestring->fast_char_at(namestring->length() - 1) != SEPARATOR_CHAR)
//                 {
//                   String * s = new String(namestring);
//                   s->append_char(SEPARATOR_CHAR);
//                   return make_value(new Pathname(s));
//                 }
//               return make_value(new Pathname(namestring));
//             }
//           if (!require_directory_p)
//             return make_value(new Pathname(namestring));
//         }
      if (equal(pathname->directory(), list1(K_absolute)) && pathname->name() == NIL)
        {
          if (pathname->device() == NIL) // current drive
            return make_value(new Pathname(namestring));
          // make sure drive is valid
          char drive[3] = " :";
          drive[0] = xchar(CL_character(pathname->device()));
          char buf[MAX_PATH];
          if (QueryDosDevice(drive, buf, sizeof(buf)))
            return make_value(new Pathname(namestring));
          // not valid, fall through...
        }
      else
        {
          if (namestring->length() > 1)
            {
              char c = namestring->fast_char_at(namestring->length() - 1);
              if (c == SEPARATOR_CHAR)
                namestring = namestring->substring(0, namestring->length() - 1);
            }
          WIN32_FIND_DATA find_data;
          HANDLE h = FindFirstFile(namestring->as_c_string(), &find_data);
          if (h != INVALID_HANDLE_VALUE)
            {
              FindClose(h);
              if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY)
                {
                  if (namestring->fast_char_at(namestring->length() - 1) != SEPARATOR_CHAR)
                    {
                      String * s = new String(namestring);
                      s->append_char(SEPARATOR_CHAR);
                      return make_value(new Pathname(s));
                    }
                  return make_value(new Pathname(namestring));
                }
              if (!require_directory_p)
                return make_value(new Pathname(namestring));
            }
        }
#else
#if defined(__FreeBSD__)
      // REVIEW
      char * buffer = (char *) malloc(PATH_MAX);
      memset(buffer, 0, PATH_MAX);
      char * canonical_path = realpath(namestring->as_c_string(), buffer);
      if (!canonical_path)
        free(buffer);
#else
      // Linux
      // canonicalize_file_name() is a GNU extension
      char * canonical_path = canonicalize_file_name(namestring->as_c_string());
#endif
      if (canonical_path)
        {
          struct stat statbuf;
          // stat() follows symlinks; lstat() does not
          if (stat(canonical_path, &statbuf) == 0)
            {
              String * s = new String(canonical_path);
              free(canonical_path);
              canonical_path = NULL;
              bool is_directory = S_ISDIR(statbuf.st_mode);
              if (is_directory)
                {
                  if (s->char_at(s->length() - 1) != SEPARATOR_CHAR)
                    s->append_char(SEPARATOR_CHAR);
                }
              if (is_directory || !require_directory_p)
                return make_value(new Pathname(s));
            }
          else
            {
              free(canonical_path);
              canonical_path = NULL;
            }
        }
#endif
    }
  // not found
  if (errorp)
    {
      String * s = new String("The file ");
      s->append(pathname->prin1_to_string());
      s->append(" does not exist.");
      return signal_lisp_error(new FileError(s, make_value(pathname)));
    }
  else
    return NIL;
}

// ### truename filespec => truename
Value CL_truename(Value arg)
{
  // "An error of type FILE-ERROR is signaled if an appropriate file cannot be
  // located within the file system for the given filespec, or if the file
  // system cannot perform the requested operation."
  return truename(arg, false, true);
}

// ### probe-file pathspec => truename
Value CL_probe_file(Value arg)
{
  return truename(arg, false, false);
}

// ### probe-directory
Value EXT_probe_directory(Value arg)
{
  return truename(arg, true, false);
}

// ### file-directory-p
Value EXT_file_directory_p(Value arg)
{
  Pathname * pathname = the_pathname(coerce_to_pathname(arg));
  AbstractString * namestring = pathname->namestring();
  struct stat statbuf;
  // stat() follows symlinks; lstat() does not
  return (stat(namestring->as_c_string(), &statbuf) == 0
          && S_ISDIR(statbuf.st_mode)) ? T : NIL;
}

// ### file-write-date
Value CL_file_write_date(Value arg)
{
  Pathname * default_pathname =
    the_pathname(coerce_to_pathname(current_thread()->symbol_value(S_default_pathname_defaults)));
  Value default_version = K_newest;
  Pathname * pathname = merge_pathnames(the_pathname(coerce_to_pathname(arg)),
                                        default_pathname, default_version);
  if (pathname->is_wild())
    return signal_lisp_error(new FileError("Bad place for a wild pathname.",
                                           make_value(pathname)));
  AbstractString * namestring = pathname->namestring();
  struct stat statbuf;
  // stat() follows symlinks; lstat() does not
  if (stat(namestring->as_c_string(), &statbuf) == 0)
    {
      mpz_t z;
      mpz_init_set_ui(z, statbuf.st_mtime);
      mpz_add_ui(z, z, 2208988800UL);
      Value value = normalize(z);
      MPZ_CLEAR(z);
      return value;
    }
  else
    return NIL;
}

// ### get-universal-time
Value CL_get_universal_time()
{
  mpz_t z;
  mpz_init_set_ui(z, time(NULL));
  mpz_add_ui(z, z, 2208988800UL);
  Value value = normalize(z);
  MPZ_CLEAR(z);
  return value;
}

// ### rename-file filespec new-name => defaulted-new-name, old-truename, new-truename
Value CL_rename_file(Value arg1, Value arg2)
{
  Pathname * original_pathname = the_pathname(truename(arg1, false, true));
//   final String originalNamestring = original.getNamestring();
  SimpleString * original_namestring = original_pathname->namestring();

//   Pathname newName = coerceToPathname(second);
  Pathname * new_pathname = the_pathname(coerce_to_pathname(arg2));
  if (new_pathname->is_wild())
//     signal(new FileError("Bad place for a wild pathname.", newName));
    return signal_lisp_error(new FileError("Bad place for a wild pathname.",
                                           make_value(new_pathname)));
  new_pathname = merge_pathnames(new_pathname, original_pathname, NIL);

//   final String newNamestring;
//   if (newName instanceof LogicalPathname)
//     newNamestring = LogicalPathname.translateLogicalPathname((LogicalPathname)newName).getNamestring();
//   else
//     newNamestring = newName.getNamestring();
  SimpleString * new_namestring = new_pathname->namestring();

  if (original_namestring && new_namestring)
    {
//       final File source = new File(originalNamestring);
//       final File destination = new File(newNamestring);
//       if (Utilities.isPlatformWindows)
//         {
//           if (destination.isFile())
//             destination.delete();
//         }
//       if (source.renameTo(destination))
//         // Success!
#ifdef WIN32
      struct _stat statbuf;
      if (_stat(new_namestring->as_c_string(), &statbuf) == 0)
        {
          if (S_ISREG(statbuf.st_mode))
            {
              if (unlink(new_namestring->as_c_string()) != 0)
                {
                  String * s = new String("Error ");
                  s->append_long(GetLastError());
                  s->append(" deleting ");
                  s->append(new_namestring);
                  s->append_char('.');
                  return signal_lisp_error(s);
                }
            }
        }
#endif
      if (rename(original_namestring->as_c_string(), new_namestring->as_c_string()) == 0)
        {
          Value value1 = make_value(new_pathname);
          Value value2 = make_value(original_pathname);
          Value value3 = truename(value1, false, true);
          return current_thread()->set_values(value1, value2, value3);
        }
#ifdef WIN32
      else
        {
          String * s = new String("Error ");
          s->append_long(GetLastError());
          s->append(" renaming ");
          s->append(original_namestring);
          s->append_char('.');
          return signal_lisp_error(s);
        }
#endif
    }
  String * s = new String("Unable to rename ");
  s->append(original_pathname->prin1_to_string());
  s->append(" to ");
  s->append(new_pathname->prin1_to_string());
  s->append_char('.');
  return signal_lisp_error(new FileError(s, make_value(original_pathname))); // REVIEW
}
