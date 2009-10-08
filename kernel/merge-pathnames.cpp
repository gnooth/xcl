// merge-pathnames.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "Pathname.hpp"
#include "LogicalPathname.hpp"

Value merge_directories(Value dir, Value default_dir)
{
  if (dir == NIL)
    return default_dir;
  if (car(dir) == K_relative && default_dir != NIL)
    {
      Value temp = NIL;
      while (default_dir != NIL)
        {
          temp = make_cons(car(default_dir), temp);
          default_dir = xcdr(default_dir);
        }
      dir = cdr(dir); // Skip :RELATIVE.
      while (dir != NIL)
        {
          temp = make_cons(car(dir), temp);
          dir = xcdr(dir);
        }
//       Value[] array = result.copyToArray();
//       for (long i = 0; i < array.length - 1; i++)
//         {
//           if (array[i] == Keyword.BACK)
//             {
//               if (array[i+1] instanceof AbstractString || array[i+1] == Keyword.WILD)
//                 {
//                   array[i] = null;
//                   array[i+1] = null;
//                 }
//             }
//         }
//       result = NIL;
//       for (long i = 0; i < array.length; i++)
//         {
//           if (array[i] != null)
//             result = new Cons(array[i], result);
//         }
      Value result = NIL;
      while (temp != NIL)
        {
          Value first = car(temp);
          if (first == K_back)
            {
              Value second = CL_cadr(temp);
              if (stringp(second) || second == K_wild)
                {
                  temp = CL_cddr(temp);
                  continue;
                }
            }
          result = make_cons(first, result);
          temp = xcdr(temp);
        }
      return result;
    }
  else
    return dir;
}

Pathname * merge_pathnames(Pathname * pathname, Pathname * default_pathname,
                           Value default_version)
{
  Value host;
  if (pathname->host() != NIL)
    host = pathname->host();
  else
    host = default_pathname->host();
  Value device;
  if (pathname->device() != NIL)
    device = pathname->device();
  else
    device = default_pathname->device();
  Value directory =
    merge_directories(pathname->directory(), default_pathname->directory());
  Value name;
  if (pathname->name() != NIL)
    name = pathname->name();
  else
    name = default_pathname->name();
  Value type;
  if (pathname->type() != NIL)
    type = pathname->type();
  else
    type = default_pathname->type();
  Value version;
  if (pathname->version() != NIL)
    version = pathname->version();
  else if (stringp(pathname->name()))
    version = default_version;
  else if (default_pathname->version() != NIL)
    version = default_pathname->version();
  else
    version = default_version;
  if (logical_pathname_p(pathname))
    return new LogicalPathname(host, device, directory, name, type, version);
  else
    return new Pathname(host, device, directory, name, type, version);
}

// ### merge-pathnames pathname &optional default-pathname default-version => merged-pathname
Value CL_merge_pathnames(unsigned int numargs, Value args[])
{
  Thread * const thread = current_thread();
  if (numargs < 1 || numargs > 3)
    return wrong_number_of_arguments(S_merge_pathnames, numargs, 1, 3);
  Pathname * pathname = the_pathname(coerce_to_pathname(args[0]));
  Pathname * default_pathname;
  if (numargs >= 2)
    default_pathname = the_pathname(coerce_to_pathname(args[1]));
  else
    {
      default_pathname =
        the_pathname(coerce_to_pathname(thread->symbol_value(S_default_pathname_defaults)));
    }
  Value default_version;
  if (numargs == 3)
    default_version = args[2];
  else
    default_version = K_newest;
  return make_value(merge_pathnames(pathname, default_pathname, default_version));
}
