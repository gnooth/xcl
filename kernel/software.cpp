// software.cpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

#ifndef WIN32
#include <sys/utsname.h>
#endif

// ### software-type
Value CL_software_type()
{
#ifdef WIN32
  return make_simple_string("Windows");
#else
  struct utsname buf;
  if(uname(&buf) == 0)
    return make_simple_string(buf.sysname);
  else
    return NIL;
#endif
}

// ### software-version
Value CL_software_version()
{
#ifdef WIN32
  OSVERSIONINFOEX osvi;
  ZeroMemory(&osvi, sizeof(OSVERSIONINFOEX));
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);
  if (GetVersionEx((OSVERSIONINFO *) &osvi))
    {
      DWORD major = osvi.dwMajorVersion;
      DWORD minor = osvi.dwMinorVersion;
      String * s = new String();
      s->append_long(major);
      s->append_char('.');
      s->append_long(minor);
      return make_value(s);
    }
  else
    return NIL;
#else
  struct utsname buf;
  if(uname(&buf) == 0)
    return make_simple_string(buf.release);
  else
    return NIL;
#endif
}
