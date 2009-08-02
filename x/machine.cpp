// machine.cpp
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

// ### machine-type
Value CL_machine_type()
{
#ifdef __x86_64__
  return make_simple_string("X86-64");
#else
  return make_simple_string("X86");
#endif
}

// ### machine_version
Value CL_machine_version()
{
  String * s = new String();
  for (unsigned int i = 0x80000002; i <= 0x80000004; i++)
    {
      unsigned int eax, ebx, ecx, edx;
      asm("cpuid": "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx) : "a" (i));
      char buf[17];
      buf[16] = '\0';
      for(int j = 0; j < 4; j++)
        {
          int shift = j * 8;
          buf[j] = eax >> shift;
          buf[j + 4] = ebx >> shift;
          buf[j + 8] = ecx >> shift;
          buf[j + 12] = edx >> shift;
        }
      s->append(buf);
    }
  return make_value(s);
}

// ### machine-instance
Value CL_machine_instance()
{
#ifdef WIN32
  char buf[MAX_COMPUTERNAME_LENGTH + 1];
  DWORD size = sizeof(buf);
  if (GetComputerName(buf, &size))
    return make_simple_string(buf);
  else
    return NIL;
#else
  char buf[256];
  memset(buf, 0, sizeof(buf));
  if (gethostname(buf, sizeof(buf)) == 0)
    return make_simple_string(buf);
  else
    return NIL;
#endif
}

// ### short-site-name
Value CL_short_site_name()
{
  return CL_machine_instance();
}

// ### long-site-name
Value CL_long_site_name()
{
  return CL_machine_instance();
}
