// lisp-implementation.cpp
//
// Copyright (C) 2007-2010 Peter Graves <gnooth@gmail.com>
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
#include "xcl_home.hpp"

// ### lisp-implementation-type
Value CL_lisp_implementation_type()
{
  return make_simple_string("XCL");
}

// ### lisp-implementation-version
Value CL_lisp_implementation_version()
{
  String * s = new String(xcl_home_pathname()->namestring());
  s->append("version");
  long fd = OPEN(s->as_c_string(), O_RDONLY);
  if (fd >= 0)
    {
      char buf[256];
      long n = READ(fd, buf, sizeof(buf));
      CLOSE(fd);
      if (n > 0)
        {
          for (unsigned long i = 0; i < sizeof(buf); i++)
            {
              if (buf[i] < ' ')
                {
                  buf[i] = 0;
                  return make_simple_string(buf);
                }
            }
        }
    }
  return make_simple_string("0.0.0.291");
}
