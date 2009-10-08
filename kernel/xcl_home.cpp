// xcl_home.cpp
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

#include <stdlib.h>

#include "lisp.hpp"
#include "xcl_home.hpp"

// no trailing '/'
const char * xcl_home()
{
  char * xcl_home = getenv("XCL_HOME");
  return xcl_home ? xcl_home : XCL_HOME;
}

Pathname * xcl_home_pathname()
{
    String * s = new String(xcl_home());
    const long len = s->length();
    if (len == 0 || !is_separator_char(s->char_at(len - 1)))
      s->append_char(SEPARATOR_CHAR);
    return new Pathname(s);
}
