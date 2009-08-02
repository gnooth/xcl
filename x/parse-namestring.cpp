// parse-namestring.cpp
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

Value parse_namestring(AbstractString * namestring)
{
  // check for a logical pathname host
  AbstractString * h = get_host_string(namestring);
  if (h != NULL && LOGICAL_PATHNAME_TRANSLATION_TABLE->get(make_value(h)) != NULL_VALUE)
    {
      // a defined logical pathname host
      return make_value(new LogicalPathname(h, namestring->substring(namestring->index_of(':') + 1)));
    }
  return make_value(new Pathname(namestring));
}

Value parse_namestring(AbstractString * namestring, AbstractString * host)
{
  AbstractString * s = namestring;
  // look for a logical pathname host in the namestring
  AbstractString * h = get_host_string(namestring);
  if (h != NULL)
    {
      if (!h->equal(host))
        {
          String * message = new String("Host in ");
          message->append(namestring);
          message->append(" does not match requested host ");
          message->append(host);
          return signal_lisp_error(message);
        }
      // remove host prefix from namestring
      s = namestring->substring(namestring->index_of(':') + 1);
    }
  if (LOGICAL_PATHNAME_TRANSLATION_TABLE->get(make_value(host)) != NULL_VALUE)
    {
      // a defined logical pathname host
      return make_value(new LogicalPathname(host, s));
    }
  String * message = new String(host);
  message->append(" is not defined as a logical pathname host.");
  return signal_lisp_error(message);
}

// ### %parse-namestring string host default-pathname => pathname, position
Value SYS_parse_namestring_internal(Value arg1, Value arg2, Value arg3)
{
  Thread * const thread = current_thread();
  AbstractString * namestring = check_string(arg1);
  // The HOST parameter must be a string or NIL.
  if (arg2 == NIL)
    {
      // "If HOST is NIL, DEFAULT-PATHNAME is a logical pathname, and THING is
      // a syntactically valid logical pathname namestring without an explicit
      // host, then it is parsed as a logical pathname namestring on the host
      // that is the host component of DEFAULT-PATHNAME."
      arg3 = coerce_to_pathname(arg3);
      return thread->set_values(parse_namestring(namestring),
                                make_fixnum(namestring->length()));
    }
  AbstractString * host = check_string(arg2);
  return thread->set_values(parse_namestring(namestring, host),
                            make_fixnum(namestring->length()));
}
