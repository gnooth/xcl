// source-information.cpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

// ### record-source-information
Value SYS_record_source_information(Value name, Value source_position)
{
  if (non_nil_symbol_p(name)) // FIXME support setf functions too
    {
      Thread * thread = current_thread();
      Value source = thread->symbol_value(S_source_file);
      the_non_nil_symbol(name)->put(S_source_internal,
                                    source != NIL ? make_cons(source, source_position) : NIL);
    }
  return T;
}

// ### source
Value SYS_source(Value arg)
{
  return SYS_get3(arg, S_source_internal, NIL);
}

// ### set-source
Value SYS_set_source(Value arg1, Value arg2)
{
  check_symbol(arg1)->put(S_source_internal, arg2);
  return arg2;
}

// ### source-pathname
Value SYS_source_pathname(Value arg)
{
  Value source = SYS_get3(arg, S_source_internal, NIL);
  return consp(source) ? xcar(source) : source;
}

// ### source-file-position
Value SYS_source_file_position(Value arg)
{
  Value source = SYS_get3(arg, S_source_internal, NIL);
  return consp(source) ? xcdr(source) : NIL;
}
