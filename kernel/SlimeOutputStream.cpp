// SlimeOutputStream.cpp
//
// Copyright (C) 2009-2010 Peter Graves <gnooth@gmail.com>
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
#include "SlimeOutputStream.hpp"

bool SlimeOutputStream::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_slime_output_stream || type == S_ansi_stream || type == S_stream || type == S_atom || type == T);
  else
    return (type == C_slime_output_stream || type == C_ansi_stream || type == C_stream || type == C_t);
}

// ### make-slime-output-stream function => slime-output-stream
Value EXT_make_slime_output_stream(Value arg)
{
  TypedObject * function;
  if (symbolp(arg))
    function = check_function(CL_symbol_function(arg));
  else
    function = check_function(arg);
  return make_value(new SlimeOutputStream(function));
}
