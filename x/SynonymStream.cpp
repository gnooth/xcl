// SynonymStream.cpp
//
// Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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
#include "SynonymStream.hpp"

bool SynonymStream::typep(Value type) const
{
  return (type == S_synonym_stream || type == S_stream || type == S_atom || type == T
          || type == C_synonym_stream || type == C_stream || type == C_t);
}

// ### make-synonym-stream symbol => synonym-stream
Value CL_make_synonym_stream(Value arg)
{
  if (!symbolp(arg))
    return signal_type_error(arg, S_symbol);
  return make_value(new SynonymStream(arg));
}

// ### synonym-stream-symbol synonym-stream => symbol
Value CL_synonym_stream_symbol(Value arg)
{
  return check_synonym_stream(arg)->symbol();
}
