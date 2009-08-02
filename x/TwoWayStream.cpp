// TwoWayStream.cpp
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
#include "TwoWayStream.hpp"

bool TwoWayStream::typep(Value type) const
{
  return (type == S_two_way_stream || type == S_stream || type == S_atom || type == T
          || type == C_two_way_stream || type == C_stream || type == C_t);
}

// ### make-two-way-stream input-stream output-stream => two-way-stream
Value CL_make_two_way_stream(Value arg1, Value arg2)
{
  Stream * in = check_stream(arg1);
  if (!in->is_input_stream())
    return signal_type_error(arg1, list2(S_satisfies, S_input_stream_p));
  Stream * out = check_stream(arg2);
  if (!out->is_output_stream())
    return signal_type_error(arg2, list2(S_satisfies, S_output_stream_p));
  return make_value(new TwoWayStream(in, out));
}

// ### two-way-stream-input-stream two-way-stream => input-stream
Value CL_two_way_stream_input_stream(Value arg)
{
  return make_value(check_two_way_stream(arg)->input_stream());
}

// ### two-way-stream-output-stream two-way-stream => output-stream
Value CL_two_way_stream_output_stream(Value arg)
{
  return make_value(check_two_way_stream(arg)->output_stream());
}
