// reader.cpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

#include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
#include "runtime.h"
#include "primitives.hpp"
#include "Complex.hpp"
#include "EndOfFile.hpp"
#include "Package.hpp"
#include "Pathname.hpp"
#include "ProgramError.hpp"
#include "ReaderError.hpp"
#include "Readtable.hpp"
#include "SimpleArray_T.hpp"
#include "SimpleArray_UB8_1.hpp"
#include "ZeroRankArray.hpp"
#include "keywordp.hpp"

Value stream_read(Value streamarg, bool eof_error_p,
                  Value eof_value, bool recursive_p,
                  Thread * thread, Readtable * rt)
{
  if (ansi_stream_p(streamarg))
    {
      Stream * stream = the_stream(streamarg);
      Value result = stream->read_preserving_whitespace(eof_error_p, eof_value, recursive_p, thread, rt);
      if (result != eof_value && !recursive_p)
        {
          int c = stream->read_char();
          if (c >= 0)
            {
              if (!rt->is_whitespace(c))
                stream->unread_char(c);
            }
        }
      if (thread->symbol_value(S_read_suppress) != NIL)
        return NIL;
      else
        return result;
    }
  else
    {
      // fundamental-stream
      assert(false);
      return NIL;
    }
}
