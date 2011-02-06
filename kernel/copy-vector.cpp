// copy-vector.cpp
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

Value SYS_copy_vector(Value arg)
{
  AbstractVector* v1 = check_vector(arg);
  INDEX length = v1->length();
  AbstractVector* v2 =
    the_vector(the_symbol(S_make_array)->function()->execute(make_unsigned_fixnum(length),
                                                             K_element_type,
                                                             v1->element_type()));
  for (INDEX i = length; i-- > 0;)
    v2->aset(i, v1->aref(i));
  return make_value(v2);
}
