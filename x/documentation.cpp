// documentation.cpp
//
// Copyright (C) 2009 Peter Graves <peter@armedbear.org>
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
#include "documentation.hpp"

EqHashTable * DOCUMENTATION_HASH_TABLE;

// ### %documentation object doctype => documentation
Value SYS_documentation_internal(Value object, Value doctype)
{
  Value alist = DOCUMENTATION_HASH_TABLE->get(object);
  if (alist != NULL_VALUE)
    {
      Value entry = EXT_assq(doctype, alist);
      if (consp(entry))
        return the_cons(entry)->xcdr();
    }
  return NIL;
}

// ### %set-documentation object doctype new-value => new-value
Value SYS_set_documentation_internal(Value object, Value doctype, Value new_value)
{
  Value alist = DOCUMENTATION_HASH_TABLE->get(object);
  Value entry;
  if (alist == NULL_VALUE)
    {
      // no alist
      entry = make_cons(doctype, new_value);
      alist = make_cons(entry, NIL);
      DOCUMENTATION_HASH_TABLE->put(object, alist);
    }
  else
    {
      entry = EXT_assq(doctype, alist);
      if (consp(entry))
        the_cons(entry)->setcdr(new_value);
      else
        {
          // no entry
          entry = make_cons(doctype, new_value);
          alist = make_cons(entry, alist);
          DOCUMENTATION_HASH_TABLE->put(object, alist);
        }
    }
  return new_value;
}
