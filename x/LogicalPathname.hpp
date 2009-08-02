// LogicalPathname.hpp
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

#ifndef __LOGICAL_PATHNAME_HPP
#define __LOGICAL_PATHNAME_HPP

#include "HashTable.hpp"
#include "Pathname.hpp"

extern EqualHashTable * LOGICAL_PATHNAME_TRANSLATION_TABLE;

class LogicalPathname : public Pathname
{
public:
  LogicalPathname(Value host, Value device, Value directory,
                  Value name, Value type, Value version);

  LogicalPathname(AbstractString * host, AbstractString * rest);

  virtual Value type_of() const;
  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual AbstractString * write_to_string();

  virtual String * directory_namestring();
};

inline bool logical_pathname_p(Pathname * pathname)
{
  return (pathname->widetag() == WIDETAG_LOGICAL_PATHNAME);
}

inline bool logical_pathname_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_LOGICAL_PATHNAME);
}

#endif
