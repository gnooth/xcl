// Autoload.hpp
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

#ifndef __AUTOLOAD_HPP
#define __AUTOLOAD_HPP

class String;

class Autoload : public Function
{
private:
  AbstractString * const _filename;

public:
  Autoload(Value name)
    : Function(WIDETAG_AUTOLOAD, name), _filename(NULL)
  {
  }

  Autoload(Value name, AbstractString * filename)
    : Function(WIDETAG_AUTOLOAD, name), _filename(filename)
  {
  }


  virtual ~Autoload() {}

  void load();

  virtual Value execute();
  virtual Value execute(Value arg);
  virtual Value execute(Value arg1, Value arg2);
  virtual Value execute(Value arg1, Value arg2, Value arg3);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);
  virtual Value execute(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6);
  virtual Value execute(unsigned int numargs, Value args[]);

  virtual AbstractString * prin1_to_string();
};

inline bool autoloadp(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_AUTOLOAD);
}

inline Autoload * the_autoload(Value value)
{
  assert(autoloadp(value));
  return reinterpret_cast<Autoload *>(value - LOWTAG_TYPED_OBJECT);
}

#endif
