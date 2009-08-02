// Binding.hpp
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

#ifndef __BINDING_HPP
#define __BINDING_HPP

class Binding : public gc
{
private:
  Value _name;
  Value _value;
  bool _specialp;
  Binding * _next;

public:
  Binding(Value name, Value value, Binding * next)
    : _name(name), _value(value), _specialp(false), _next(next)
  {}

  Binding(Value name, Value value, bool specialp, Binding * next)
    : _name(name), _value(value), _specialp(specialp), _next(next)
  {}

  Value name() const
  {
    return _name; 
  }
  
  Value value() const
  {
    return _value; 
  }
  
  bool specialp() const
  {
    return _specialp;
  }
  
  Binding * next() const
  {
    return _next;
  }

  void set_value(Value value)
  {
    _value = value;
  }
};

#endif
