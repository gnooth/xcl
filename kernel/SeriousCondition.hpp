// SeriousCondition.hpp
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

#ifndef __SERIOUS_CONDITION_HPP
#define __SERIOUS_CONDITION_HPP

#include "Condition.hpp"

class SeriousCondition : public Condition
{
public:
  SeriousCondition()
    : Condition()
  {
  }
  
  SeriousCondition(AbstractString * s)
    : Condition(s)
  {
  }
  
  SeriousCondition(const char * s)
    : Condition(s)
  {
  }
  
  virtual Value type_of() const
  {
    return S_serious_condition;
  }

  virtual Value class_of() const
  {
    return C_serious_condition;
  }
  
  virtual bool typep(Value type) const;
};

#endif
