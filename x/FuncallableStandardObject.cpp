// FuncallableStandardObject.cpp
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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
#include "runtime.h"

void FuncallableStandardObject::autocompile()
{
  if (!the_symbol(S_compile)->is_autoload())
    {
      Thread * thread = current_thread();
      Value compiled_function = RT_thread_call_symbol_2(thread, S_compile, NIL,
                                                        make_value(_function));
      thread->clear_values();
      if (functionp(compiled_function))
        _function = the_typed_object(compiled_function);
      _autocompilep = false;
    }
}

// ### funcallable-instance-function
// not in MOP
Value SYS_funcallable_instance_function(Value arg)
{
  return make_value(check_funcallable_instance(arg)->funcallable_instance_function());
}

// ### set-funcallable-instance-function funcallable-instance function => unspecified
// MOP p. 230
Value MOP_set_funcallable_instance_function(Value arg1, Value arg2)
{
  FuncallableStandardObject * instance = check_funcallable_instance(arg1);
  TypedObject * function = check_typed_object(arg2);
  instance->set_funcallable_instance_function(function);
  return T;
}
