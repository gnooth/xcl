// TypedObject.cpp
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

#include "lisp.hpp"

Value TypedObject::type_of() const
{
  return T;
}

Value TypedObject::class_of() const
{
  return C_t; // REVIEW
}

bool TypedObject::typep(Value type) const
{
  return (type == S_atom || type == T);
}

bool TypedObject::eql(Value value) const
{
  if (typed_object_p(value))
    return the_typed_object(value) == this;

  return false;
}

bool TypedObject::equal(Value value) const
{
  if (typed_object_p(value))
    return the_typed_object(value) == this;

  return false;
}

bool TypedObject::equalp(Value value) const
{
  return equal(value);
}

AbstractString * TypedObject::write_to_string()
{
  return unreadable_string();
}

AbstractString * TypedObject::princ_to_string()
{
  Thread * const thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_print_escape, NIL);
  thread->bind_special(S_print_readably, NIL);
  AbstractString * s = write_to_string();
  thread->set_last_special_binding(last_special_binding);
  return s;
}

AbstractString * TypedObject::prin1_to_string()
{
  Thread * const thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_print_escape, T);
  AbstractString * s = write_to_string();
  thread->set_last_special_binding(last_special_binding);
  return s;
}

AbstractString * TypedObject::unreadable_string()
{
  String * s = new String("#<");
  s->append(::prin1_to_string(type_of()));
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  s->append(buf);
  return s;
}

AbstractString * TypedObject::unreadable_string(AbstractString * string)
{
  String * s = new String("#<");
  s->append(string);
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  s->append(buf);
  return s;
}

int TypedObject::arity() const
{
  signal_type_error(make_value(this), S_function);
  // not reached
  return -1;
}

unsigned int TypedObject::minargs() const
{
  signal_type_error(make_value(this), S_function);
  // not reached
  return 0;
}

unsigned int TypedObject::maxargs() const
{
  signal_type_error(make_value(this), S_function);
  // not reached
  return 0;
}

Value TypedObject::parts()
{
  return current_thread()->set_values(NIL, NIL, NIL);
}
