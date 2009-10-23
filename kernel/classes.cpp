// classes.cpp
//
// Copyright (C) 2006-2008 Peter Graves <peter@armedbear.org>
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
#include "HashTable.hpp"
#include "primitives.hpp"

bool LispClass::subclassp(Value cls) const
{
  Value cpl = this->cpl();
  while (cpl != NIL)
    {
      if (car(cpl) == cls)
        return true;
      cpl = xcdr(cpl);
    }
  return false;
}

AbstractString * LispClass::write_to_string()
{
  String * s = new String("#<");
  s->append(::write_to_string(type_of()));
  s->append_char(' ');
  s->append(the_symbol(name())->write_to_string());
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  s->append(buf);
  return s;
}

// ### %class-name
Value SYS_class_name_internal(Value arg)
{
  return the_class(arg)->name();
}

// ### subclassp
Value EXT_subclassp(Value arg1, Value arg2)
{
  return check_class(arg1)->subclassp(arg2) ? T : NIL;
}

bool StandardClass::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_standard_class || type == S_class
            || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_standard_class || type == C_class
            || type == C_standard_object || type == C_t);
}

bool FuncallableStandardClass::typep(Value type) const
{
  if (symbolp(type))
    return (type == S_funcallable_standard_class || type == S_class
            || type == S_standard_object || type == S_atom || type == T);
  else
    return (type == C_funcallable_standard_class || type == C_class
            || type == C_standard_object || type == C_t);
}

Value FuncallableStandardClass::class_of() const
{
  return C_funcallable_standard_class;
}

static EqHashTable * map;

inline Value add_built_in_class(Value name)
{
  Value value = make_value(new BuiltInClass(name));
  map->put(name, value);
  return value;
}

inline Value add_structure_class(Value name)
{
  Value value = make_value(new StructureClass(name));
  map->put(name, value);
  return value;
}

inline Value add_standard_class(Value name)
{
  Value value = make_value(new StandardClass(name));
  map->put(name, value);
  return value;
}

inline Value add_funcallable_standard_class(Value name)
{
  Value value = make_value(new FuncallableStandardClass(name));
  map->put(name, value);
  return value;
}

inline Value add_condition_class(Value name)
{
  Value value = make_value(new StandardClass(name));
  map->put(name, value);
  return value;
}

void initialize_classes()
{
  map = new EqHashTable();

#define DEFINE_BUILT_IN_CLASS(name) \
  C_ ## name = add_built_in_class(S_ ## name)
#define DEFINE_STRUCTURE_CLASS(name) \
  C_ ## name = add_structure_class(S_ ## name)
#define DEFINE_STANDARD_CLASS(name) \
  C_ ## name = add_standard_class(S_ ## name)
#define DEFINE_FUNCALLABLE_STANDARD_CLASS(name) \
  C_ ## name = add_funcallable_standard_class(S_ ## name)
#define DEFINE_CONDITION_CLASS(name) \
  C_ ## name = add_condition_class(S_ ## name)
#include "classdefs.hpp"
#undef DEFINE_BUILT_IN_CLASS
#undef DEFINE_STRUCTURE_CLASS
#undef DEFINE_STANDARD_CLASS
#undef DEFINE_FUNCALLABLE_STANDARD_CLASS
#undef DEFINE_CONDITION_CLASS
}

Value add_class(Value name, Value value)
{
  map->put(name, value);
  return value;
}

Value find_class(Value name)
{
  return map->get(name);
}

// ### class-of object => class
Value CL_class_of(Value obj)
{
  long tag = lowtag_of(obj);
  switch (tag)
    {
    case LOWTAG_EVEN_FIXNUM:
    case LOWTAG_ODD_FIXNUM:
      return C_integer;
    case LOWTAG_LIST:
      return (obj == NIL) ? C_null : C_cons;
    case LOWTAG_TYPED_OBJECT:
      return the_typed_object(obj)->class_of();
    case LOWTAG_CHARACTER:
      return C_character;
    case LOWTAG_SYMBOL:
      return C_symbol;
    default:
      {
        String * message = new String();
        message->append(the_symbol(S_class_of)->prin1_to_string());
        message->append(": unknown lowtag ");
        message->append_long(tag);
        return signal_lisp_error(message);
      }
    }
}

static Value find_class_not_symbol(Value arg)
{
  String * message = new String(::prin1_to_string(arg));
  message->append(" is not a legal class name.");
  return signal_lisp_error(message);
}

static Value find_class_not_found(Value arg)
{
  String * message = new String("There is no class named ");
  message->append(::prin1_to_string(arg));
  message->append_char('.');
  return signal_lisp_error(message);
}

// ### find-class symbol &optional errorp environment => class
// REVIEW environment argument is ignored
Value CL_find_class(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 3)
    return wrong_number_of_arguments(S_find_class, numargs, 1, 3);
  Value name = args[0];
  if (!symbolp(name))
    return find_class_not_symbol(name);
  Value value = map->get(name);
  if (value != NULL_VALUE)
    return value;
  if (numargs < 2 || args[1] != NIL)
//     {
//       String * message = new String(::prin1_to_string(name));
//       message->append(" does not name a class.");
//       return signal_lisp_error(message);
    return find_class_not_found(name);
//     }
  return NIL;
}

// ### find-class-1 symbol => class
Value SYS_find_class_1(Value arg)
{
  if (!symbolp(arg))
    return find_class_not_symbol(arg);
  Value value = map->get(arg);
  if (value != NULL_VALUE)
    return value;
  return find_class_not_found(arg);
}

// ### set-find-class symbol new-class => new-class
Value SYS_set_find_class(Value arg1, Value arg2)
{
  if (!symbolp(arg1))
    return signal_type_error(arg1, S_symbol);
  if (arg2 == NIL)
    map->remove(arg1);
  else
    {
      if (!classp(arg2))
        return signal_type_error(arg2, S_class);
      map->put(arg1, arg2);
    }
  return arg2;
}

// ### classp
Value EXT_classp(Value arg)
{
  return classp(arg) ? T : NIL;
}

// ### %class-precedence-list
Value SYS_xclass_precedence_list(Value arg)
{
  return check_class(arg)->cpl();
}

// REVIEW
// ### allocate-instance-standard-class => instance
Value SYS_allocate_instance_standard_class()
{
  Value value = map->get(S_standard_class);
  Layout * layout = check_layout(check_standard_object(value)->iref(3)); // REVIEW 3
  StandardClass * instance = new StandardClass();
  instance->set_layout(layout);
  return make_value(instance);
}

// REVIEW
// ### allocate-instance-funcallable-standard-class => instance
Value SYS_allocate_instance_funcallable_standard_class()
{
  Value value = map->get(S_funcallable_standard_class);
  Layout * layout = check_layout(check_standard_object(value)->iref(3)); // REVIEW 3
  FuncallableStandardClass * instance = new FuncallableStandardClass();
  instance->set_layout(layout);
  return make_value(instance);
}
