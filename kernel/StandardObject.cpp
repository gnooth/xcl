// StandardObject.cpp
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
#include "primitives.hpp"
#include "StringOutputStream.hpp"
#include "StandardObject.hpp"
#include "UndefinedFunction.hpp"

Value StandardObject::type_of() const
{
  // "For objects of metaclass STRUCTURE-CLASS or STANDARD-CLASS, and for
  // conditions, TYPE-OF returns the proper name of the class returned by
  // CLASS-OF if it has a proper name, and otherwise returns the class itself."
  if (_layout)
    {
      LispClass * c = _layout->lisp_class();
      if (c)
        {
          Value name = c->name();
          if (name != NIL && find_class(name) == make_value(c))
            return name;
          else
            return make_value(c);
        }
    }
  return S_standard_object;
}

Value StandardObject::class_of() const
{
  if (_layout)
    {
      LispClass * cls = _layout->lisp_class();
      if (cls)
        return make_value(cls);
    }
  return C_standard_object;
}

bool StandardObject::typep(Value type) const
{
  if (type == S_standard_object || type == S_atom || type == T)
    return true;
  if (type == C_standard_object || type == C_t)
    return true;
  if (_layout)
    {
      LispClass * cls = _layout->lisp_class();
      if (cls)
        {
          if (type == make_value(cls))
            return true;
          if (type == cls->name())
            return true;
          Value cpl = cls->cpl();
          while (cpl != NIL)
            {
              if (type == car(cpl))
                return T;
              if (type == check_class(xcar(cpl))->name())
                return T;
              cpl = xcdr(cpl);
            }
        }
    }
  return false;
}

Layout * StandardObject::update_layout()
{
  assert(_layout->is_invalid());
  Layout * old_layout = _layout;
  LispClass * lisp_class = old_layout->lisp_class();
  Layout * new_layout = lisp_class->class_layout();
  assert(!new_layout->is_invalid());
  StandardObject * new_instance =
    new StandardObject(widetag(), new_layout, new_layout->numslots());
  assert(new_instance->_layout == new_layout);

  Value added = NIL;
  Value discarded = NIL;
  Value plist = NIL;
  // Old local slots.
  Value * old_slot_names = old_layout->slot_names();
  unsigned long old_numslots = old_layout->numslots();
  for (unsigned long i = 0; i < old_numslots; i++)
    {
      Value slot_name = old_slot_names[i];
      long j = new_layout->slot_index(slot_name);
      if (j >= 0)
        new_instance->_slots[j] = _slots[i];
      else
        {
          discarded = make_cons(slot_name, discarded);
          if (_slots[i] != UNBOUND_VALUE)
            {
              plist = make_cons(slot_name, plist);
              plist = make_cons(_slots[i], plist);
            }
        }
    }
  // Old shared slots.
  Value rest = old_layout->shared_slots(); // A list.
  while (rest != NIL)
    {
      Value location = car(rest);
      Value slot_name = car(location);
      int i = new_layout->slot_index(slot_name);
      if (i >= 0)
        new_instance->_slots[i] = xcdr(location);
      rest = xcdr(rest);
    }
  // Go through all the new local slots to compute the added slots.
  Value * new_slot_names = new_layout->slot_names();
  INDEX new_numslots = new_layout->numslots();
  for (INDEX i = 0; i < new_numslots; i++)
    {
      Value slot_name = new_slot_names[i];
      int j = old_layout->slot_index(slot_name);
      if (j >= 0)
        continue;
      Value location = old_layout->shared_slot_location(slot_name);
      if (location != NIL)
        continue;
      // Not found.
      added = make_cons(slot_name, added);
    }
  // Swap slots.
  Value * temp_slots = _slots;
  _slots = new_instance->_slots;
  new_instance->_slots = temp_slots;
  INDEX temp_numslots = _numslots;
  _numslots = new_instance->_numslots;
  new_instance->_numslots = temp_numslots;
  // Swap layouts.
  Layout * temp_layout = _layout;
  _layout = new_instance->_layout;
  new_instance->_layout = temp_layout;
  assert(!_layout->is_invalid());
  // Call UPDATE-INSTANCE-FOR-REDEFINED-CLASS.
  TypedObject * function = the_symbol(S_update_instance_for_redefined_class)->function();
  if (function)
    current_thread()->execute(function, make_value(this), added, discarded, plist);
  else
    signal_undefined_function(S_update_instance_for_redefined_class);
  return new_layout;
}

AbstractString * StandardObject::write_to_string()
{
  if (CL_fboundp(S_print_object) != NIL)
    {
      Thread * const thread = current_thread();
      StringOutputStream * stream = new StringOutputStream(S_character);
      thread->execute(the_symbol(S_print_object)->function(),
                      make_value(this),
                      make_value(stream));
      AbstractString * s = stream->get_string();
      return s;
    }
  else
    return unreadable_string();
}

// ### standard-object-p
Value SYS_standard_object_p(Value arg)
{
  return standard_object_p(arg) ? T : NIL;
}

// ### swap-slots instance-1 instance-2 => t
Value SYS_swap_slots(Value arg1, Value arg2)
{
  StandardObject * instance1 = check_standard_object(arg1);
  StandardObject * instance2 = check_standard_object(arg2);
  Value * slots1 = instance1->slots();
  long numslots1 = instance1->numslots();
  instance1->set_slots(instance2->slots());
  instance1->set_numslots(instance2->numslots());
  instance2->set_slots(slots1);
  instance2->set_numslots(numslots1);
  return T;
}

// ### allocate-standard-instance layout => instance
Value SYS_allocate_standard_instance(Value arg)
{
  Layout * layout = check_layout(arg);
  unsigned long numslots = layout->numslots();
  StandardObject * instance = new StandardObject(WIDETAG_STANDARD_OBJECT, layout, numslots);
  return make_value(instance);
}

// ### allocate-funcallable-standard-instance layout => instance
Value SYS_allocate_funcallable_standard_instance(Value arg)
{
  Layout * layout = check_layout(arg);
//   unsigned long numslots = layout->numslots();
  FuncallableStandardObject * instance =
    new FuncallableStandardObject(layout);
  return make_value(instance);
}

// ### std-instance-layout instance => layout
Value SYS_std_instance_layout(Value arg)
{
  StandardObject * instance = check_standard_object(arg);
  Layout * layout = instance->layout();
  if (!layout)
    return signal_lisp_error("No layout for instance.");
  if (layout->is_invalid())
    {
      // Update instance.
      layout = instance->update_layout();
    }
  return layout ? make_value(layout) : NIL;
}

// ### set-std-instance-layout instance new-layout => new-layout
Value SYS_set_std_instance_layout(Value arg1, Value arg2)
{
  check_standard_object(arg1)->set_layout(check_layout(arg2));
  return arg2;
}

Value StandardObject::slot_value(Value arg)
{
  if (!symbolp(arg))
    return signal_type_error(arg, S_symbol);
  Layout * layout = this->layout();
  if (!layout)
    return signal_lisp_error("No layout for instance.");
  if (layout->is_invalid())
    {
      // Update instance.
      layout = update_layout();
    }
  Value value;
  long index = layout->slot_index(arg);
  if (index >= 0)
    {
      value = iref(index);
    }
  else
    {
      // not an instance slot
      Value location = layout->shared_slot_location(arg);
      if (location == NIL)
        {
          // slot-missing
          return current_thread()->execute(the_symbol(S_slot_missing)->function(),
                                           class_of(),
                                           make_value(this),
                                           arg,
                                           S_slot_value);
        }
      value = cdr(location);
    }
  if (value == UNBOUND_VALUE)
    {
      Thread * const thread = current_thread();
      value = thread->execute(the_symbol(S_slot_unbound)->function(),
                              class_of(),
                              make_value(this),
                              arg);
      thread->clear_values();
    }
  return value;
}

// ### std-instance-slot-value instance slot-name => value
Value SYS_std_instance_slot_value(Value arg1, Value arg2)
{
  return check_standard_object(arg1)->slot_value(arg2);
}

void StandardObject::set_slot_value(Value arg1, Value arg2)
{
  if (!symbolp(arg1))
    {
      signal_type_error(arg1, S_symbol);
      return;
    }
  Layout * layout = this->layout();
  if (!layout)
    {
      signal_lisp_error("No layout for instance.");
      return;
    }
  if (layout->is_invalid())
    {
      // Update instance.
      layout = update_layout();
    }
  long index = layout->slot_index(arg1);
  if (index >= 0)
    {
      iset(index, arg2);
    }
  else
    {
      // not an instance slot
      Value location = layout->shared_slot_location(arg1);
      if (location != NIL)
        SYS_setcdr(location, arg2);
      else
        {
          // slot-missing
          current_thread()->execute(the_symbol(S_slot_missing)->function(),
                                    class_of(),
                                    make_value(this),
                                    arg1,
                                    S_setf,
                                    arg2);
        }
    }
}

// ### set-std-instance-slot-value instance slot-name value => value
Value SYS_set_std_instance_slot_value(Value arg1, Value arg2, Value arg3)
{
  check_standard_object(arg1)->set_slot_value(arg2, arg3);
  return arg3;
}

// ### iref instance index => value
Value SYS_iref(Value arg1, Value arg2)
{
  return check_standard_object(arg1)->iref(check_index(arg2));
}

// ### iset instance index new-value => new-value
Value SYS_iset(Value arg1, Value arg2, Value arg3)
{
  check_standard_object(arg1)->iset(check_index(arg2), arg3);
  return arg3;
}
