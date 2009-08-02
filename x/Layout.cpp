// Layout.cpp
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

#include "lisp.hpp"
#include "Layout.hpp"

Layout::Layout(LispClass * lisp_class, Value instance_slots, Value shared_slots)
  : TypedObject(WIDETAG_LAYOUT), _class(lisp_class), _shared_slots(shared_slots),
    _invalid(false)
{
  // REVIEW do we need this type checking here?
  if (!listp(instance_slots))
    signal_type_error(instance_slots, S_list);
  if (!listp(shared_slots))
    signal_type_error(shared_slots, S_list);

  _numslots = length(instance_slots);
  _slot_names = (Value *) GC_malloc(_numslots * sizeof(Value));
  for (INDEX i = 0; i < _numslots; i++)
    {
      _slot_names[i] = car(instance_slots);
      instance_slots = xcdr(instance_slots);
    }
}

Layout::Layout(Layout * old_layout)
  : TypedObject(WIDETAG_LAYOUT), _invalid(false)
{
  _class = old_layout->_class;
  _numslots = old_layout->_numslots;
  _slot_names = old_layout->_slot_names;
  _shared_slots = old_layout->_shared_slots;
}

bool Layout::typep(Value type) const
{
  return (type == S_layout || type == S_atom || type == T
          || type == C_layout || type == C_t);
}

Value Layout::class_of() const
{
  return C_layout;
}

// ### make-layout class instance-slots shared-slots => layout
Value SYS_make_layout(Value arg1, Value arg2, Value arg3)
{
  if (!classp(arg1))
    signal_type_error(arg1, S_class);
  if (!listp(arg2))
    signal_type_error(arg2, S_list);
  if (!listp(arg3))
    signal_type_error(arg3, S_list);
  return make_value(new Layout(the_class(arg1), arg2, arg3));
}

// ### layout-class layout => class
Value SYS_layout_class(Value arg)
{
  LispClass * lisp_class = check_layout(arg)->lisp_class();
  return lisp_class ? make_value(lisp_class) : NIL;
}

// ### layout-invalid-p layout => boolean
Value SYS_layout_invalid_p(Value arg)
{
  return check_layout(arg)->is_invalid() ? T : NIL;
}

// ### layout-slot-location layout slot-name => location
Value SYS_layout_slot_location(Value arg1, Value arg2)
{
  return check_layout(arg1)->slot_location(arg2);
}

// ### layout-slot-names layout => slot-names
Value SYS_layout_slot_names(Value arg)
{
  Layout * layout = check_layout(arg);
  Value * slot_names = layout->slot_names();
  Value result = NIL;
  for (INDEX i = layout->numslots(); i-- > 0;)
    result = make_cons(slot_names[i], result);
  return result;
}

// ### %make-instances-obsolete class => class
Value SYS_make_instances_obsolete_internal(Value arg)
{
  LispClass * lisp_class = check_class(arg);
  Layout * old_layout = lisp_class->class_layout();
  lisp_class->set_class_layout(new Layout(old_layout));
  old_layout->invalidate();
  return arg;
}
