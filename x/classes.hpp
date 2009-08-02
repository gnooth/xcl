// classes.hpp
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

#ifndef __CLASSES_HPP
#define __CLASSES_HPP

#include "StandardObject.hpp"

// specializer
// -----------
// direct-methods

// class
// -----
// prototype
// name
// layout
// precedence-list
// direct-superclasses
// direct-subclasses

// standard-class
// --------------
// direct-slots
// slots

class LispClass : public StandardObject
{
protected:
  LispClass(long widetag, INDEX numslots)
    : StandardObject(widetag, numslots)
  {
    for (INDEX i = 0; i < numslots; i++)
      xiset(i, NIL);
  }

public:
  Value name() const
  {
    return xiref(2);
  }

  void set_name(Value name)
  {
    xiset(2, name);
  }

  Layout * class_layout()
  {
    return check_layout(xiref(3));
  }

  void set_class_layout(Layout * layout)
  {
    xiset(3, make_value(layout));
  }

  Value cpl() const
  {
    return xiref(4);
  }

  void set_cpl(Value list)
  {
    xiset(4, list);
  }

  bool subclassp(Value cls) const;

  virtual AbstractString * write_to_string();
};

class BuiltInClass : public LispClass
{
  // direct-methods             0
  // prototype                  1
  // name                       2
  // layout                     3
  // precedence-list            4
  // direct-superclasses        5
  // direct-subclasses          6

public:
  BuiltInClass(Value name)
    : LispClass(WIDETAG_BUILT_IN_CLASS, 7)
  {
    set_name(name);
  }

  virtual bool typep(Value type) const;

  virtual Value type_of() const;

  virtual AbstractString * write_to_string();
};

class StandardClass : public LispClass
{
  // direct-methods              0
  // prototype                   1
  // name                        2
  // layout                      3
  // precedence-list             4
  // direct-superclasses         5
  // direct-subclasses           6
  // direct-slots                7
  // slots                       8
  // direct-default-initargs     9
  // default-initargs           10
  // finalized-p                11

public:
  StandardClass()
    : LispClass(WIDETAG_STANDARD_CLASS, 12)
  {
  }

  StandardClass(Value name)
    : LispClass(WIDETAG_STANDARD_CLASS, 12)
  {
    set_name(name);
  }

  virtual bool typep(Value type) const;

  virtual Value type_of() const
  {
    return S_standard_class;
  }
};

class FuncallableStandardClass : public LispClass
{
  // direct-methods              0
  // prototype                   1
  // name                        2
  // layout                      3
  // precedence-list             4
  // direct-superclasses         5
  // direct-subclasses           6
  // direct-slots                7
  // slots                       8
  // direct-default-initargs     9
  // default-initargs           10
  // finalized-p                11

public:
  FuncallableStandardClass()
    : LispClass(WIDETAG_FUNCALLABLE_STANDARD_CLASS, 12)
  {
  }

  FuncallableStandardClass(Value name)
    : LispClass(WIDETAG_FUNCALLABLE_STANDARD_CLASS, 12)
  {
    set_name(name);
  }

  virtual bool typep(Value type) const;

  virtual Value type_of() const
  {
    return S_funcallable_standard_class;
  }

  virtual Value class_of() const;
};

class StructureClass : public LispClass
{
  // direct-methods             0
  // prototype                  1
  // name                       2
  // layout                     3
  // precedence-list            4
  // direct-superclasses        5
  // direct-subclasses          6
  // direct-slots               7
  // slots                      8

public:
  StructureClass(Value name);

  StructureClass(Value name, Value slots);

  Value slots() const { return xiref(8); }

  void set_slots(Value slots) { xiset(8, slots); }

  virtual bool typep(Value type) const;

  virtual Value type_of() const;
};

inline bool classp(Value value)
{
  if (typed_object_p(value))
    {
      TypedObject * obj = the_typed_object(value);
      if (obj->widetag() & WIDETAG_CLASS_BIT)
        return true;
      else
        return obj->typep(S_class);
    }
  else
    return false;
}

inline LispClass * the_class(Value value)
{
  assert(classp(value));
  return reinterpret_cast<LispClass *>(value - LOWTAG_TYPED_OBJECT);
}

inline LispClass * check_class(Value value)
{
  if (classp(value))
    return the_class(value);
  signal_type_error(value, S_class);
  // Not reached.
  return NULL;
}

#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

#define DEFINE_BUILT_IN_CLASS(name)             EXTERN Value C_ ## name
#define DEFINE_STRUCTURE_CLASS(name)            EXTERN Value C_ ## name
#define DEFINE_STANDARD_CLASS(name)             EXTERN Value C_ ## name
#define DEFINE_FUNCALLABLE_STANDARD_CLASS(name) EXTERN Value C_ ## name
#define DEFINE_CONDITION_CLASS(name)            EXTERN Value C_ ## name

#include "classdefs.hpp"

#undef DEFINE_BUILT_IN_CLASS
#undef DEFINE_STRUCTURE_CLASS
#undef DEFINE_STANDARD_CLASS
#undef DEFINE_FUNCALLABLE_STANDARD_CLASS
#undef DEFINE_CONDITION_CLASS

extern void initialize_classes();

extern Value add_class(Value name, Value value);

extern Value find_class(Value name);

#include "FuncallableStandardObject.hpp"

class StandardGenericFunction : public FuncallableStandardObject
{
public:
  StandardGenericFunction(Layout * layout)
    : FuncallableStandardObject(layout)
  {
  }

  // REVIEW
  Value name() const
  {
    return xiref(0);
  }

  void set_name(Value name)
  {
    return xiset(0, name);
  }

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual AbstractString * write_to_string();
};

// inline bool standard_generic_function_p(Value value)
// {
//   return (typed_object_p(value)
//           && the_typed_object(value)->widetag() == WIDETAG_STANDARD_GENERIC_FUNCTION);
// }

// inline StandardGenericFunction * the_standard_generic_function(Value value)
// {
//   assert(standard_generic_function_p(value));
//   return reinterpret_cast<StandardGenericFunction *>(value - LOWTAG_TYPED_OBJECT);
// }

// inline StandardGenericFunction * check_standard_generic_function(Value value)
// {
//   if (standard_generic_function_p(value))
//     return the_standard_generic_function(value);
//   signal_type_error(value, S_standard_generic_function);
//   // Not reached.
//   return NULL;
// }

#endif // classes.hpp
