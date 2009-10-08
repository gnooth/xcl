// Package.hpp
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

#ifndef __PACKAGE_HPP
#define __PACKAGE_HPP

#include "SymbolHashTable.hpp"

// REVIEW thread safety
class Package : public TypedObject
{
private:
  SimpleString * _name;
  SymbolHashTable * _internal_symbols;
  SymbolHashTable * _external_symbols;
  SymbolHashTable * _shadowing_symbols;

  Value _nicknames;
  Value _use_list;
  Value _used_by_list;

  void init();

public:
  Package();

  Package(AbstractString * name);

  Package(const char * name);

  SimpleString * name() const { return _name; }

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  Value nicknames() { return _nicknames; } // REVIEW

  void add_nickname(SimpleString * nickname);

  void add_nickname(const char * nickname)
  {
    return add_nickname(new_simple_string(nickname));
  }

  Value use_list() { return _use_list; } // REVIEW
  Value used_by_list() { return _used_by_list; } // REVIEW

  void set_use_list(Value value) { _use_list = value; } // REVIEW
  void set_used_by_list(Value value) { _used_by_list = value; } // REVIEW

  virtual AbstractString * write_to_string();

  void use_package(Package * package);
  void unuse_package(Package * package);

  bool uses(Package * package);

  Value add_external_symbol(Symbol * symbol);
  Value add_external_symbol(const char * name);
  
  Value add_nil();

  Symbol * find_internal_symbol(AbstractString * name)
  {
    return _internal_symbols->get(name);
  }

  Symbol * find_external_symbol(AbstractString * name)
  {
    return _external_symbols->get(name);
  }

  Symbol * find_accessible_symbol(AbstractString * name);

  Value find_symbol(AbstractString * name);

  Value intern(AbstractString * name);

  Value intern(AbstractString * name, bool exportp);

  Value intern(const char * name, bool exportp)
  {
    return intern(new_simple_string(name), exportp);
  }

  Value import_symbol(Symbol * symbol);
  void export_symbol(Symbol * symbol);
  void unexport_symbol(Symbol * symbol);
  Value unintern_symbol(Symbol * symbol);

  void shadow(AbstractString * symbol_name);

  void shadowing_import(Symbol * symbol);

  Value internal_symbols()
  {
    return _internal_symbols->symbols();
  }

  Value external_symbols()
  {
    return _external_symbols->symbols();
  }

  Value inherited_symbols();

  Value shadowing_symbols()
  {
    return _shadowing_symbols ? _shadowing_symbols->symbols() : NIL;
  }

  Value delete_package();
  Value rename_package(Value new_name, Value new_nicknames);
};

inline bool packagep(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_PACKAGE);
}

inline Package * the_package(Value value)
{
  assert(packagep(value));
  return reinterpret_cast<Package *>(value - LOWTAG_TYPED_OBJECT);
}

inline Package * check_package(Value value)
{
  if (packagep(value))
    return the_package(value);
  signal_type_error(value, S_package);
  // Not reached.
  return NULL;
}

extern Package * coerce_to_package(Value value);

extern Package * current_package();
extern Package * current_package(Thread * thread);
extern Package * find_package(AbstractString * name);

extern Package * PACKAGE_CL;
extern Package * PACKAGE_CL_USER;
extern Package * PACKAGE_KEYWORD;
extern Package * PACKAGE_SYS;
extern Package * PACKAGE_MOP;
extern Package * PACKAGE_EXT;
extern Package * PACKAGE_TPL;
extern Package * PACKAGE_COMPILER;

#endif // Package.hpp
