// Package.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "HashTable.hpp"
#include "Package.hpp"
#include "PackageError.hpp"
#include "PrintNotReadable.hpp"

Package * PACKAGE_CL;
Package * PACKAGE_CL_USER;
Package * PACKAGE_KEYWORD;
Package * PACKAGE_SYS;
Package * PACKAGE_MOP;
Package * PACKAGE_EXT;
Package * PACKAGE_TPL;
Package * PACKAGE_COMPILER;

static EqualHashTable * package_map;
static Value all_packages;

// anonymous package, not in map
Package::Package()
  : TypedObject(WIDETAG_PACKAGE), _name(new_simple_string("anonymous package")), _shadowing_symbols(NULL),
    _nicknames(NIL), _use_list(NIL), _used_by_list(NIL)
{
  _internal_symbols = new SymbolHashTable();
  _external_symbols = new SymbolHashTable();
}

Package::Package(AbstractString * name)
 : TypedObject(WIDETAG_PACKAGE), _name(new_simple_string(name)), _shadowing_symbols(NULL),
   _nicknames(NIL), _use_list(NIL), _used_by_list(NIL)
{
  init();
}

Package::Package(const char * name)
  : TypedObject(WIDETAG_PACKAGE), _name(new_simple_string(name)), _shadowing_symbols(NULL),
    _nicknames(NIL), _use_list(NIL), _used_by_list(NIL)
{
  init();
}

void Package::init()
{
  _internal_symbols = new SymbolHashTable();
  _external_symbols = new SymbolHashTable();
  Value package = make_value(this);
  package_map->put(make_value(_name), package);
  all_packages = make_cons(package, all_packages);
}

Value Package::type_of() const
{
  return S_package;
}

Value Package::class_of() const
{
  return C_package;
}

bool Package::typep(Value type) const
{
  return (type == S_package || type == S_atom || type == T
          || type == C_package || type == C_t);
}

AbstractString * Package::write_to_string()
{
  if (current_thread()->symbol_value(S_print_readably) != NIL)
    {
      if (_name)
        {
          String * s = new String();
          s->append("#.(");
          s->append(the_symbol(S_find_package)->prin1_to_string());
          s->append_char(' ');
          s->append(_name->prin1_to_string());
          s->append_char(')');
          return s;
        }
      signal_lisp_error(new PrintNotReadable(make_value(this)));
      // not reached
      return NULL;
    }
  String * s = new String(::write_to_string(S_package));
  if (_name)
    {
      s->append_char(' ');
      s->append_char('"');
      s->append(_name);
      s->append_char('"');
    }
  return unreadable_string(s);
}

// FIXME not thread-safe
void Package::add_nickname(SimpleString * s)
{
  Value nickname = make_value(s);
  Value package = package_map->get(nickname);
  if (package == NULL_VALUE)
    package_map->put(nickname, make_value(this));
  else if (package != make_value(this))
    {
      String * message = new String("A package named ");
      message->append(s);
      message->append(" already exists.");
      signal_lisp_error(new PackageError(message, nickname));
      return;
    }
  Value list = _nicknames;
  while (list != NIL)
    {
      if (::equal(car(list), nickname))
        return;
      list = cdr(list);
    }
  _nicknames = make_cons(nickname, _nicknames);
}

// FIXME not thread-safe
void Package::use_package(Package * package)
{
  Value value = make_value(package);
  if (!memq(value, _use_list))
    {
      _use_list = make_cons(value, _use_list);

      // add this package to package's used-by list
      Value used_by_list = package->used_by_list();
      used_by_list = make_cons(make_value(this), used_by_list);
      package->set_used_by_list(used_by_list);
    }
}

// ### use-package packages-to-use &optional package => t
Value CL_use_package(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_use_package, numargs, 1, 2);
    }
  Value packages_to_use = args[0];
  if (listp(packages_to_use))
    {
      while (packages_to_use != NIL)
        {
          package->use_package(coerce_to_package(car(packages_to_use)));
          packages_to_use = xcdr(packages_to_use);
        }
    }
  else
    package->use_package(coerce_to_package(packages_to_use));
  return T;
}

// FIXME not thread-safe
void Package::unuse_package(Package * package)
{
  Value value = make_value(package);
  if (memq(value, _use_list))
    {
      _use_list = SYS_list_delete_eq(value, _use_list);

      // remove this package from package's used-by list
      Value used_by_list = package->used_by_list();
      used_by_list = SYS_list_delete_eq(make_value(this), used_by_list);
      package->set_used_by_list(used_by_list);
    }
}

// ### unuse-package packages-to-unuse &optional package => t
Value CL_unuse_package(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_unuse_package, numargs, 1, 2);
    }
  Value packages_to_unuse = args[0];
  if (listp(packages_to_unuse))
    {
      while (packages_to_unuse != NIL)
        {
          package->unuse_package(coerce_to_package(car(packages_to_unuse)));
          packages_to_unuse = xcdr(packages_to_unuse);
        }
    }
  else
    package->unuse_package(coerce_to_package(packages_to_unuse));
  return T;
}

bool Package::uses(Package * package)
{
  Value item = make_value(package);
  Value list = _use_list;
  while (list != NIL)
    {
      if (item == xcar(list))
        return true;
      list = xcdr(list);
    }
  return false;
}

Value Package::add_external_symbol(Symbol * symbol)
{
  assert(symbol->name() != NULL);
  assert(_internal_symbols->get(symbol->name()) == NULL);
  assert(_external_symbols->get(symbol->name()) == NULL);
  _external_symbols->put(symbol);
  return make_value(symbol);
}

Value Package::add_external_symbol(const char * name)
{
  assert(_internal_symbols->get(name) == 0);
  assert(_external_symbols->get(name) == 0);
  Symbol * symbol = new Symbol(name, this);
  _external_symbols->put(symbol);
  return make_value(symbol);
}

Value Package::add_nil()
{
  assert(this == PACKAGE_CL);
  Symbol * symbol = new Symbol("NIL", this);
  _external_symbols->put(symbol);
  return make_value(symbol, LOWTAG_SYMBOL);
}

Symbol * Package::find_accessible_symbol(AbstractString * name)
{
  // Look in external and internal symbols of this package.
  Symbol * symbol = _external_symbols->get(name);
  if (symbol)
    return symbol;
  symbol = _internal_symbols->get(name);
  if (symbol)
    return symbol;
  // Look in external symbols of used packages.
  if (consp(_use_list))
    {
      Value list = _use_list;
      while (list != NIL)
        {
          Package * package = check_package(car(list));
          symbol = package->_external_symbols->get(name);
          if (symbol)
            return symbol;
          list = xcdr(list);
        }
    }
  // not found
  return NULL;
}

Value Package::find_symbol(AbstractString * name)
{
  Thread * thread = current_thread();
  // Look in external and internal symbols of this package.
  Symbol * symbol = _external_symbols->get(name);
  if (symbol)
    return thread->set_values(make_value(symbol), K_external);
  symbol = _internal_symbols->get(name);
  if (symbol)
    return thread->set_values(make_value(symbol), K_internal);
  // Look in external symbols of used packages.
  if (consp(_use_list))
    {
      Value list = _use_list;
      while (list != NIL)
        {
          Package * package = check_package(car(list));
          symbol = package->_external_symbols->get(name);
          if (symbol)
            return thread->set_values(make_value(symbol), K_inherited);
          list = xcdr(list);
        }
    }
  // not found
  return thread->set_values(NIL, NIL);
}

// intern string &optional package => symbol, status
Value Package::intern(AbstractString * name)
{
  Thread * thread = current_thread();
  // Look in external and internal symbols of this package.
  Symbol * symbol = _external_symbols->get(name);
  if (symbol)
    return thread->set_values(make_value(symbol), K_external);
  symbol = _internal_symbols->get(name);
  if (symbol)
    return thread->set_values(make_value(symbol), K_internal);
  // look in external symbols of used packages
  if (consp(_use_list))
    {
      Value list = _use_list;
      while (list != NIL)
        {
          Package * package = check_package(car(list));
          symbol = package->_external_symbols->get(name);
          if (symbol)
            return thread->set_values(make_value(symbol), K_inherited);
          list = cdr(list);
        }
    }
  // not found
  symbol = new Symbol(name, this);
  if (this == PACKAGE_KEYWORD)
    {
      symbol->set_constant_value(make_value(symbol));
      _external_symbols->put(symbol);
    }
  else
    _internal_symbols->put(symbol);
  return thread->set_values(make_value(symbol), NIL);
}

Value Package::intern(AbstractString * name, bool exportp)
{
  // look in external and internal symbols of this package
  Symbol * symbol = _external_symbols->get(name);
  if (symbol)
    return make_value(symbol);

  symbol = _internal_symbols->get(name);
  if (symbol)
    {
      assert(!exportp);
      return make_value(symbol);
    }

  // Look in external symbols of used packages.
  if (consp(_use_list))
    {
      Value list = _use_list;
      while (list != NIL)
        {
          Package * package = check_package(car(list));
          symbol = package->_external_symbols->get(name);
          if (symbol)
            return make_value(symbol);
          list = cdr(list);
        }
    }

  // not found
  symbol = new Symbol(name, this);
  if (this == PACKAGE_KEYWORD)
    {
      symbol->set_constant_value(make_value(symbol));
      _external_symbols->put(symbol);
    }
  else if (exportp)
    _external_symbols->put(symbol);
  else
    _internal_symbols->put(symbol);
  return make_value(symbol);
}

Value Package::import_symbol(Symbol * symbol)
{
  assert(symbol != NULL);
  if (symbol->package() == make_value(this))
    return T; // Nothing to do.
  SimpleString * name = symbol->name();
  Symbol * sym = find_accessible_symbol(name);
  if (sym && sym != symbol)
    {
      String * s = new String("The symbol ");
      s->append(sym->prin1_to_string());
      s->append(" is already accessible in ");
      s->append(prin1_to_string());
      s->append_char('.');
      return signal_lisp_error(new PackageError(s, make_value(this)));
    }
  _internal_symbols->put(symbol);
  if (symbol->package() == NIL)
    symbol->set_package(make_value(this));
  return T;
}

void Package::export_symbol(Symbol * symbol)
{
  assert(symbol != NULL);

  // EXPORT.5
  Value list = used_by_list();
  while (list != NIL)
    {
      Package * package = check_package(car(list));
      Symbol * sym = package->find_external_symbol(symbol->name());
      if (sym != NULL && sym != symbol)
        {
          String * s = new String("A symbol named ");
          s->append(symbol->name()->prin1_to_string());
          s->append(" is already exported by ");
          s->append(package->prin1_to_string());
          s->append(", which uses ");
          s->append(prin1_to_string());
          s->append_char('.');
          signal_lisp_error(new PackageError(s, make_value(this)));
          // Not reached.
          return;
        }
      list = xcdr(list);
    }

  if (symbol->package() == make_value(this))
    {
      AbstractString * name = symbol->name();
      if (_external_symbols->get(name) == symbol)
        return; // nothing to do
      if (_internal_symbols->get(name) != symbol)
        assert(false); // can't happen
      if (_internal_symbols->remove(symbol) != symbol)
        assert(false); // can't happen
      assert(_internal_symbols->get(name) == NULL);
      add_external_symbol(symbol);
      return;
    }
  Symbol * sym = find_accessible_symbol(symbol->name());
  if (sym != symbol)
    {
      String * s = new String("The symbol ");
      s->append(symbol->prin1_to_string());
      s->append(" is not accessible in ");
      s->append(prin1_to_string());
      s->append_char('.');
      signal_lisp_error(new PackageError(s, make_value(this)));
      // Not reached.
      return;
    }
  _internal_symbols->remove(symbol);
  _external_symbols->put(symbol);
}

void Package::unexport_symbol(Symbol * symbol)
{
  assert(symbol != NULL);
  if (symbol->package() == make_value(this))
    {
      if (_external_symbols->get(symbol->name()) == symbol)
        {
          _external_symbols->remove(symbol);
          _internal_symbols->put(symbol);
        }
    }
  else
    {
      // Signal an error if symbol is not accessible.
      if (_use_list != NIL)
        {
          SimpleString * name = symbol->name();
          Value used_packages = _use_list;
          while (used_packages != NIL)
            {
              Package * package = check_package(car(used_packages));
              if (package->find_external_symbol(name) == symbol)
                return; // OK.
              used_packages = xcdr(used_packages);
            }
        }
      // Not accessible.
      String * s = new String("The symbol ");
      s->append(symbol->prin1_to_string());
      s->append(" is not accessible in ");
      s->append(prin1_to_string());
      s->append_char('.');
      signal_lisp_error(new PackageError(s, make_value(this)));
    }
}

Value Package::unintern_symbol(Symbol * symbol)
{
  assert(symbol != NULL);
  AbstractString * name = symbol->name();
  // Check for conflicts that might be exposed in used package list if we
  // remove a shadowing symbol. (UNINTERN.8)
  if (_use_list != NIL && _shadowing_symbols != NULL
      && _shadowing_symbols->get(name) == symbol)
    {
      Symbol * sym = NULL;
      Value used_packages = _use_list;
      while (used_packages != NIL)
        {
          Package * package = check_package(car(used_packages));
          Symbol * s = package->find_external_symbol(symbol->name());
          if (s != NULL)
            {
              if (sym == NULL)
                sym = s;
              else if (sym != s)
                {
                  String * message = new String("Uninterning the symbol ");
                  message->append(symbol->prin1_to_string());
                  message->append(" causes a name conflict between ");
                  message->append(sym->prin1_to_string());
                  message->append(" and ");
                  message->append(s->prin1_to_string());
                  message->append_char('.');
                  return signal_lisp_error(new PackageError(message, make_value(this)));
                }
            }
          used_packages = xcdr(used_packages);
        }
    }
  // Reaching here, it's OK to remove the symbol.
  if (_internal_symbols->get(name) == symbol)
    _internal_symbols->remove(symbol);
  else if (_external_symbols->get(name) == symbol)
    _external_symbols->remove(symbol);
  else
    // not found
    return NIL;
  assert(_internal_symbols->get(name) == NULL);
  assert(_external_symbols->get(name) == NULL);
  if (symbol->package() == make_value(this))
    symbol->set_package(NIL);
  return T;
}

void Package::shadow(AbstractString * symbol_name)
{
  if (_shadowing_symbols == NULL)
    _shadowing_symbols = new SymbolHashTable();
  SimpleString * s = new_simple_string(symbol_name);
  Symbol * symbol = _external_symbols->get(s);
  if (symbol)
    {
      _shadowing_symbols->put(symbol);
      return;
    }
  symbol = _internal_symbols->get(s);
  if (symbol)
    {
      _shadowing_symbols->put(symbol);
      return;
    }
  if (_shadowing_symbols->get(s))
    return;
  symbol = new Symbol(s, this);
  _internal_symbols->put(symbol);
  _shadowing_symbols->put(symbol);
}

// ### shadow symbol-names &optional package => t
Value CL_shadow(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_shadow, numargs, 1, 2);
    }
  Value symbol_names = args[0];
  if (listp(symbol_names))
    {
      while (symbol_names != NIL)
        {
          package->shadow(string(car(symbol_names)));
          symbol_names = xcdr(symbol_names);
        }
    }
  else
    package->shadow(string(symbol_names));
  return T;
}

void Package::shadowing_import(Symbol * symbol)
{
  if (_shadowing_symbols == NULL)
    _shadowing_symbols = new SymbolHashTable();
  SimpleString * s = symbol->name();
  Symbol * sym = _internal_symbols->get(s);
  if (sym)
    _internal_symbols->remove(sym);
  else
    {
      sym = _external_symbols->get(s);
      if (sym)
        _external_symbols->remove(sym);
    }
  _internal_symbols->put(symbol);
  _shadowing_symbols->put(symbol);
}

// ### shadowing-import symbols &optional package => t
Value CL_shadowing_import(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_shadowing_import, numargs, 1, 2);
    }
  Value symbols = args[0];
  if (listp(symbols))
    {
      while (symbols != NIL)
        {
          package->shadowing_import(check_symbol(car(symbols)));
          symbols = xcdr(symbols);
        }
    }
  else
    package->shadowing_import(check_symbol(symbols));
  return T;
}

Value Package::delete_package()
{
  if (_name != NULL)
    {
      package_map->remove(make_value(_name));
      _name = NULL;
      while (_nicknames != NIL)
        {
          Value nickname = car(_nicknames);
          package_map->remove(nickname);
          _nicknames = xcdr(_nicknames);
        }
      Value this_package = make_value(this);
      all_packages = SYS_list_delete_eq(this_package, all_packages);
      Value list = external_symbols();
      while (list != NIL)
        {
          Symbol * symbol = check_symbol(xcar(list));
          if (symbol->package() == this_package)
            symbol->set_package(NIL);
          _external_symbols->remove(symbol);
          list = xcdr(list);
        }
      list = internal_symbols();
      while (list != NIL)
        {
          Symbol * symbol = check_symbol(xcar(list));
          if (symbol->package() == this_package)
            symbol->set_package(NIL);
          _internal_symbols->remove(symbol);
          list = xcdr(list);
        }
      return T;
    }
  return NIL;
}

// FIXME not thread-safe
Value Package::rename_package(Value new_name, Value new_nicknames)
{
  // "The consequences are undefined if NEW-NAME or any NEW-NICKNAME conflicts
  // with any existing package names."
  AbstractString * conflicting_name = NULL;
  do
    {
      // FIXME new_name can be a package object as well as a string designator
      AbstractString * name = string(new_name);
      Package * p = find_package(name);
      if (p && p != this)
        {
          conflicting_name = name;
          break;
        }
      Value nicks = new_nicknames;
      while (nicks != NIL)
        {
          Value nick = car(nicks);
          name = string(nick);
          p = find_package(name);
          if (p && p != this)
            {
              conflicting_name = name;
              break;
            }
          nicks = xcdr(nicks);
        }
    }
  while (false);

  if (conflicting_name)
    {
      String * s = new String("A package named ");
      s->append(conflicting_name->prin1_to_string());
      s->append(" already exists.");
      return signal_lisp_error(new PackageError(s, make_value(conflicting_name)));
    }

  // No conflicts.
  if (_name != NULL)
    {
      package_map->remove(make_value(_name));
      _name = NULL;
    }
  while (_nicknames != NIL)
    {
      package_map->remove(car(_nicknames));
      _nicknames = xcdr(_nicknames);
    }
  _name = new_simple_string(string(new_name));
  Value this_package = make_value(this);
  package_map->put(make_value(_name), this_package);
  Value nicks = new_nicknames;
  while (nicks != NIL)
    {
      Value nickname = make_value(new_simple_string(string(car(nicks))));
      package_map->put(nickname, this_package);
      if (!memq(nickname, _nicknames))
        _nicknames = make_cons(nickname, _nicknames);
      nicks = xcdr(nicks);
    }
  return this_package;
}

// ### rename-package package new-name &optional new-nicknames => package-object
Value CL_rename_package(unsigned int numargs, Value args[])
{
  if (numargs < 2 || numargs > 3)
    return wrong_number_of_arguments(S_rename_package, numargs, 2, 3);
  Package * package = coerce_to_package(args[0]);
  return package->rename_package(args[1], numargs == 3 ? args[2] : NIL);
}

// ### %delete-package
Value SYS_delete_package_internal(Value arg)
{
  return coerce_to_package(arg)->delete_package();
}

Package * current_package()
{
  Value value = current_thread()->lookup_special(S_current_package);
  if (value == NULL_VALUE)
    value = the_symbol(S_current_package)->value();
  return check_package(value);
}

Package * current_package(Thread * thread)
{
  Value value = thread->lookup_special(S_current_package);
  if (value == NULL_VALUE)
    value = the_symbol(S_current_package)->value();
  return check_package(value);
}

Package * coerce_to_package(Value value)
{
  if (packagep(value))
    return the_package(value);
  Value package = package_map->get(CL_string(value));
  if (package != NULL_VALUE)
    return check_package(package);
  String * s = new String(prin1_to_string(value));
  s->append(" is not the name of a package.");
  signal_lisp_error(new PackageError(s, value));
  // not reached
  return NULL;
}

Package * find_package(AbstractString * name)
{
  Value package = package_map->get(make_value(name));
  return package != NULL_VALUE ? check_package(package) : NULL;
}

// ### find-package
Value CL_find_package(Value arg)
{
  if (packagep(arg))
    return arg;
  Value package = package_map->get(CL_string(arg));
  return package != NULL_VALUE ? package : NIL;
}

// ### package-nicknames
Value CL_package_nicknames(Value arg)
{
  return coerce_to_package(arg)->nicknames();
}

// ### package-use-list
Value CL_package_use_list(Value arg)
{
  return coerce_to_package(arg)->use_list();
}

// ### package_used_by_list
Value CL_package_used_by_list(Value arg)
{
  return coerce_to_package(arg)->used_by_list();
}

// ### intern
Value CL_intern(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      {
        AbstractString * string = check_string(args[0]);
        return current_package()->intern(string);
      }
    case 2:
      {
        AbstractString * string = check_string(args[0]);
        Value package = CL_find_package(args[1]);
        return the_package(package)->intern(string);
      }
    default:
      return wrong_number_of_arguments(S_intern, numargs, 1, 2);
    }
}

// ### import symbols &optional package => t
Value CL_import(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_import, numargs, 1, 2);
    }
  Value list = args[0];
  if (!listp(list))
    list = make_cons(list);
  while (list != NIL)
    {
      Symbol * symbol = check_symbol(car(list));
      package->import_symbol(symbol);
      list = cdr(list);
    }
  return T;
}

// ### export symbols &optional package => t
Value CL_export(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_export, numargs, 1, 2);
    }
  Value list = args[0];
  if (!listp(list))
    list = make_cons(list);
  while (list != NIL)
    {
      Symbol * symbol = check_symbol(car(list));
      package->export_symbol(symbol);
      list = xcdr(list);
    }
  return T;
}

// ### unexport symbols &optional package => t
Value CL_unexport(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_unexport, numargs, 1, 2);
    }
  Value list = args[0];
  if (!listp(list))
    list = make_cons(list);
  while (list != NIL)
    {
      Symbol * symbol = check_symbol(car(list));
      package->unexport_symbol(symbol);
      list = cdr(list);
    }
  return T;
}

// ### unintern symbol &optional package => generalized-boolean
Value CL_unintern(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      {
        Symbol * symbol = check_symbol(args[0]);
        Package * package = current_package();
        return package->unintern_symbol(symbol);
      }
    case 2:
      {
        Symbol * symbol = check_symbol(args[0]);
        Package * package = coerce_to_package(args[1]);
        return package->unintern_symbol(symbol);
      }
    default:
      return wrong_number_of_arguments(S_unintern, numargs, 1, 2);
    }
}

// ### find-symbol string &optional package => symbol, status
Value CL_find_symbol(unsigned int numargs, Value args[])
{
  Package * package;
  switch (numargs)
    {
    case 1:
      package = current_package();
      break;
    case 2:
      package = coerce_to_package(args[1]);
      break;
    default:
      return wrong_number_of_arguments(S_find_symbol, numargs, 1, 2);
    }
  return package->find_symbol(check_string(args[0]));
}

// ### %make-package package-name nicknames use => package
Value SYS_make_package_internal(Value arg1, Value arg2, Value arg3)
{
  SimpleString * package_name = new_simple_string(string(arg1));
  Package * package = find_package(package_name);
  if (package)
    {
      String * s = new String("A package named ");
      s->append(package_name);
      s->append(" already exists.");
      signal_lisp_error(s);
    }
  Value nicknames = check_list(arg2);
  if (nicknames != NIL)
    {
      Value list = nicknames;
      while (list != NIL)
        {
          AbstractString * nick = string(car(list));
          if (find_package(nick))
            {
              String * s = new String("A package named ");
              s->append(nick);
              s->append(" already exists.");
              return signal_lisp_error(new PackageError(s, make_value(nick)));
            }
          list = xcdr(list);
        }
    }
  Value use = check_list(arg3);
  if (use != NIL)
    {
      Value list = use;
      while (list != NIL)
        {
          // Must be a package designator.
          coerce_to_package(car(list));
          list = xcdr(list);
        }
    }
  // Now create the package.
  package = new Package(package_name);
  // Add the nicknames.
  while (nicknames != NIL)
    {
      SimpleString * nickname = new_simple_string(string(xcar(nicknames)));
      package->add_nickname(nickname);
      nicknames = xcdr(nicknames);
    }
  // Create the use list.
  while (use != NIL)
    {
//       Value obj = car(use);
//       if (packagep(obj))
//         package->use_package(obj);
//       else
//         {
//           String s = javaString(obj);
//           Package p = Packages.findPackage(s);
//           if (p == null)
//             {
//               signal(new LispError(obj.writeToString() +
//                                    " is not the name of a package."));
//               return NIL;
//             }
//           package.usePackage(p);
//         }
      package->use_package(coerce_to_package(xcar(use)));
      use = xcdr(use);
    }
  return make_value(package);
}

// ### %defpackage name nicknames size shadows shadowing-imports use
// imports interns exports doc-string => package
Value SYS_defpackage_internal(unsigned int numargs, Value args[])
{
  if (numargs != 10)
    return wrong_number_of_arguments(S_defpackage_internal, numargs, 10, 10);

  AbstractString * package_name = check_string(args[0]);
  Value nicknames = check_list(args[1]);
  // FIXME size is ignored
  // Value size = args[2];
  Value shadows = check_list(args[3]);
  Value shadowing_imports = check_list(args[4]);
  Value use = check_list(args[5]);
  Value imports = check_list(args[6]);
  Value interns = check_list(args[7]);
  Value exports = check_list(args[8]);
  // FIXME docString is ignored
  // Value docString = args[9];
  Package * package = find_package(package_name);
  if (package)
    return make_value(package); // REVIEW
  if (nicknames != NIL)
    {
      Value list = nicknames;
      while (list != NIL)
        {
          AbstractString * nick = string(car(list));
          if (find_package(nick))
            {
              String * s = new String("A package named ");
              s->append(nick);
              s->append(" already exists.");
              return signal_lisp_error(new PackageError(s, make_value(nick)));
            }
          list = xcdr(list);
        }
    }
  package = new Package(package_name);
  while (nicknames != NIL)
    {
      SimpleString * nickname = new_simple_string(string(xcar(nicknames)));
      package->add_nickname(nickname);
      nicknames = xcdr(nicknames);
    }
  while (shadows != NIL)
    {
      AbstractString * symbol_name = check_string(car(shadows));
      package->shadow(symbol_name);
      shadows = xcdr(shadows);
    }
  while (shadowing_imports != NIL)
    {
      Value entry = car(shadowing_imports);
      Package * other_package = coerce_to_package(car(entry));
      Value symbol_names = cdr(entry);
      while (symbol_names != NIL)
        {
          AbstractString * symbol_name = check_string(car(symbol_names));
          Symbol * sym = other_package->find_accessible_symbol(symbol_name);
          if (sym)
            package->shadowing_import(sym);
          else
            {
              String * s = new String("No symbol named ");
              s->append(symbol_name->prin1_to_string());
              s->append(" is accessible in ");
              s->append(other_package->prin1_to_string());
              s->append_char('.');
              return signal_lisp_error(s);
            }
          symbol_names = xcdr(symbol_names);
        }
      shadowing_imports = xcdr(shadowing_imports);
    }
  while (use != NIL)
    {
      package->use_package(coerce_to_package(car(use)));
      use = xcdr(use);
    }
  while (imports != NIL)
    {
      Value entry = car(imports);
      Package * other_package = coerce_to_package(car(entry));
      Value symbol_names = cdr(entry);
      while (symbol_names != NIL)
        {
          AbstractString * symbol_name = check_string(car(symbol_names));
          Symbol * sym = other_package->find_accessible_symbol(symbol_name);
          if (sym)
            package->import_symbol(sym);
          else
            {
              String * s = new String("No symbol named ");
              s->append(symbol_name->prin1_to_string());
              s->append(" is accessible in ");
              s->append(other_package->prin1_to_string());
              s->append_char('.');
              return signal_lisp_error(s);
            }
          symbol_names = xcdr(symbol_names);
        }
      imports = xcdr(imports);
    }
  while (interns != NIL)
    {
      package->intern(check_string(car(interns)));
      interns = xcdr(interns);
    }
  while (exports != NIL)
    {
      // REVIEW ugly
      package->export_symbol(the_symbol(package->intern(check_string(car(exports)))));
      exports = xcdr(exports);
    }
  // intern() returns multiple values above, but we need to return a single value here
  current_thread()->clear_values();
  return make_value(package);
}

// ### list-all-packages
Value CL_list_all_packages()
{
  return CL_reverse(all_packages);
}

// ### packagep
Value CL_packagep(Value arg)
{
  return packagep(arg) ? T : NIL;
}

// ### package-name
Value CL_package_name(Value arg)
{
  SimpleString * name = coerce_to_package(arg)->name();
  return name ? make_value(name) : NIL;
}

// ### package-external-symbols
Value SYS_package_external_symbols(Value arg)
{
  return coerce_to_package(arg)->external_symbols();
}

// ### package-internal-symbols
Value SYS_package_internal_symbols(Value arg)
{
  return coerce_to_package(arg)->internal_symbols();
}

Value Package::inherited_symbols()
{
  Value result = NIL;
  Value used_packages = use_list();
  while (used_packages != NIL)
    {
      Package * p = check_package(car(used_packages));
      Value externals = p->external_symbols();
      while (externals != NIL)
        {
          Symbol * symbol = the_symbol(xcar(externals));
          SimpleString * symbol_name = symbol->name();
          externals = xcdr(externals);
          if (_shadowing_symbols != NULL && _shadowing_symbols->get(symbol_name) != NULL)
            continue;
          if (_external_symbols->get(symbol_name) == symbol)
            continue;
          result = make_cons(make_value(symbol), result);
        }
      used_packages = xcdr(used_packages);
    }
  return result;

}

// ### package-inherited-symbols
Value SYS_package_inherited_symbols(Value arg)
{
  return coerce_to_package(arg)->inherited_symbols();
}

// ### package-symbols
Value SYS_package_symbols(Value arg)
{
  Package * package = coerce_to_package(arg);
  return SYS_two_arg_append(package->external_symbols(), package->internal_symbols());
}

// ### package-shadowing-symbols package => symbols
Value CL_package_shadowing_symbols(Value arg)
{
  return coerce_to_package(arg)->shadowing_symbols();
}

// ### %in-package name => package
Value SYS_in_package_internal(Value arg)
{
  Value package = package_map->get(arg);
  if (package == NULL_VALUE)
    {
      AbstractString * package_name = check_string(arg);
      String * s = new String("The name ");
      s->append(package_name);
      s->append(" does not designate a package.");
      return signal_lisp_error(new PackageError(s, arg));
    }
  return current_thread()->set_symbol_value(S_current_package, package);
}

void initialize_packages_1()
{
  package_map = new EqualHashTable();
  all_packages = NIL;

  PACKAGE_CL       = new Package("COMMON-LISP");
  PACKAGE_CL_USER  = new Package("COMMON-LISP-USER");
  PACKAGE_KEYWORD  = new Package("KEYWORD");
  PACKAGE_SYS      = new Package("SYSTEM");
  PACKAGE_MOP      = new Package("MOP");
  PACKAGE_EXT      = new Package("EXTENSIONS");
  PACKAGE_TPL      = new Package("TOP-LEVEL");
  PACKAGE_COMPILER = new Package("COMPILER");
}

void initialize_packages_2()
{
  PACKAGE_CL->add_nickname("CL");
  PACKAGE_CL_USER->add_nickname("CL-USER");
  PACKAGE_SYS->add_nickname("SYS");
  PACKAGE_EXT->add_nickname("EXT");
  PACKAGE_TPL->add_nickname("TPL");

  PACKAGE_CL_USER->use_package(PACKAGE_CL);
  PACKAGE_CL_USER->use_package(PACKAGE_MOP);
  PACKAGE_CL_USER->use_package(PACKAGE_EXT);

  PACKAGE_SYS->use_package(PACKAGE_CL);
  PACKAGE_SYS->use_package(PACKAGE_MOP); // REVIEW
  PACKAGE_SYS->use_package(PACKAGE_EXT);

  PACKAGE_MOP->use_package(PACKAGE_CL);
  PACKAGE_MOP->use_package(PACKAGE_SYS);
  PACKAGE_MOP->use_package(PACKAGE_EXT);

  PACKAGE_EXT->use_package(PACKAGE_CL);

  PACKAGE_TPL->use_package(PACKAGE_CL);
  PACKAGE_TPL->use_package(PACKAGE_SYS);
  PACKAGE_TPL->use_package(PACKAGE_EXT);

  PACKAGE_COMPILER->add_nickname("C");
  PACKAGE_COMPILER->use_package(PACKAGE_CL);
  PACKAGE_COMPILER->use_package(PACKAGE_SYS);
  PACKAGE_COMPILER->use_package(PACKAGE_EXT);
}
