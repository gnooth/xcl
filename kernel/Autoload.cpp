// Autoload.cpp
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
#include "Package.hpp"
#include "Pathname.hpp"
#include "xcl_home.h"

Autoload::Autoload(Value name)
  : Function(WIDETAG_AUTOLOAD, name), _filename(NULL)
{
}

Autoload::Autoload(Value name, AbstractString * filename)
  : Function(WIDETAG_AUTOLOAD, name), _filename(filename)
{
}

void Autoload::load()
{
  Thread * const thread = current_thread();

//   Pathname * defaults =
//     the_pathname(coerce_to_pathname(thread->symbol_value(S_xcl_home)));
  String * prefix = new String(XCL_HOME);
  prefix->append("/lisp/");
  Pathname * defaults = check_pathname(parse_namestring(prefix));

  Value device = defaults->device();
  Value directory = defaults->directory();
  Value name;
  if (_filename)
    name = make_value(new_simple_string(_filename));
  else
    name = make_value(the_symbol(operator_name())->name()->downcase());
  Value type = make_simple_string("xcl");
  Pathname * pathname = new Pathname(NIL, device, directory, name, type, NIL);
  if (CL_probe_file(make_value(pathname)) == NIL)
    {
      type = make_simple_string("lisp");
      pathname = new Pathname(NIL, device, directory, name, type, NIL);
    }
  AbstractString * namestring = pathname->namestring();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_current_readtable, thread->symbol_value(S_standard_readtable));
  thread->bind_special(S_current_package, make_value(PACKAGE_SYS));
  thread->bind_special(S_read_base, FIXNUM_TEN);
  if (thread->symbol_value(S_autoload_verbose) != NIL)
    {
      Stream * out = check_stream(thread->symbol_value(S_standard_output));
      String * message = new String("; Autoloading ");
      message->append(namestring);
      message->append(" ...\n");
      out->fresh_line();
      out->write_string(message);
      CL_load(make_value(namestring));
      message = new String("; Autoloaded ");
      message->append(namestring);
      message->append("\n");
      out->fresh_line();
      out->write_string(message);
    }
  else
    CL_load(make_value(namestring));
  thread->set_last_special_binding(last_special_binding);
}

Value Autoload::execute()
{
  load();
  return the_symbol(operator_name())->function()->execute();
}

Value Autoload::execute(Value arg)
{
  load();
  return the_symbol(operator_name())->function()->execute(arg);
}

Value Autoload::execute(Value arg1, Value arg2)
{
  load();
  return the_symbol(operator_name())->function()->execute(arg1, arg2);
}

Value Autoload::execute(Value arg1, Value arg2, Value arg3)
{
  load();
  return the_symbol(operator_name())->function()->execute(arg1, arg2, arg3);
}

Value Autoload::execute(Value arg1, Value arg2, Value arg3, Value arg4)
{
  load();
  return the_symbol(operator_name())->function()->execute(arg1, arg2, arg3, arg4);
}

Value Autoload::execute(Value arg1, Value arg2, Value arg3, Value arg4,
                        Value arg5)
{
  load();
  return the_symbol(operator_name())->function()->execute(arg1, arg2, arg3, arg4, arg5);
}

Value Autoload::execute(Value arg1, Value arg2, Value arg3, Value arg4,
                        Value arg5, Value arg6)
{
  load();
  return the_symbol(operator_name())->function()->execute(arg1, arg2, arg3, arg4, arg5,
                                                arg6);
}

Value Autoload::execute(unsigned int numargs, Value args[])
{
  load();
  return the_symbol(operator_name())->function()->execute(numargs, args);
}

AbstractString * Autoload::prin1_to_string()
{
  String * string = new String("#<");
  string->append(the_symbol(S_autoload)->prin1_to_string());
  if (operator_name() != NULL_VALUE)
    {
      string->append_char(' ');
      string->append(::prin1_to_string(operator_name()));
    }
  if (_filename != NULL)
    {
      string->append(" \"");
      string->append(_filename);
      string->append_char('"');
    }
  char buf[256];
  SNPRINTF(buf, sizeof(buf), " {%lX}>", (unsigned long) this);
  string->append(buf);
  return string;
}

// ### autoload
Value EXT_autoload(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      if (listp(args[0]))
        {
          Value list = args[0];
          while (list != NIL)
            {
              Value name = car(list);
              check_symbol(name)->set_autoload(new Autoload(name));
              list = xcdr(list);
            }
          return T;
        }
      else if (symbolp(args[0]))
        {
          the_symbol(args[0])->set_autoload(new Autoload(args[0]));
          return T;
        }
      else
        return signal_type_error(args[0], list3(S_or, S_symbol, S_list));
    case 2:
      if (listp(args[0]))
        {
          AbstractString * filename = check_string(args[1]);
          Value list = args[0];
          while (list != NIL)
            {
              Value name = car(list);
              check_symbol(name)->set_autoload(new Autoload(name, filename));
              list = xcdr(list);
            }
          return T;
        }
      else if (symbolp(args[0]))
        {
          the_symbol(args[0])->set_autoload(new Autoload(args[0], check_string(args[1])));
          return T;
        }
      else
        return signal_type_error(args[0], list3(S_or, S_symbol, S_list));
    default:
      return wrong_number_of_arguments(S_autoload, numargs, 1, 2);
    }
}

// ### autoload-macro
Value EXT_autoload_macro(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 1:
      if (listp(args[0]))
        {
          Value list = args[0];
          while (list != NIL)
            {
              Value name = car(list);
              check_symbol(name)->set_autoload_macro(new Autoload(name));
              list = xcdr(list);
            }
          return T;
        }
      else if (symbolp(args[0]))
        {
          the_symbol(args[0])->set_autoload_macro(new Autoload(args[0]));
          return T;
        }
      else
        return signal_type_error(args[0], list3(S_or, S_symbol, S_list));
    case 2:
      if (listp(args[0]))
        {
          AbstractString * filename = check_string(args[1]);
          Value list = args[0];
          while (list != NIL)
            {
              Value name = car(list);
              check_symbol(name)->set_autoload_macro(new Autoload(name, filename));
              list = xcdr(list);
            }
          return T;
        }
      else if (symbolp(args[0]))
        {
          the_symbol(args[0])->set_autoload_macro(new Autoload(args[0], check_string(args[1])));
          return T;
        }
      else
        return signal_type_error(args[0], list3(S_or, S_symbol, S_list));
    default:
      return wrong_number_of_arguments(S_autoload, numargs, 1, 2);
    }
}

// ### resolve
Value EXT_resolve(Value arg)
{
  Symbol * sym = check_symbol(arg);
  TypedObject * function = sym->function();
  if (function && function->widetag() == WIDETAG_AUTOLOAD)
    reinterpret_cast<Autoload *>(function)->load();
  function = sym->function();
  if (function)
    return make_value(function);
  else
    return NIL;
}

// ### autoloadp
Value EXT_autoloadp(Value symbol)
{
  if (symbolp(symbol))
    {
      TypedObject * function = the_symbol(symbol)->function();
      if (function && function->widetag() == WIDETAG_AUTOLOAD)
        return T;
    }
  return NIL;
}
