// Restart.cpp
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
#include "primitives.hpp"
#include "StringOutputStream.hpp"

class Restart : public TypedObject
{
private:
  Value _name;
  Value _function;
  Value _report_function;
  Value _interactive_function;
  Value _test_function;

public:
  Restart(Value name, Value function, Value report_function,
          Value interactive_function, Value test_function);

  Value name() const
  {
    return _name;
  }

  Value function() const
  {
    return _function;
  }

  Value report_function() const
  {
    return _report_function;
  }

  Value interactive_function() const
  {
    return _interactive_function;
  }

  Value test_function() const
  {
    return _test_function;
  }

  virtual Value type_of() const
  {
    return S_restart;
  }

  virtual Value class_of() const
  {
    return C_restart;
  }

  virtual bool typep(Value type) const;

  virtual AbstractString * write_to_string();

};

inline bool restart_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_RESTART);
}

inline Restart * the_restart(Value value)
{
  assert(restart_p(value));
  return reinterpret_cast<Restart *>(value - LOWTAG_TYPED_OBJECT);
}

inline Restart * check_restart(Value value)
{
  if (restart_p(value))
    return the_restart(value);
  signal_type_error(value, S_restart);
  // Not reached.
  return NULL;
}

Restart::Restart(Value name, Value function, Value report_function,
                 Value interactive_function, Value test_function)
  : TypedObject(WIDETAG_RESTART), _name(name), _function(function),
    _report_function(report_function), _interactive_function(interactive_function),
    _test_function(test_function)
{
}

bool Restart::typep(Value type) const
{
  return (type == S_restart || type == S_atom || type == T
          || type == C_restart || type == C_t);
}

AbstractString * Restart::write_to_string()
{
  Thread * thread = current_thread();
  if (thread->symbol_value(S_print_escape) != NIL)
    {
      String * s = new String();
      s->append(the_symbol(S_restart)->write_to_string());
      if (_name)
        {
          s->append_char(' ');
          s->append(::write_to_string(_name));
        }
      return unreadable_string(s);
    }
  StringOutputStream * stream = new StringOutputStream(S_character);
  thread->execute(the_symbol(S_print_restart)->function(),
                  make_value(this),
                  make_value(stream));
  AbstractString * s = stream->get_string();
  return s;
}

// ### %make-restart name function report-function interactive_function test_function => restart
Value SYS_make_restart_internal(Value name, Value function, Value report_function,
                                Value interactive_function, Value test_function)
{
  return make_value(new Restart(name, function, report_function, interactive_function,
                                test_function));
}

// ### restart-name restart => name
Value CL_restart_name(Value arg)
{
  return check_restart(arg)->name();
}

// ### restart-function restart => function
Value SYS_restart_function(Value arg)
{
  return check_restart(arg)->function();
}

// ### restart-report-function restart => report-function
Value SYS_restart_report_function(Value arg)
{
  return check_restart(arg)->report_function();
}

// ### restart-interactive-function restart => interactive-function
Value SYS_restart_interactive_function(Value arg)
{
  return check_restart(arg)->interactive_function();
}

// ### restart-test-function restart => test-function
Value SYS_restart_test_function(Value arg)
{
  return check_restart(arg)->test_function();
}
