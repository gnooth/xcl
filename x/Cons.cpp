// Cons.cpp
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

unsigned long Cons::count = 0;

Value Cons::reverse()
{
  Cons * cons = this;
  Value result = make_cons(cons->xcar());
  while (consp(cons->xcdr()))
    {
      cons = the_cons(cons->xcdr());
      result = make_cons(cons->xcar(), result);
    }
  if (cons->xcdr() != NIL)
    return signal_type_error(cons->xcdr(), S_list);
  return result;
}

Value Cons::nreverse()
{
  if (consp(_cdr))
    {
      Cons * cons = the_cons(_cdr);
      if (consp(cons->_cdr))
        {
          Cons * cons1 = cons;
          Value list = NIL;
          do
            {
              Cons * temp = the_cons(cons->_cdr);
              cons->_cdr = list;
              list = make_value(cons);
              cons = temp;
            }
          while (consp(cons->_cdr));
          if (cons->_cdr != NIL)
            return signal_type_error(cons->_cdr, S_list);
          _cdr = list;
          cons1->_cdr = make_value(cons);
        }
      else if (cons->_cdr != NIL)
        return signal_type_error(cons->_cdr, S_list);
      Value temp = _car;
      _car = cons->_car;
      cons->_car = temp;
    }
  else if (_cdr != NIL)
    return signal_type_error(_cdr, S_list);
  return make_value(this);
}

AbstractString * Cons::write_to_string()
{
  Thread * thread = current_thread();
  Value print_length = thread->symbol_value(S_print_length);
  INDEX max_length;
  if (print_length != NIL)
    max_length = check_index(print_length);
  else
    max_length = MOST_POSITIVE_FIXNUM;
  Value print_level = thread->symbol_value(S_print_level);
  INDEX max_level;
  if (print_level != NIL)
    max_level = check_index(print_level);
  else
    max_level = MOST_POSITIVE_FIXNUM;
  String * s = new String();
  if (_car == S_quote)
    {
      if (consp(_cdr))
        {
          // Not a dotted list.
          if (the_cons(_cdr)->_cdr == NIL)
            {
              s->append_char('\'');
              s->append(::write_to_string(the_cons(_cdr)->_car));
              return s;
            }
        }
    }
  if (_car == S_function)
    {
      if (consp(_cdr))
        {
          // Not a dotted list.
          if (the_cons(_cdr)->_cdr == NIL)
            {
              s->append("#'");
              s->append(::write_to_string(the_cons(_cdr)->_car));
              return s;
            }
        }
    }
  Value current_print_level = thread->symbol_value(S_current_print_level);
  INDEX current_level = check_index(current_print_level);
  if (current_level < max_level)
    {
      void * last_special_binding = thread->last_special_binding();
      thread->bind_special(S_current_print_level, make_number(current_level + 1));
      INDEX count = 0;
      bool truncated = false;
      s->append_char('(');
      if (count < max_length)
        {
          Cons * p = this;
          s->append(::write_to_string(_car));
          ++count;
          while (consp(p->_cdr))
            {
              p = the_cons(p->_cdr);
              s->append_char(' ');
              if (count < max_length)
                {
                  s->append(::write_to_string(p->_car));
                  ++count;
                }
              else
                {
                  truncated = true;
                  break;
                }
            }
          if (!truncated && p->_cdr != NIL)
            {
              s->append(" . ");
              s->append(::write_to_string(p->_cdr));
            }
        }
      else
        truncated = true;
      if (truncated)
        s->append("...");
      s->append_char(')');
      thread->set_last_special_binding(last_special_binding);
    }
  else
    s->append_char('#');
  return s;
}
