// Cons.hpp
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

#ifndef __CONS_HPP
#define __CONS_HPP

class Cons : public LispObject
{
  friend Value make_cons(Value car, Value cdr);
  friend Value make_cons(Value car);

private:
  static unsigned long count;

  Value _car;
  Value _cdr;

  Cons(Value car, Value cdr) : _car(car), _cdr(cdr)
  {
    ++count;
  }

  Cons(Value car) : _car(car), _cdr(NIL)
  {
    ++count;
  }

public:
  static unsigned long get_count() { return count; }
  static void reset_count() { count = 0; }

  void setcar(Value car) { _car = car; }
  void setcdr(Value cdr) { _cdr = cdr; }
  Value xcar() { return _car; }
  Value xcdr() { return _cdr; }

  Value reverse();
  Value nreverse();

  AbstractString * write_to_string();
};

inline Value make_value(Cons * cons)
{
  return make_value(cons, LOWTAG_CONS);
}

inline bool consp(Value value)
{
  return (value & LOWTAG_MASK) == LOWTAG_CONS;
}

inline Cons * the_cons(Value value)
{
  assert(consp(value));
  return (Cons *) (value - LOWTAG_CONS);
}

inline Cons * check_cons(Value value)
{
  if (consp(value))
    return the_cons(value);
  signal_type_error(value, S_cons);
  // Not reached.
  return NULL;
}

inline Value make_cons(Value car, Value cdr)
{
  return make_value(new Cons(car, cdr), LOWTAG_CONS);
}

inline Value make_cons(Value car)
{
  return make_value(new Cons(car, NIL), LOWTAG_CONS);
}

#endif
