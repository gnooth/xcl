// cxr.cpp
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

// ### car
Value CL_car(Value arg)
{
  return car(arg);
}

// ### %car
Value SYS_xcar(Value arg)
{
  return xcar(arg);
}

// ### rplaca cons object => cons
Value CL_rplaca(Value cons, Value object)
{
  check_cons(cons)->setcar(object);
  return cons;
}

// ### %rplaca cons object => cons
Value SYS_xrplaca(Value cons, Value object)
{
  the_cons(cons)->setcar(object);
  return cons;
}

// ### setcar cons object => object
Value SYS_setcar(Value cons, Value object)
{
  check_cons(cons)->setcar(object);
  return object;
}

// ### %setcar cons object => object
Value SYS_xsetcar(Value cons, Value object)
{
  the_cons(cons)->setcar(object);
  return object;
}

// ### cdr
Value CL_cdr(Value arg)
{
  return cdr(arg);
}

// ### %cdr
Value SYS_xcdr(Value arg)
{
  return xcdr(arg);
}

// ### rplacd cons object => cons
Value CL_rplacd(Value cons, Value object)
{
  check_cons(cons)->setcdr(object);
  return cons;
}

// ### %rplacd cons object => cons
Value SYS_xrplacd(Value cons, Value object)
{
  the_cons(cons)->setcdr(object);
  return cons;
}

// ### setcdr cons object => object
Value SYS_setcdr(Value cons, Value object)
{
  check_cons(cons)->setcdr(object);
  return object;
}

// ### %setcdr cons object => object
Value SYS_xsetcdr(Value cons, Value object)
{
  the_cons(cons)->setcdr(object);
  return object;
}

// ### caar
Value CL_caar(Value arg)
{
  return car(car(arg));
}

// ### cadr
Value CL_cadr(Value arg)
{
  return car(cdr(arg));
}

// ### %cadr
Value SYS_xcadr(Value arg)
{
  return xcar(xcdr(arg));
}

// ### cdar
Value CL_cdar(Value arg)
{
  return cdr(car(arg));
}

// ### cddr
Value CL_cddr(Value arg)
{
  return cdr(cdr(arg));
}

// ### %cddr
Value SYS_xcddr(Value arg)
{
  return xcdr(xcdr(arg));
}

// ### caddr
Value CL_caddr(Value arg)
{
  return car(cdr(cdr(arg)));
}

// ### %caddr
Value SYS_xcaddr(Value arg)
{
  return xcar(xcdr(xcdr(arg)));
}

// ### caadr
Value CL_caadr(Value arg)
{
  return car(car(cdr(arg)));
}

// ### caaar
Value CL_caaar(Value arg)
{
  return car(car(car(arg)));
}

// ### cdaar
Value CL_cdaar(Value arg)
{
  return cdr(car(car(arg)));
}

// ### cddar
Value CL_cddar(Value arg)
{
  return cdr(cdr(car(arg)));
}

// ### cdddr
Value CL_cdddr(Value arg)
{
  return cdr(cdr(cdr(arg)));
}

// ### cadar
Value CL_cadar(Value arg)
{
  return car(cdr(car(arg)));
}

// ### cdadr
Value CL_cdadr(Value arg)
{
  return cdr(car(cdr(arg)));
}

// ### first
Value CL_first(Value arg)
{
  return car(arg);
}

// ### second
Value CL_second(Value arg)
{
  return car(cdr(arg));
}

// ### third
Value CL_third(Value arg)
{
  return car(cdr(cdr(arg)));
}

// ### fourth
Value CL_fourth(Value arg)
{
  return car(cdr(cdr(cdr(arg))));
}

// ### rest
Value CL_rest(Value arg)
{
  return cdr(arg);
}
