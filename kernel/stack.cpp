// stack.cpp
//
// Copyright (C) 2010 Peter Graves <gnooth@gmail.com>
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

unsigned long * stack_top;

unsigned long * __attribute__ ((noinline)) current_sp()
{
  unsigned long * sp;

#ifdef __x86_64__
  asm volatile ("movq %%rsp,%0" : "=g" (sp));
#else
  asm volatile ("movl %%esp,%0" : "=g" (sp));
#endif

  return (unsigned long *) (((unsigned long) sp) + sizeof(unsigned long));
}

// ### current-stack-as-list
Value SYS_current_stack_as_list()
{
  unsigned long * sp = current_sp();
  Value list = NIL;
  while (sp <= stack_top)
    {
      Value val = make_number(*sp);
      list = make_cons(val, list);
      sp = (unsigned long *) (((unsigned long) sp) + sizeof(unsigned long));
    }
  return list;
}
