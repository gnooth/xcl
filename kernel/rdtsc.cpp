// rdtsc.cpp
//
// Copyright (C) 2011 Peter Graves <gnooth@gmail.com>
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

#include <stdint.h>
#include "lisp.hpp"
#include "primitives.hpp"
#include "runtime.h"

// ### %cpuid
Value SYS_xcpuid()
{
#ifdef __x86_64__
  __asm__ __volatile__("cpuid"
                       ::: "%rax", "%rbx", "%rcx", "%rdx" // clobber list
                       );
#else
  __asm__ __volatile__("cpuid"
                       ::: "%eax", "%ebx", "%ecx", "%edx" // clobber list
                       );
#endif
  return NIL;
}

// ### rdtsc
Value SYS_rdtsc()
{
  uint32_t lo, hi;

// #ifdef __x86_64__
//   __asm__ __volatile__ (//"xorl %%eax,%%eax\n\t"
//                         "cpuid"
//                         ::: "%rax", "%rbx", "%rcx", "%rdx" // clobber list
//                         );
// #else
//   __asm__ __volatile__ (//"xorl %%eax,%%eax\n\t"
//                         "cpuid"
//                         ::: "%eax", "%ebx", "%ecx", "%edx" // clobber list
//                         );
// #endif

  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));

#ifdef __x86_64__
//   return make_unsigned_integer((uint64_t)hi << 32 | lo);
  uint64_t t = ((uint64_t) hi) << 32 | lo;
  return t << FIXNUM_SHIFT;
#else
  if (hi == 0)
    return make_unsigned_integer(lo);
  mpz_t result;
  mpz_init_set_ui(result, hi);
  mpz_mul_2exp(result, result, 32);
  mpz_add_ui(result, result, lo);
  return make_value(new Bignum(result));
#endif
}
