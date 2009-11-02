// UnwindProtect.cpp
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
#include "Frame.hpp"
#include "UnwindProtect.hpp"

void UnwindProtect::run_cleanup_code()
{
#ifdef __x86_64__
  __asm__ __volatile__("push %%rbp\n\t"
                       "movq %%rax,%%rbp\n\t"
                       "call *%%rdx\n\t"
                       "pop %%rbp\n\t"
                       : // no output registers
                       : "a"(_rbp), "d"(_code) // input
                       : "rbx","r12","r13","r14","r15","memory" // clobber list
                       );
#else
  __asm__ __volatile__("push %%ebp\n\t"
                       "movl %%eax,%%ebp\n\t"
                       "call *%%edx\n\t"
                       "pop %%ebp\n\t"
                       : // no output registers
                       : "r"(_ebp), "r"(_code) // input
                       : "ebx","esi","edi" // clobber list
                       );
#endif
}

