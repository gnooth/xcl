// runtime.h
//
// Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

#ifndef __RUNTIME_H
#define __RUNTIME_H

#include "Value.h"

#ifdef	__cplusplus
extern "C" {
#endif

  extern Value RT_current_thread_call_symbol_0(Value symbol);
  extern Value RT_current_thread_call_symbol_1(Value symbol, Value arg);
  extern Value RT_current_thread_call_symbol_2(Value symbol, Value arg1, Value arg2);
  extern Value RT_current_thread_call_symbol_3(Value symbol, Value arg1, Value arg2, Value arg3);
  extern Value RT_current_thread_call_symbol_4(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4);
  extern Value RT_current_thread_call_symbol_5(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);
  extern Value RT_current_thread_call_symbol_6(Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6);

  extern Value RT_current_thread_call_symbol(Value symbol, unsigned int numargs, Value args[]);

  extern Value RT_thread_call_symbol_0(Thread * thread, Value symbol);
  extern Value RT_thread_call_symbol_1(Thread * thread, Value symbol, Value arg);
  extern Value RT_thread_call_symbol_2(Thread * thread, Value symbol, Value arg1, Value arg2);
  extern Value RT_thread_call_symbol_3(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3);
  extern Value RT_thread_call_symbol_4(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4);
  extern Value RT_thread_call_symbol_5(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);
  extern Value RT_thread_call_symbol_6(Thread * thread, Value symbol, Value arg1, Value arg2, Value arg3, Value arg4, Value arg5, Value arg6);

  extern Value RT_thread_call_symbol(Thread * thread, Value symbol, unsigned int numargs, Value args[]);

  extern void RT_handle_interrupt();

  extern jmp_buf * RT_frame_jmp(Frame * frame);

  extern Block * RT_enter_block(Thread * thread, Value block_name);
  extern void RT_leave_block(Thread * thread, Block * block);
  extern Value RT_block_non_local_return(Thread * thread, Block * block);
  extern void RT_return_from(Thread * thread, Value block_name, Value result) __attribute__ ((noreturn));

  extern Catch * RT_enter_catch(Thread * thread, Value tag);
  extern void RT_leave_catch(Thread * thread, Catch * catch_frame);
  extern Value RT_caught_throw(Thread * thread, Catch * catch_frame);
  extern void RT_throw(Thread * thread, Value tag, Value result) __attribute__ ((noreturn));

  extern Tagbody * RT_add_tagbody(Thread * thread);
  extern void RT_add_tag(Thread * thread, Value tag_name, Tagbody * tagbody, int index);
  extern void RT_leave_tagbody(Thread * thread, Tagbody * tagbody);
  extern void RT_non_local_go(Thread * thread, Value tag_name);

  extern Values * RT_thread_copy_values(Thread * thread, Value primary_value);
  extern Value RT_thread_set_values(Thread * thread, Values * values);
  extern UnwindProtect* RT_enter_unwind_protect(Thread * thread, void * code, long rbp);
  extern void RT_leave_unwind_protect(Thread * thread, UnwindProtect * uwp);

  extern void RT_unwind_to(Frame * frame, Thread * thread);

  extern void RT_progv_bind_vars(Thread * thread, Value symbols, Value values);

  extern Value RT_gethash2(Thread * thread, Value arg1, Value arg2);
  extern Value RT_gethash3(Thread * thread, Value arg1, Value arg2, Value arg3);

  extern int RT_thread_uwp_in_cleanup_p(Thread * thread, UnwindProtect * uwp);

#ifdef	__cplusplus
}
#endif

#endif
