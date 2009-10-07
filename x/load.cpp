// load.cpp
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
#include "runtime.h"
#include "primitives.hpp"
#include "FileError.hpp"
#include "FileStream.hpp"
#include "Pathname.hpp"
#include "FaslReadtable.hpp"
#include "xcl_home.h"

// ### load-stream
Value SYS_load_stream(Value streamarg, Value filespec, Value verbose, Value print)
{
  double start = uptime();
  Stream * stream = check_stream(streamarg);
  Thread * thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S__load_stream_, streamarg);
  // "LOAD binds *READTABLE* and *PACKAGE* to the values they held before
  // loading the file."
  thread->bind_special(S_current_readtable, thread->symbol_value(S_current_readtable));
  thread->bind_special(S_current_package, thread->symbol_value(S_current_package));
  Stream * out = NULL;
  if (verbose != NIL)
    {
      String * message = new String("; Loading ");
      if (pathnamep(filespec))
        message->append(the_pathname(filespec)->namestring());
      else
        message->append(::prin1_to_string(streamarg));
      message->append(" ...\n");
      out = check_stream(thread->symbol_value(S_standard_output));
      out->fresh_line();
      out->write_string(message);
    }
  Value eof = streamarg;
  Function * eval = (Function *)the_symbol(S_eval)->function();
  while (true)
    {
      thread->set_symbol_value(S_source_position, stream->file_position());
      Readtable * rt = check_readtable(thread->symbol_value(S_current_readtable));
      Value obj = stream->read(false, eof, false, thread, rt);
      if (obj == eof)
        break;
      Value result = thread->execute(eval, obj);
      if (print != NIL)
        {
          Stream * out = check_stream(thread->symbol_value(S_standard_output));
          out->write_string(::prin1_to_string(result));
          out->write_char('\n');
        }
    }
  thread->set_last_special_binding(last_special_binding);
  if (verbose != NIL)
    {
      String * message = new String("; Loaded ");
      if (pathnamep(filespec))
        message->append(the_pathname(filespec)->namestring());
      else
        message->append(::prin1_to_string(streamarg));
      message->append(" (");
      double elapsed = uptime() - start;
      if (elapsed < 0.001)
        elapsed = 0;
      message->append((new SingleFloat(elapsed))->write_to_string());
      message->append(" seconds)\n");
      out->fresh_line();
      out->write_string(message);
    }
  thread->clear_values();
  return T;
}

// ### load
// preliminary version (redefined in load.lisp)
Value CL_load(Value arg)
{
  Thread * const thread = current_thread();
  AbstractString * namestring;
  Pathname * pathname = the_pathname(coerce_to_pathname(arg));
  Value dir = pathname->directory();
  if (consp(dir) && xcar(dir) == K_absolute)
    namestring = pathname->namestring();
  else
    {
      Pathname * defaults =
        the_pathname(coerce_to_pathname(thread->symbol_value(S_default_pathname_defaults)));
      pathname = merge_pathnames(pathname, defaults, K_newest);
      namestring = pathname->namestring();
    }
  FileStream * stream = new FileStream(make_value(pathname),
                                       make_value(namestring),
                                       S_character,
                                       K_input,
                                       NIL);

  void * last_special_binding = thread->last_special_binding();
  Value pathname_value = make_value(pathname);
  thread->bind_special(S_load_pathname, pathname_value);
  thread->bind_special(S_load_truename, pathname_value);
  thread->bind_special(S_source_file, pathname_value);
  thread->bind_special(S_source_position, FIXNUM_ZERO);
  Value result = SYS_load_stream(make_value(stream),
                                 make_value(pathname),
                                 thread->symbol_value(S_load_verbose),
                                 thread->symbol_value(S_load_print));
  thread->set_last_special_binding(last_special_binding);
  // In case of error, we don't get here and so might leak a file descriptor.
  // This situation is handled correctly by the final version in load.lisp.
  stream->close();
  return result;
}

// ### load-system-file
Value SYS_load_system_file(Value arg)
{
  Thread * thread = current_thread();
  Pathname * defaults =
    the_pathname(coerce_to_pathname(thread->symbol_value(S_xcl_home)));
  Pathname * pathname =
    merge_pathnames(the_pathname(coerce_to_pathname(arg)), defaults, K_newest);
//   printf("load-system-file %s\n", pathname->namestring()->as_c_string());
  if (pathname->type() == NIL)
    {
      pathname = new Pathname(NIL,
                              pathname->device(),
                              pathname->directory(),
                              pathname->name(),
                              make_simple_string("xcl"),
                              NIL);
      if (CL_probe_file(make_value(pathname)) == NIL)
        pathname = new Pathname(NIL,
                                pathname->device(),
                                pathname->directory(),
                                pathname->name(),
                                make_simple_string("lisp"),
                                NIL);
    }
  return RT_thread_call_symbol_1(thread, S_load, make_value(pathname));
}

// ### maybe-load-system-file
Value SYS_maybe_load_system_file(Value arg)
{
  Thread * thread = current_thread();
  Pathname * defaults =
    the_pathname(coerce_to_pathname(thread->symbol_value(S_xcl_home)));
  Pathname * pathname =
    merge_pathnames(the_pathname(coerce_to_pathname(arg)), defaults, K_newest);
  if (CL_probe_file(make_value(pathname)) != NIL)
    return RT_thread_call_symbol_1(thread, S_load, make_value(pathname));
  return NIL;
}

Value fasl_load_stream(Thread * thread)
{
  Value stream = thread->symbol_value(S__load_stream_);
  Stream * in = check_stream(stream);
  Value eof = stream;
  Function * eval = (Function *)the_symbol(S_eval)->function();
  while (true)
    {
      Value obj = in->read(false, eof, false, thread, FASL_READTABLE);
      if (obj == eof)
        break;
      thread->execute(eval, obj);
    }
  return T;
}

// ### init-fasl fasl-version => T
Value SYS_init_fasl(Value arg)
{
  // FIXME check version

  Thread * thread = current_thread();
  // this binding gets undone when LOAD returns
  thread->bind_special(S_fasl_anonymous_package, NIL);
  return fasl_load_stream(thread);
}
