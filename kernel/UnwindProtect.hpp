// UnwindProtect.hpp
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

#ifndef __UNWIND_PROTECT_CONTEXT_HPP
#define __UNWIND_PROTECT_CONTEXT_HPP

class UnwindProtect : public Frame
{
private:
  Value _cleanup_forms;
  Environment * _env;
  void * _code;
#ifdef __x86_64__
  long _rbp;
#else
  int _ebp;
#endif

public:
  UnwindProtect(Value cleanup_forms, Environment * env, Frame * next)
    : Frame(UNWIND_PROTECT, next), _cleanup_forms(cleanup_forms), _env(env)
  {
  }

#ifdef __x86_64__
  UnwindProtect(void * code, long rbp, Frame * next)
    : Frame(UNWIND_PROTECT, next), _code(code), _rbp(rbp)
  {
  }
#else
  UnwindProtect(void * code, int ebp, Frame * next)
    : Frame(UNWIND_PROTECT, next), _code(code), _ebp(ebp)
  {
  }
#endif

  void * code()
  {
    return _code;
  }

#ifdef __x86_64__
  long rbp()
  {
    return _rbp;
  }
#else
  int ebp()
  {
    return _ebp;
  }
#endif

  void run_cleanup_forms(Thread * thread)
  {
    while (_cleanup_forms != NIL)
      {
        // We don't ever want to run the same cleanup form more than once, so
        // we remove the form from the list before evaluating it (MISC.273).
        Value form = car(_cleanup_forms);
        _cleanup_forms = xcdr(_cleanup_forms);
        eval(form, _env, thread);
      }
  }
};

#endif // UnwindProtect.hpp
