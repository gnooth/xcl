// symbols.hpp
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

#ifndef __SYMBOLS_HPP
#define __SYMBOLS_HPP

#ifdef MAIN
#define EXTERN
#else
#define EXTERN extern
#endif

#define DEFINE_SYMBOL(identifier, package, name) \
  EXTERN Value S_ ## identifier

#define DEFINE_KEYWORD(identifier, name) \
  EXTERN Value K_ ## identifier

#include "symdefs.hpp"

#undef DEFINE_SYMBOL
#undef DEFINE_KEYWORD

#endif // symbols.hpp
