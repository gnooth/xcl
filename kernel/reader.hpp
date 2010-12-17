// reader.hpp
//
// Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

#ifndef __READER_HPP
#define __READER_HPP

extern Value stream_read(Value streamarg, bool eof_error_p,
                         Value eof_value, bool recursive_p,
                         Thread * thread, Readtable * rt);

extern Value stream_read_preserving_whitespace(Value streamarg, bool eof_error_p,
                                               Value eof_value, bool recursive_p,
                                               Thread * thread, Readtable * rt);

extern Value stream_read_list(Value streamarg, bool require_proper_list,
                              Thread * thread, Readtable * rt);

extern Value stream_read_vector(Value streamarg, INDEX size,
                                Thread * thread, Readtable * rt);

extern Value stream_read_atom(Value streamarg, BASE_CHAR c1,
                              Readtable * rt, Thread * thread);

extern Value stream_read_symbol(Value streamarg, Readtable * rt);

extern Value stream_read_delimited_list(Value streamarg, BASE_CHAR delimiter,
                                        Thread * thread);

extern Value stream_read_string(Value streamarg, BASE_CHAR terminator,
                                Readtable * rt);

extern Value stream_read_dispatch_char(Value streamarg, BASE_CHAR dispatch_char,
                                       Thread * thread, Readtable * rt);

#endif // reader.hpp
