// platform.hpp
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

#ifndef __PLATFORM_HPP
#define __PLATFORM_HPP

#include <stddef.h>
#include <cstdio>
#include <string.h>
#include <assert.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#if defined(WIN32) && defined(_MSC_VER)

#define O_RDONLY _O_RDONLY

inline int OPEN(const char * filename, int flags)
{
#pragma warning(push)
#pragma warning(disable : 4996)
  return _open(filename, flags);
#pragma warning(pop)
}

inline int READ(int fd, void * buf, size_t count)
{
  return _read(fd, buf, count);
}

inline int WRITE(int fd, const void *buf, size_t count)
{
  return _write(fd, buf, count);
}

inline int CLOSE(int fd)
{
  return _close(fd);
}

inline int FILENO(FILE * stream)
{
  return _fileno(stream);
}

#define SNPRINTF ::sprintf_s

#else

inline int OPEN(const char * filename, int flags)
{
  return open(filename, flags);
}

inline int OPEN(const char * filename, int flags, int mode)
{
  return open(filename, flags, mode);
}

inline ssize_t READ(int fd, void * buf, size_t count)
{
  return read(fd, buf, count);
}

inline ssize_t WRITE(int fd, const void *buf, size_t count)
{
  return write(fd, buf, count);
}

inline long CLOSE(int fd)
{
  return close(fd);
}

inline long FILENO(FILE * stream)
{
  return fileno(stream);
}

#define SNPRINTF snprintf

#endif

#endif // platform.hpp
