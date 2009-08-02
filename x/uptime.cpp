// uptime.cpp
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

#ifdef WIN32
// #include <windows.h>
// #include <time.h>
#else
#include <sys/time.h>
#endif

#include "lisp.hpp"

static double start_time; // in seconds

void initialize_uptime()
{
#ifdef WIN32
  clock_t now = clock();
  start_time = (double) now / CLOCKS_PER_SEC;
#else
  struct timeval now;
  gettimeofday(&now, (struct timezone *) 0);
  start_time = (now.tv_sec * 1000000L + now.tv_usec) / (double) 1000000;
#endif
}

// Returns uptime in seconds.
double uptime()
{
#ifdef WIN32
  clock_t now = clock();
  double current_time = (double) now / CLOCKS_PER_SEC;
#else
  struct timeval now;
  gettimeofday(&now, (struct timezone *) 0);
  double current_time = (now.tv_sec * 1000000L + now.tv_usec) / (double) 1000000;
#endif
  return current_time - start_time;
}

String * uptime_as_string()
{
  String * s = new String((new SingleFloat(uptime()))->write_to_string());
  s->append(" seconds");
  return s;
}
