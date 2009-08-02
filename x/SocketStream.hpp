// SocketStream.hpp
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

#ifndef __SOCKET_STREAM_HPP
#define __SOCKET_STREAM_HPP

class SocketStream : public Stream
{
public:
  SocketStream(int fd)
    : Stream(WIDETAG_SOCKET_STREAM, DIRECTION_IO, fd)
  {
  }

  virtual Value type_of() const
  {
    return S_socket_stream;
  }

  virtual Value class_of() const
  {
    return C_socket_stream;
  }

  virtual bool typep(Value type) const;

#ifdef WIN32
  virtual int read_char()
  {
    if (_last_char >= 0)
      {
        BASE_CHAR c2 = (BASE_CHAR) _last_char;
        _last_char = -1;
        return c2;
      }
    BASE_CHAR c;
    int n = recv(_fd, (char *) &c, 1, 0);
//     if (c == '\r')
//       n = recv(_socket->fd(), &c, 1, 0);
//     return n <= 0 ? -1 : c;
    if (n == 0)
      return -1;
    else
      return c;
  }

  virtual void write_char(BASE_CHAR c)
  {
    if (send(_fd, (char *) &c, 1, 0) != 1)
      {
        printf("WSAGetLastError() = %d\n", WSAGetLastError());
        fflush(stdout);
        signal_lisp_error("send() error\n");
      }
    if (c == '\n')
      _charpos = 0;
    else
      ++_charpos;
  }
#endif

  virtual Value close()
  {
#ifdef WIN32
    ::closesocket(_fd);
#else
    ::close(_fd);
#endif
    _open = false;
    return T;
  }
};

#endif // SocketStream.hpp
