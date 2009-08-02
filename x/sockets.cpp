// sockets.cpp
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
#include <winsock2.h>
#else
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include "lisp.hpp"
#include "primitives.hpp"
#include "ServerSocket.hpp"
#include "SocketStream.hpp"

// ### %make-socket address-family type connect local-host local-port remote-host remote-port
// => socket-stream or socket
Value SYS_make_socket_internal(unsigned int numargs, Value args[])
{
  if (numargs != 7)
    return wrong_number_of_arguments(S_make_socket_internal, numargs, 7, 7);
  // address-family
  if (args[0] != K_internet)
    return signal_lisp_error("Unsupported address family.");
  // type
  if (args[1] != K_stream)
    return signal_lisp_error("Unsupported socket type.");
#ifdef WIN32
  WSADATA wsaData;
  if (WSAStartup(MAKEWORD(2, 2), &wsaData ) != 0)
    return signal_lisp_error("WSAStartup() error");
  // FIXME we need to call WSACleanup() somewhere
#endif
  // connect
  if (args[2] == K_active)
    {
      struct hostent * host = gethostbyname(check_string(args[5])->copy_to_c_string());
      if (host == NULL)
        {
#ifdef WIN32
          printf("WSAGetLastError() = %d\n", WSAGetLastError());
          fflush(stdout);
#endif
          return signal_lisp_error("Error looking up host.");
        }
      int fd = socket(PF_INET, SOCK_STREAM, 0);
      if (fd < 0)
        return signal_lisp_error("Unable to create socket.");
      struct sockaddr_in address;
      address.sin_family = AF_INET;
      address.sin_port = htons(fixnum_value(args[6]));
      memcpy(&address.sin_addr, host->h_addr_list[0], sizeof(address.sin_addr));
      if (connect(fd, (struct sockaddr *) &address, sizeof(address)) != 0)
        return signal_lisp_error("Unable to connect.");
      return make_value(new SocketStream(fd));
    }
#ifdef WIN32
  if (args[2] == K_passive)
    {
      int fd = socket(PF_INET, SOCK_STREAM, 0);
      if (fd < 0)
        return signal_lisp_error("Unable to create socket.");
      int i = 1;
      setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (const char *) &i, sizeof(i));
      struct sockaddr_in address;
      address.sin_family = AF_INET;
      address.sin_port = htons(fixnum_value(args[4]));
      memset(&address.sin_addr, 0, sizeof(address.sin_addr));
      if (bind(fd, (struct sockaddr *) &address, sizeof(address)))
        return signal_lisp_error("Unable to bind.");
      int addr_length = sizeof(struct sockaddr_in);
      getsockname(fd, (struct sockaddr *) &address, &addr_length);
      if (listen(fd, 5))
        return signal_lisp_error("Unable to listen.");
      return make_value(new ServerSocket(fd));
    }
#else
  if (args[2] == K_passive)
    {
      int fd = socket(PF_INET, SOCK_STREAM, 0);
      if (fd < 0)
        return signal_lisp_error("Unable to create socket.");
      int i = 1;
      setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &i, sizeof(i));
      struct sockaddr_in address;
      address.sin_family = AF_INET;
      address.sin_port = htons(fixnum_value(args[4]));
      memset(&address.sin_addr, 0, sizeof(address.sin_addr));
      if (bind(fd, (struct sockaddr *) &address, sizeof(address)))
        return signal_lisp_error("Unable to bind.");
      socklen_t addr_length = sizeof(struct sockaddr_in);
      getsockname(fd, (struct sockaddr *) &address, &addr_length);
      if (listen(fd, 5))
        return signal_lisp_error("Unable to listen.");
      return make_value(new ServerSocket(fd));
    }
#endif
  return signal_lisp_error("Not implemented.");
}

// ### accept-connection socket => socket-stream
Value EXT_accept_connection(Value arg)
{
#ifdef WIN32
//   return signal_lisp_error("Not implemented.");
  struct sockaddr_in address;
  memset(&address, 0, sizeof(struct sockaddr_in));
  int addr_length = sizeof(struct sockaddr_in);
  ServerSocket * sock = check_server_socket(arg);
  int fd = accept(sock->fd(), (struct sockaddr *) &address, &addr_length);
  if (fd < 0)
    return signal_lisp_error("Unable to accept connection.");
  return make_value(new SocketStream(fd));
#else
  struct sockaddr_in address;
  memset(&address, 0, sizeof(struct sockaddr_in));
  socklen_t addr_length = sizeof(struct sockaddr_in);
  ServerSocket * sock = check_server_socket(arg);
  int fd = accept(sock->fd(), (struct sockaddr *) &address, &addr_length);
  if (fd < 0)
    return signal_lisp_error("Unable to accept connection.");
  return make_value(new SocketStream(fd));
#endif
}

// ### local-port socket => port
Value EXT_local_port(Value arg)
{
  return make_fixnum(check_server_socket(arg)->local_port());
}
