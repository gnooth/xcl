// ServerSocket.hpp
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

#ifndef __SERVER_SOCKET_HPP
#define __SERVER_SOCKET_HPP

class ServerSocket : public TypedObject
{
private:
  int _fd;
  
public:
  ServerSocket(int fd) : TypedObject(WIDETAG_SERVER_SOCKET), _fd(fd)
  {
  }
  
  int fd() const
  {
    return _fd;
  }
  
  virtual Value type_of() const
  {
    return S_server_socket;
  }

  virtual Value class_of() const
  {
    return C_server_socket;
  }
  
  unsigned short local_port();
};

inline bool server_socket_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SERVER_SOCKET);
}

inline ServerSocket * the_server_socket(Value value)
{
  assert(server_socket_p(value));
  return reinterpret_cast<ServerSocket *>(value - LOWTAG_TYPED_OBJECT);
}

inline ServerSocket * check_server_socket(Value value)
{
  if (server_socket_p(value))
    return the_server_socket(value);
  signal_type_error(value, S_server_socket);
  // Not reached.
  return NULL;
}

#endif // ServerSocket.hpp
