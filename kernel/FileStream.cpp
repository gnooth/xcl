// FileStream.cpp
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

#include <errno.h>

#ifndef WIN32
#include <sys/mman.h>
#endif

#include "lisp.hpp"
#include "runtime.h"
#include "primitives.hpp"
#include "FileError.hpp"
#include "Pathname.hpp"
#include "FileStream.hpp"

FileStream::FileStream(Value pathname, Value namestring, Value element_type,
                       Value direction, Value if_exists)
  : AnsiStream(WIDETAG_FILE_STREAM), _pathname(pathname), _output_buffer(NULL),
    _output_buffer_size(0), _output_buffer_offset(0)
{
#ifdef WIN32
  _hfile = INVALID_HANDLE_VALUE;
  _hmapping = NULL;
#endif

  const char * filename = check_string(namestring)->as_c_string();

  _element_type = element_type;

  if (element_type == S_character || element_type == S_base_char)
    _bytes_per_unit = 1;
  else if (consp(element_type))
    {
      Value type_specifier_atom = xcar(element_type);
      if (type_specifier_atom == S_unsigned_byte || type_specifier_atom == S_signed_byte)
        {
          unsigned long numbits = check_index(car(xcdr(element_type)));
          _bytes_per_unit = numbits / 8;
        }
    }
  else
    ; // FIXME

  if (direction == K_input)
    _direction = DIRECTION_INPUT;
  else if (direction == K_output)
    _direction = DIRECTION_OUTPUT;
  else if (direction == K_io)
    _direction = DIRECTION_IO;

  if (direction == K_input || direction == K_io)
    {
      if (::equal(_element_type, UB8_TYPE))
        _read_byte_function = S_read_8_bits;
    }

#ifdef WIN32
  if (direction == K_input && _bytes_per_unit == 1)
    {
      _hfile = CreateFile(filename,
                          GENERIC_READ,
                          FILE_SHARE_READ,
                          NULL, // default security descriptor
                          OPEN_EXISTING,
                          FILE_ATTRIBUTE_NORMAL,
                          NULL // template file (ignored for existing file)
                          );
      if (_hfile != INVALID_HANDLE_VALUE)
        {
          // "An attempt to map a file with a length of 0 (zero) fails with an
          // error code of ERROR_FILE_INVALID. Applications should test for
          // files with a length of 0 (zero) and reject those files."
          DWORD size = GetFileSize(_hfile, NULL);
          if (size != INVALID_FILE_SIZE || size != 0)
            {
              _hmapping = CreateFileMapping(_hfile,
                                            NULL, // default security descriptor
                                            PAGE_READONLY,
                                            0, 0, // use current size of file
                                            NULL);
              if (_hmapping != NULL)
                {
                  _data = MapViewOfFile(_hmapping,
                                        FILE_MAP_READ,
                                        0,
                                        0,
                                        size);
                  if (_data == NULL)
                    {
                      printf("MapViewOfFile failed: %s\n", filename);
                      fflush(stdout);
                    }
                }
              else
                {
                  printf("CreateFileMapping failed\n");
                  fflush(stdout);
                }
            }
          if (_data != NULL)
            {
              // success!
              _data_size = size;
              return;
            }
          else
            {
              // clean up
              if (_hmapping != NULL)
                {
                  CloseHandle(_hmapping);
                  _hmapping = NULL;
                }
              _data_size = 0;
              CloseHandle(_hfile);
              _hfile = INVALID_HANDLE_VALUE;
            }
        }
    }
#endif

  if (direction == K_input)
    {
      int flags = O_RDONLY;
#ifdef WIN32
      flags |= _O_BINARY;
#endif
      _fd = OPEN(filename, flags);
    }
  else if (direction == K_output || direction == K_io)
    {
      int flags = O_CREAT;
      if (direction == K_output)
        flags |= O_WRONLY;
      else
        flags |= O_RDWR;
      if (if_exists == K_supersede || if_exists == K_new_version)
        flags |= O_TRUNC;               // REVIEW
#ifdef WIN32
      flags |= _O_BINARY;
#endif
      _fd = OPEN(filename, flags, 0644); // REVIEW mode
    }
  else
    {
      signal_lisp_error("Direction must be :INPUT, :OUTPUT, or :IO.");
      // Not reached.
      return;
    }

  if (_fd < 0)
    {
      String * string = new String("Unable to open ");
      string->append(::prin1_to_string(pathname));
      signal_lisp_error(new FileError(string, pathname));
      // Not reached.
      return;
    }

#ifndef WIN32
  if (direction == K_input && _bytes_per_unit == 1)
    {
      struct stat statbuf;
      if (fstat(_fd, &statbuf) == 0)
        {
          _data_size = statbuf.st_size;
          _data = mmap(NULL, _data_size, PROT_READ, MAP_PRIVATE, _fd, 0);
          if (_data == MAP_FAILED)
            {
              _data = NULL;
              _data_size = 0;
            }
        }
    }
#endif

  if (direction == K_output || direction == K_io)
    {
      if (if_exists == K_append)
        {
          // "Output operations on the stream destructively modify the existing
          // file. The file pointer is initially positioned at the end of the
          // file."
          if (lseek(_fd, 0, SEEK_END) < 0)
            {
              // get errno before calling close()
              String * string = new String("lseek error ");
              string->append_long(errno);
              close();
              signal_lisp_error(new FileError(string, pathname));
            }
        }
    }

  if (direction == K_output && _bytes_per_unit == 1 && if_exists != K_append)
    {
      const unsigned int size = 4096;
      _output_buffer = (BYTE *) GC_malloc_atomic_ignore_off_page(size);
      _output_buffer_size = size;
      _output_buffer_offset = 0;
    }
}

Value FileStream::type_of() const
{
  return S_file_stream;
}

Value FileStream::class_of() const
{
  return C_file_stream;
}

bool FileStream::typep(Value type) const
{
  return (type == S_file_stream || type == S_stream || type == S_atom || type == T
          || type == C_file_stream || type == C_stream || type == C_t);
}

int FileStream::read_char()
{
  BASE_CHAR c;
  if (_last_char >= 0)
    {
      c = (BASE_CHAR) _last_char;
      _last_char = -1;
      return c;
    }
  if (_data)
    {
      if (_data_offset < _data_size)
        {
          c = *(((BASE_CHAR *)_data) + _data_offset);
          ++_data_offset;
          return c;
        }
      else
        return -1;
    }
#ifdef WIN32
 top:
#endif
  int n = READ(_fd, &c, 1);
#ifdef WIN32
  if (c == '\r' && n == 1)
    n = READ(_fd, &c, 1);
  if (n == 0)
    {
      if (GetLastError() == ERROR_OPERATION_ABORTED)
        {
          SetLastError(0);
          if (interrupted)
            RT_handle_interrupt();
          goto top;
        }
    }
#endif
  return n <= 0 ? -1 : c;
}

void FileStream::flush_output_buffer()
{
  WRITE(_fd, _output_buffer, _output_buffer_offset);
  _output_buffer_offset = 0;
}

void FileStream::write_char(BASE_CHAR c)
{
  if (_output_buffer != NULL)
    {
      write_byte_to_output_buffer((BYTE) c);
      if (c == '\n')
        _charpos = 0;
      else
        ++_charpos;
    }
  else
    inline_write_char(c);
}

void FileStream::write_string(const char * s)
{
  // FIXME
  INDEX len = strlen(s);
  for (INDEX i = 0; i < len; i++)
    write_char(s[i]);
}

void FileStream::write_string(AbstractString * string)
{
  // FIXME
  INDEX len = string->length();
  for (INDEX i = 0; i < len; i++)
    write_char(string->fast_char_at(i));
}

long FileStream::read_byte()
{
  unsigned char c;
  if (_data)
    {
      if (_data_offset < _data_size)
        {
          c = *(((BYTE *)_data) + _data_offset);
          ++_data_offset;
          return c;
        }
      else
        return -1;
    }
  long n = READ(_fd, &c, 1);
  return n <= 0 ? -1 : c;
}

void FileStream::write_byte(BYTE b)
{
  if (_output_buffer != NULL)
    write_byte_to_output_buffer(b);
  else
    inline_write_byte(b);
}

Value FileStream::close()
{
  if (_data != NULL)
    {
#ifdef WIN32
      if (!UnmapViewOfFile(_data))
        {
          printf("UnmapViewOfFile failed\n");
          fflush(stdout);
        }
      _data = NULL;
      if (_hmapping != NULL)
        {
          if (!CloseHandle(_hmapping))
            {
              printf("CloseHandle(_hmapping) failed\n");
              fflush(stdout);
            }
          _hmapping = NULL;
        }
      if (_hfile != INVALID_HANDLE_VALUE)
        {
          if (!CloseHandle(_hfile))
            {
              printf("CloseHandle(_hfile) failed\n");
              fflush(stdout);
            }
          _hfile = INVALID_HANDLE_VALUE;
        }
#else
      int ret = munmap(_data, _data_size);
      if (ret)
        {
          printf("munmap returned %d\n", ret);
          fflush(stdout);
        }
      _data = NULL;
#endif
      _data_size = 0;
    }
  if (_output_buffer != NULL)
    flush_output_buffer();
  if (_fd >= 0)
    {
      ::close(_fd);
      _fd = -1;
    }
  _open = false;
  return T;
}

void FileStream::finish_output()
{
  if (_output_buffer != NULL)
    flush_output_buffer();
}

Value FileStream::file_position()
{
  if (_data)
    return make_integer(_data_offset);
  if (_output_buffer != NULL)
    flush_output_buffer();
  long n = lseek(_fd, 0, SEEK_CUR);
  if (n >= 0)
    {
      if (_bytes_per_unit == 1)
        return make_integer(n);
      else
        return make_integer(n / _bytes_per_unit);
    }
  return NIL;
}

Value FileStream::set_file_position(Value arg)
{
  if (_data)
    {
      if (arg == K_start)
        {
          _data_offset = 0;
          return T;
        }
      if (arg == K_end)
        {
          _data_offset = _data_size;
          return T;
        }
      INDEX n = check_index(arg); // FIXME arg might be a bignum
      if (n <= _data_offset)
        {
          _data_offset = n;
          return T;
        }
      return NIL;
    }
  else
    {
      if (arg == K_start)
        return lseek(_fd, 0, SEEK_SET) == 0 ? T : NIL;
      if (arg == K_end)
        return lseek(_fd, 0, SEEK_END) >= 0 ? T : NIL;
      INDEX n = check_index(arg); // FIXME arg might be a bignum
      if (_bytes_per_unit == 1)
        return lseek(_fd, n, SEEK_SET) >= 0 ? T : NIL;
      else
        return lseek(_fd, n * _bytes_per_unit, SEEK_SET) >= 0 ? T : NIL;
    }
}

Value FileStream::file_length() const
{
  INDEX length;
  if (_data)
    length = _data_size;
  else
    {
      struct stat statbuf;
      if (fstat(_fd, &statbuf) == 0)
        length = statbuf.st_size;
      else
        return signal_lisp_error("fstat error");
    }
  if (_bytes_per_unit != 1)
    length /= _bytes_per_unit;
  if (length <= (INDEX) MOST_POSITIVE_FIXNUM)
    return make_fixnum(length);
  return make_value(new Bignum(length));
}

// ### make-file-stream pathname namestring element-type direction if-exists => stream
Value SYS_make_file_stream(Value pathname, Value namestring, Value element_type,
                           Value direction, Value if_exists)
{
  return make_value(new FileStream(pathname, namestring,
                                   element_type, direction, if_exists));
}

// Atomically creates a new, empty file named by this namestring if and only if
// a file with this name does not yet exist. Returns T if the named file did
// not exist and was created, NIL otherwise. See java.io.File.createNewFile().
Value SYS_create_new_file(Value namestring)
{
  const char * filename = check_string(namestring)->as_c_string();
  long fd = OPEN(filename, O_WRONLY | O_CREAT | O_EXCL, 0644);
  if (fd < 0)
    return NIL;
  CLOSE(fd);
  return T;
}

// ### stream-external-format stream => format
Value CL_stream_external_format(Value arg)
{
  return check_ansi_stream(arg)->external_format();
}
