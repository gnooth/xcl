// Pathname.hpp
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

#ifndef __PATHNAME_HPP
#define __PATHNAME_HPP

class Pathname : public TypedObject
{
protected:
  Value _host;
  Value _device;
  Value _directory;
  Value _name;

  // a string, NIL, :WILD or :UNSPECIFIC
  Value _type;

  // a positive integer, or NIL, :WILD, :UNSPECIFIC, or :NEWEST
  Value _version;

  AbstractString * _namestring;

  Pathname(long widetag) : TypedObject(widetag), _host(NIL), _device(NIL), _directory(NIL),
    _name(NIL), _type(NIL), _version(NIL), _namestring(NULL)
  {
  }

public:
  Pathname(Value host, Value device, Value directory, Value name, Value type,
           Value version);

  Pathname(AbstractString * s);

  Value host()      const { return _host; }
  Value device()    const { return _device; }
  Value directory() const { return _directory; }
  Value name()      const { return _name; }
  Value type()      const { return _type; }
  Value version()   const { return _version; }

  virtual Value type_of() const;
  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  virtual bool equal(Value value) const;

  virtual unsigned long hash();

  virtual AbstractString * write_to_string();

  virtual String * directory_namestring();

  SimpleString * namestring();

  bool is_wild() const;
};

inline bool pathnamep(Value value)
{
  if (typed_object_p(value))
    {
      long widetag = the_typed_object(value)->widetag();
      if (widetag == WIDETAG_PATHNAME || widetag == WIDETAG_LOGICAL_PATHNAME)
        return true;
    }
  return false;
}

inline Pathname * the_pathname(Value value)
{
  assert(pathnamep(value));
  return reinterpret_cast<Pathname *>(value - LOWTAG_TYPED_OBJECT);
}

inline Pathname * check_pathname(Value value)
{
  if (pathnamep(value))
    return the_pathname(value);
  signal_type_error(value, S_pathname);
  // not reached
  return NULL;
}

#ifdef WIN32
const char SEPARATOR_CHAR = '\\';
#else
const char SEPARATOR_CHAR = '/';
#endif

inline bool is_separator_char(char c)
{
#ifdef WIN32
  return c == '/' || c == '\\';
#else
  return c == '/';
#endif
}

Value coerce_to_pathname(Value arg);

Value get_current_directory();

Value merge_directories(Value dir, Value default_dir);

Pathname * merge_pathnames(Pathname * pathname, Pathname * default_pathname,
                           Value default_version);

AbstractString * get_host_string(AbstractString * s);

Value parse_namestring(AbstractString * namestring);
Value parse_namestring(AbstractString * namestring, AbstractString * host);

Value parse_directory(AbstractString * s);

Value make_pathname_from_list(Value arg);

#endif // Pathname.hpp
