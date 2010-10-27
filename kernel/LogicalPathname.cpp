// LogicalPathname.cpp
//
// Copyright (C) 2009-2010 Peter Graves <gnooth@gmail.com>
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "FileError.hpp"
#include "LogicalPathname.hpp"
#include "TypeError.hpp"

EqualHashTable * LOGICAL_PATHNAME_TRANSLATION_TABLE;

LogicalPathname::LogicalPathname(Value host, Value device, Value directory,
                                 Value name, Value type, Value version)
  : Pathname(WIDETAG_LOGICAL_PATHNAME)
{
  _host = host;
  _device = device;
  _directory = directory;
  _name = name;
  _type = type;
  _version = version;
}

Value parse_logical_pathname_directory(AbstractString * s)
{
  Value result;
  unsigned long i;
  if (s->length() > 1 && s->char_at(0) == ';')
    {
      result = make_cons(K_relative);
      i = 1;
    }
  else
    {
      result = make_cons(K_absolute);
      i = 0;
    }
  const unsigned long limit = s->length();
  while (i < limit)
    {
      String * token = new String();
      while (i < limit)
        {
          char c = s->char_at(i++);
          if (is_separator_char(c))
            break;
          else
            token->append_char(c);
        }
      Value value;
      if (token->equal("*"))
        value = K_wild;
      else if (token->equal("**"))
        value = K_wild_inferiors;
      else if (token->equal(".."))
        {
          if (stringp(car(result)))
            {
              result = cdr(result);
              continue;
            }
          value = K_up;
        }
      else
        value = make_value(token);
      result = make_cons(value, result);
    }
  return CL_nreverse(result);
}

LogicalPathname::LogicalPathname(AbstractString * host, AbstractString * rest)
  : Pathname(WIDETAG_LOGICAL_PATHNAME)
{
//   final int limit = rest.length();
//   for (int i = 0; i < limit; i++) {
//     char c = rest.charAt(i);
//     if (LOGICAL_PATHNAME_CHARS.indexOf(c) < 0) {
//       error(new ParseError("The character #\\" + c + " is not valid in a logical pathname."));
//       return;
//     }
//   }

//   this.host = new SimpleString(host);

  _host = make_value(host);

  // "The device component of a logical pathname is always :UNSPECIFIC; no
  // other component of a logical pathname can be :UNSPECIFIC."
  _device = K_unspecific;

  long semi = rest->last_index_of(';');
  if (semi >= 0)
    {
      // directory
      AbstractString * d = rest->substring(0, semi);
      _directory = parse_logical_pathname_directory(d);
      rest = rest->substring(semi + 1);
    }
  else
    {
      // "If a relative-directory-marker precedes the directories, the
      // directory component is parsed as relative; otherwise, the directory
      // component is parsed as absolute."
      _directory = make_cons(K_absolute);
    }

  long dot = rest->index_of('.');
  if (dot >= 0)
    {
      AbstractString * n = rest->substring(0, dot);
      if (n->equal("*"))
        _name = K_wild;
      else
        _name = make_value(n->upcase());
      rest = rest->substring(dot + 1);
      dot = rest->index_of('.');
      if (dot >= 0)
        {
          AbstractString * t = rest->substring(0, dot);
          if (t->equal("*"))
            _type = K_wild;
          else
//             _type = new SimpleString(t.toUpperCase());
            _type = make_value(t->upcase());
          // What's left is the version.
          AbstractString * v = rest->substring(dot + 1);
          if (v->equal("*"))
            _version = K_wild;
          else if (v->equal("NEWEST") || v->equal("newest"))
            _version = K_newest;
          else
//             _version = PACKAGE_CL.intern("PARSE-INTEGER").execute(new SimpleString(v));
            printf("FIXME!! LogicalPathname constructor\n");
        }
      else
        {
          AbstractString * t = rest;
          if (t->equal("*"))
            _type = K_wild;
          else
//             type = new SimpleString(t.toUpperCase());
            _type = make_value(t->upcase());
        }
    }
  else
    {
      AbstractString * n = rest;
      if (n->equal("*"))
        _name = K_wild;
      else if (n->length() > 0)
//         name = new SimpleString(n.toUpperCase());
        _name = make_value(n->upcase());
    }
}

Value LogicalPathname::type_of() const
{
  return S_logical_pathname;
}

Value LogicalPathname::class_of() const
{
  return C_logical_pathname;
}

bool LogicalPathname::typep(Value type) const
{
  return (type == S_logical_pathname || type == S_pathname || type == S_atom
          || type == T || type == C_logical_pathname || type == C_pathname
          || type == C_t);
}

String * LogicalPathname::directory_namestring()
{
  String * s = new String();
  // "If a pathname is converted to a namestring, the symbols NIL and
  // :UNSPECIFIC cause the field to be treated as if it were empty. That
  // is, both NIL and :UNSPECIFIC cause the component not to appear in
  // the namestring." 19.2.2.2.3.1
  if (_directory != NIL)
    {
    Value directory = _directory;
    Value component = car(directory);
    if (component == K_absolute)
      ;
    else if (component == K_relative)
      s->append_char(';');
    else
      {
//       error(new FileError("Unsupported directory component " + component.writeToString() + ".",
//                           this));
        String * message = new String("Unsupported directory component ");
        message->append(::prin1_to_string(component));
        signal_lisp_error(new FileError(message, make_value(this)));
        // not reached
        return NULL;
      }
    directory = cdr(directory);
    while (directory != NIL)
      {
      component = car(directory);
      if (stringp(component))
        s->append(the_string(component));
      else if (component == K_wild)
        s->append_char('*');
      else if (component == K_wild_inferiors)
        s->append("**");
      else if (component == K_up)
        s->append("..");
      else
        {
          String * message = new String("Unsupported directory component ");
          message->append(::prin1_to_string(component));
          signal_lisp_error(new FileError(message, make_value(this)));
          // not reached
          return NULL;
        }
      s->append_char(';');
      directory = cdr(directory);
    }
  }
  return s;
}

AbstractString * LogicalPathname::write_to_string()
{
  Thread * thread = current_thread();
  bool escape = thread->symbol_value(S_print_escape) || thread->symbol_value(S_print_readably);
  String * s = new String();
  if (escape)
    s->append("#P\"");
  s->append(the_string(_host));
  s->append_char(':');
  if (_directory != NIL)
    s->append(directory_namestring());
  if (_name != NIL)
    {
      if (_name == K_wild)
        s->append_char('*');
      else
        s->append(::princ_to_string(_name));
    }
  if (_type != NIL)
    {
      s->append_char('.');
      if (_type == K_wild)
        s->append_char('*');
      else
        s->append(::princ_to_string(_type));
    }
  if (integerp(_version))
    {
      s->append_char('.');
      s->append(::write_to_string(_version)->upcase());
    }
  else if (_version == K_wild)
    s->append(".*");
  else if (_version == K_newest)
    s->append(".NEWEST");
  if (escape)
    s->append_char('"');
  return s;
}

AbstractString * canonicalize_string_component(AbstractString * arg)
{
  INDEX len = arg->length();
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = arg->char_at(i);
      if (strchr("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-", c) == NULL)
        {
          String * s = new String("Invalid character #\\");
          s->append_char(c);
          s->append(" in logical pathname component \"");
          s->append(arg);
          s->append_char('"');
          signal_lisp_error(s);
          // not reached
          return NULL;
        }
    }
  return arg->upcase();
}

// ### canonicalize-logical-host host => canonical-host
Value SYS_canonicalize_logical_host(Value arg)
{
  AbstractString * s = check_string(arg);
  if (s->length() == 0)
    {
      // "The null string, "", is not a valid value for any component of a
      // logical pathname." 19.3.2.2
      return signal_lisp_error("Invalid logical host name: \"\"");
    }
  return make_value(canonicalize_string_component(s));
}

// ### make-logical-pathname namestring => logical-pathname
Value SYS_make_logical_pathname(Value arg)
{
  AbstractString * s = check_string(arg);
  AbstractString * h = get_host_string(s);
  if (h != NULL)
    {
      if (h->length() == 0)
        {
          // "The null string, "", is not a valid value for any component of a
          // logical pathname." 19.3.2.2
          return signal_lisp_error("Invalid logical host name: \"\"");
        }
      if (LOGICAL_PATHNAME_TRANSLATION_TABLE->get(make_value(h)) != NULL_VALUE)
        return make_value(new LogicalPathname(h, s->substring(s->index_of(':') + 1)));
    }
  return signal_lisp_error(new TypeError("Logical namestring does not specify a valid host",
                                         arg, list2(S_satisfies, S_logical_namestring_p)));
}

// ### logical-namestring-p object => generalized-boolean
Value SYS_logical_namestring_p(Value arg)
{
  if (!(stringp(arg)))
    return NIL;
  AbstractString * host = get_host_string(the_string(arg));
  if (host == NULL || host->length() == 0)
    return NIL;
  if (LOGICAL_PATHNAME_TRANSLATION_TABLE->get(make_value(host)) != NULL_VALUE)
    return T;
  return NIL;
}

// ### logical-pathname-p object => generalized-boolean
Value SYS_logical_pathname_p(Value arg)
{
  return logical_pathname_p(arg) ? T : NIL;
}
