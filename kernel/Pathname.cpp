// Pathname.cpp
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

#include <stdlib.h>             // getenv()
#include "lisp.hpp"
#include "primitives.hpp"
#include "Pathname.hpp"
#include "FileError.hpp"
#include "ProgramError.hpp"
#include "FileStream.hpp"

Pathname::Pathname(Value host, Value device, Value directory, Value name,
                   Value type, Value version)
: TypedObject(WIDETAG_PATHNAME), _host(host), _device(device),
  _directory(directory), _name(name), _type(type), _version(version),
  _namestring(NULL)
{
}

Pathname::Pathname(AbstractString * s)
  : TypedObject(WIDETAG_PATHNAME), _host(NIL), _device(NIL), _directory(NIL),
    _name(NIL), _type(NIL), _version(NIL), _namestring(NULL)
{
  if (!s)
    return;
#ifdef WIN32
  if (s->equal(".") || s->equal("./") || s->equal(".\\"))
     {
       _directory = make_cons(K_relative);
       return;
     }
#else
  if (s->equal(".") || s->equal("./"))
    {
      _directory = make_cons(K_relative);
      return;
    }
#endif
  if (s->equal("..") || s->equal("../"))
    {
      _directory = list2(K_relative, K_up);
      return;
    }
#ifdef WIN32
  for (INDEX i = s->length(); i-- > 0;)
    {
      if (s->fast_char_at(i) == '/')
        s->fast_set_char_at(i, '\\');
    }
#else
  const INDEX len = s->length();
  if (len > 0 && s->fast_char_at(0) == '~' && (len == 1 || s->fast_char_at(1) == '/'))
    {
      char * home = getenv("HOME");
      if (home != NULL && strlen(home) != 0)
        {
          String * temp = new String(home);
          if (temp->char_at(temp->length() - 1) != '/')
            temp->append_char('/');
          if (len > 1)
            temp->append(s->substring(2));
          s = temp;
        }
    }
#endif
  _namestring = s;
#ifdef WIN32
  if (s->length() >= 2 && s->fast_char_at(1) == ':')
    {
      String * device = new String();
      device->append_char(s->fast_char_at(0));
      _device = make_value(device);
      s = s->substring(2);
    }
#endif
  String * d = NULL;
  // find last file separator char
  for (INDEX i = s->length(); i-- > 0;)
    {
    if (is_separator_char(s->fast_char_at(i)))
      {
        d = new String(s->substring(0, i + 1));
        s = s->substring(i + 1);
        break;
      }
    }
  if (d)
    {
      if (s->equal(".."))
        {
          d->append(s);
          s = new String();
        }
      _directory = parse_directory(d);
    }
  if (s->length() > 0 && s->fast_char_at(0) == '.')
    {
      _name = make_value(new_simple_string(s));
      return;
    }
  long index = s->last_index_of('.');
  SimpleString * n = NULL;
  SimpleString * t = NULL;
  if (index > 0)
    {
      n = s->substring(0, index);
      t = s->substring(index + 1);
    }
  else if (s->length() > 0)
    n = new_simple_string(s);
  if (n)
    {
      if (n->equal("*"))
        _name = K_wild;
      else
        _name = make_value(n);
    }
  if (t)
    {
      if (t->equal("*"))
        _type = K_wild;
      else
        _type = make_value(t);
    }
}

Value parse_directory(AbstractString * s)
{
  if (s->length() == 1 && is_separator_char(s->char_at(0)))
    return make_cons(K_absolute);
  Value result;
  unsigned long i;
  if (s->length() > 1 && is_separator_char(s->char_at(0)))
    {
      result = make_cons(K_absolute);
      i = 1;
    }
  else
    {
      result = make_cons(K_relative);
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

Value get_current_directory()
{
  char buf[1024]; // REVIEW
  if (getcwd(buf, sizeof(buf)) != NULL)
    {
      String * s = new String(buf);
      INDEX len = s->length();
      if (len)
        {
          char c = s->char_at(len - 1);
          if (!is_separator_char(c))
            s->append_char(SEPARATOR_CHAR);
          return make_value(new Pathname(s));
        }
    }
  return NIL;
}

Value Pathname::type_of() const
{
  return S_pathname;
}

Value Pathname::class_of() const
{
  return C_pathname;
}

bool Pathname::typep(Value type) const
{
  return (type == S_pathname || type == S_atom || type == T
          || type == C_pathname || type == C_t);
}

bool Pathname::equal(Value value) const
{
  if (!pathnamep(value))
    return false;
  Pathname * p = the_pathname(value);
  if (this == p)
    return true;
#ifdef WIN32
  if (!::equalp(_host, p->host()))
    return false;
  if (!::equalp(_device, p->device()))
    return false;
  if (!::equalp(_directory, p->directory()))
    return false;
  if (!::equalp(_name, p->name()))
    return false;
  if (!::equalp(_type, p->type()))
    return false;
#else
  // Unix
  if (!::equal(_host, p->host()))
    return false;
  if (!::equal(_device, p->device()))
    return false;
  if (!::equal(_directory, p->directory()))
    return false;
  if (!::equal(_name, p->name()))
    return false;
  if (!::equal(_type, p->type()))
    return false;
#endif
  // ignore version component
  return true;
}

unsigned long Pathname::hash()
{
  // REVIEW case insensitivity on Windows
  return ((::hash(_host) ^
           ::hash(_device) ^
           ::hash(_directory) ^
           ::hash(_name) ^
           ::hash(_type)) & MOST_POSITIVE_FIXNUM);
}

AbstractString * Pathname::write_to_string()
{
//   Thread * thread = current_thread();
//   bool escape =
//     (thread->symbol_value(S_print_escape) != NIL || thread->symbol_value(S_print_readably) != NIL);
  String * s = new String();
  if (namestring())
    {
//       if (escape)
      Thread * thread = current_thread();
      if (thread->symbol_value(S_print_escape) != NIL || thread->symbol_value(S_print_readably) != NIL)
        s->append("#P");
      s->append(namestring()->write_to_string());
    }
  else
    {
      s->append("#P(");
      if (_host != NIL)
        {
          s->append(":HOST ");
          s->append(::write_to_string(_host));
          s->append_char(' ');
        }
      if (_device != NIL)
        {
          s->append(":DEVICE ");
          s->append(::write_to_string(_device));
          s->append_char(' ');
        }
      if (_directory != NIL)
        {
          s->append(":DIRECTORY ");
          s->append(::write_to_string(_directory));
          s->append_char(' ');
        }
      if (_name != NIL)
        {
          s->append(":NAME ");
          s->append(::write_to_string(_name));
          s->append_char(' ');
        }
      if (_type != NIL)
        {
          s->append(":TYPE ");
          s->append(::write_to_string(_type));
          s->append_char(' ');
        }
      if (_version != NIL)
        {
          s->append(":VERSION ");
          s->append(::write_to_string(_version));
          s->append_char(' ');
        }
      unsigned long last = s->length() - 1;
      if (s->char_at(last) == ' ')
        s->set_char_at(last, ')');
      else
        s->append_char(')');
    }
  return s;
}

Value coerce_to_pathname(Value arg)
{
  if (pathnamep(arg))
    return arg;
  if (stringp(arg))
    return parse_namestring(the_string(arg));
  if (file_stream_p(arg))
    return the_file_stream(arg)->pathname();
  return signal_type_error(arg, list4(S_or, S_pathname,
                                      S_string, S_file_stream));
}

// ### pathname pathspec => pathname
Value CL_pathname(Value arg)
{
  return coerce_to_pathname(arg);
}

// ### pathnamep object => generalized-boolean
Value CL_pathnamep(Value arg)
{
  return pathnamep(arg) ? T : NIL;
}

static void check_case_arg(unsigned int numargs, Value args[])
{
  if (numargs % 2 == 0)
    signal_lisp_error(new ProgramError("Odd number of keyword arguments."));
  Value unrecognized_keyword = NIL;
  const unsigned long limit = numargs - 1;
  unsigned long i = 1;
  while (i < limit)
    {
      Value keyword = args[i++];
      Value value   = args[i++];
      if (keyword == K_case)
        {
          if (value != K_common && value != K_local)
            signal_type_error(value, list3(S_member, K_common, K_local));
        }
      else if (keyword == K_allow_other_keys)
        {
          unrecognized_keyword = NIL;
          break;
        }
      else
        unrecognized_keyword = keyword;
    }
  if (unrecognized_keyword != NIL)
    {
      String * s = new String("Unrecognized keyword argument ");
      s->append(::prin1_to_string(unrecognized_keyword));
      signal_lisp_error(new ProgramError(s));
    }
}

// ### pathname-host pathname &key case => host
Value CL_pathname_host(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_pathname_host, numargs, 1, MANY);
  Value arg = args[0];
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  if (numargs > 1)
    check_case_arg(numargs, args);
  return the_pathname(arg)->host();
}

// ### pathname-device pathname &key case => device
Value CL_pathname_device(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_pathname_device, numargs, 1, MANY);
  Value arg = args[0];
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  if (numargs > 1)
    check_case_arg(numargs, args);
  return the_pathname(arg)->device();
}

// ### pathname-directory pathname &key case => directory
Value CL_pathname_directory(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_pathname_directory, numargs, 1, MANY);
  Value arg = args[0];
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  if (numargs > 1)
    check_case_arg(numargs, args);
  return the_pathname(arg)->directory();
}

// ### pathname-name pathname &key case => name
Value CL_pathname_name(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_pathname_name, numargs, 1, MANY);
  Value arg = args[0];
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  if (numargs > 1)
    check_case_arg(numargs, args);
  return the_pathname(arg)->name();
}

// ### pathname-type pathname &key case => type
Value CL_pathname_type(unsigned int numargs, Value args[])
{
  if (numargs < 1)
    return wrong_number_of_arguments(S_pathname_type, numargs, 1, MANY);
  Value arg = args[0];
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  if (numargs > 1)
    check_case_arg(numargs, args);
  return the_pathname(arg)->type();
}

// ### pathname-version pathname => version
Value CL_pathname_version(Value arg)
{
  if (pathnamep(arg))
    return the_pathname(arg)->version();
  else
    return the_pathname(coerce_to_pathname(arg))->version();
}

static void validate_directory(Value directory)
{
  while (directory != NIL)
    {
      Value first = car(directory);
      directory = xcdr(directory);
      if (first == K_absolute || first == K_wild_inferiors)
        {
          Value second = car(directory);
          if (second == K_up || second == K_back)
            {
              String * s = new String(::prin1_to_string(first));
              s->append(" may not be followed immediately by ");
              s->append(::prin1_to_string(second));
              s->append_char('.');
              signal_lisp_error(new FileError(s, make_value(new Pathname(NIL, NIL, NIL, NIL, NIL, NIL))));
            }
        }
    }
}

static void validate_string_component(AbstractString * s)
{
  const INDEX len = s->length();
  for (INDEX i = 0; i < len; i++)
    {
      char c = s->fast_char_at(i);
      if (is_separator_char(c))
        {
          String * message = new String("Invalid character #\\");
          message->append_char(c);
          message->append(" in pathname component \"");
          message->append(s);
          message->append_char('"');
          signal_lisp_error(message);
        }
    }
}

// ### make-pathname &key host device directory name type version defaults case => pathname
Value CL_make_pathname(unsigned int numargs, Value args[])
{
  if (numargs % 2 != 0)
    signal_lisp_error(new ProgramError("Odd number of keyword arguments."));

  Value host      = NIL;
  Value device    = NIL;
  Value directory = NIL;
  Value name      = NIL;
  Value type      = NIL;
  Value version   = NIL;

  Pathname * defaults = NULL;

  bool device_supplied_p = false;
  bool name_supplied_p   = false;
  bool type_supplied_p   = false;

  for (unsigned int i = 0; i < numargs; i += 2)
    {
      Value key = args[i];
      Value value = args[i+1];

      if (key == K_host)
        host = value;
      else if (key == K_device)
        {
          device = value;
          device_supplied_p = true;
        }
      else if (key == K_directory)
        {
          if (stringp(value))
            directory = list2(K_absolute, value);
          else if (value == K_wild)
            directory = list2(K_absolute, value);
          else
            directory = value;
          validate_directory(directory);
        }
      else if (key == K_name)
        {
          name = value;
          name_supplied_p = true;
        }
      else if (key == K_type)
        {
          type = value;
          type_supplied_p = true;
        }
      else if (key == K_version)
        version = value;
      else if (key == K_defaults)
        defaults = the_pathname(coerce_to_pathname(value));
      else if (key == K_case)
        ; // ignored
    }

  if (defaults != NULL)
    {
      if (host == NIL)
        host = defaults->host();
      directory = merge_directories(directory, defaults->directory());
      if (!device_supplied_p)
        device = defaults->device();
      if (!name_supplied_p)
        name = defaults->name();
      if (!type_supplied_p)
        type = defaults->type();
    }

  if (stringp(name))
    validate_string_component(the_string(name));

  Pathname * p = new Pathname(host, device, directory, name, type, version);
  return make_value(p);
}

String * Pathname::directory_namestring()
{
//   validateDirectory(true);
  String * const s = new String();
  // "If a pathname is converted to a namestring, the symbols NIL and
  // :UNSPECIFIC cause the field to be treated as if it were empty. That
  // is, both NIL and :UNSPECIFIC cause the component not to appear in
  // the namestring." 19.2.2.2.3.1
  if (_directory != NIL)
    {
      Value directory = _directory;
      Value component = car(directory);
      directory = xcdr(directory);
      if (component == K_absolute)
        s->append_char(SEPARATOR_CHAR);
      else if (component == K_relative)
        {
          if (directory == NIL)
            {
              // #p"./"
              s->append_char('.');
              s->append_char(SEPARATOR_CHAR);
            }
          else
            ; // nothing to do
        }
      else
        {
          String * message = new String("Unsupported directory component ");
          message->append(::prin1_to_string(component));
          signal_lisp_error(new FileError(message, make_value(this)));
          // not reached
          return NULL;
        }
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
          s->append_char(SEPARATOR_CHAR);
          directory = xcdr(directory);
        }
    }
  return s;
}

// ### directory-namestring pathname => namestring
Value CL_directory_namestring(Value arg)
{
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  return make_value(the_pathname(arg)->directory_namestring());
}

// ### file-namestring pathname => namestring
// "FILE-NAMESTRING returns just the name, type, and version components of PATHNAME."
Value CL_file_namestring(Value arg)
{
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  Pathname * p = the_pathname(arg);
  String * s = new String();
  Value name = p->name();
  if (stringp(name))
    s->append(the_string(name));
  else if (name == K_wild)
    s->append_char('*');
  else
    return NIL;
  Value type = p->type();
  if (stringp(type))
    {
      s->append_char('.');
      s->append(the_string(type));
    }
  else if (type == K_wild)
    s->append(".*");
  return make_value(s);
}

// ### host-namestring pathname => namestring
Value CL_host_namestring(Value arg)
{
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  Pathname * p = the_pathname(arg);
  Value host = p->host();
  if (host == NIL)
    return NIL;
  else if (stringp(host))
    return make_value(new_simple_string(the_string(host)));
  else
    return make_value(::prin1_to_string(host)); // REVIEW
}

SimpleString * Pathname::namestring()
{
  if (!_namestring)
    {
      if (_name == NIL && _type != NIL)
        return NULL;
      String * s = new String();
      // "If a pathname is converted to a namestring, the symbols NIL and
      // :UNSPECIFIC cause the field to be treated as if it were empty. That
      // is, both NIL and :UNSPECIFIC cause the component not to appear in
      // the namestring." 19.2.2.2.3.1
      if (stringp(_device))
        {
          s->append(the_string(_device));
          s->append_char(':');
        }
      s->append(directory_namestring());
      if (stringp(_name))
        s->append(the_string(_name));
      else if (_name == K_wild)
        s->append_char('*');
      if (_type != NIL)
        {
          s->append_char('.');
          if (stringp(_type))
            s->append(the_string(_type));
          else if (_type == K_wild)
            s->append_char('*');
        }
      assert(_namestring == NULL);
      _namestring = new_simple_string(s);
    }
  return new_simple_string(_namestring);
}

// ### namestring pathname => namestring
Value CL_namestring(Value arg)
{
  if (!pathnamep(arg))
    arg = coerce_to_pathname(arg);
  AbstractString * namestring = the_pathname(arg)->namestring();
  if (namestring)
    return make_value(namestring);

  String * message = new String("Pathname has no namestring: ");
  message->append(::prin1_to_string(arg));
  return signal_lisp_error(message);
}

AbstractString * get_host_string(AbstractString * s)
{
  long colon = s->index_of(':');
  if (colon >= 0)
    return s->substring(0, colon)->upcase();
  else
    return NULL;
}

// Value parse_namestring(AbstractString * namestring)
// {
//   return make_value(new Pathname(namestring));
// }

// // Value parse_namestring(AbstractString * namestring)
// // {
// //   // check for a logical pathname host
// //   AbstractString * h = get_host_string(namestring);
// //   //   if (h != null && LOGICAL_PATHNAME_TRANSLATIONS.get(new SimpleString(h)) != null)
// //   if (h != NULL && LOGICAL_PATHNAME_TRANSLATION_TABLE->get(make_value(h)) != NULL_VALUE)
// //     {
// //       // a defined logical pathname host
// //       return make_value(new LogicalPathname(h, namestring->substring(namestring->index_of(':') + 1)));
// //     }
// //   return make_value(new Pathname(namestring));
// // }

// Value parse_namestring(AbstractString * namestring, AbstractString * host)
// {
//   return make_value(new Pathname(namestring));
// }

// // ### %parse-namestring string host default-pathname => pathname, position
// Value SYS_parse_namestring_internal(Value arg1, Value arg2, Value arg3)
// {
//   Thread * const thread = current_thread();
//   AbstractString * namestring = check_string(arg1);
//   // The HOST parameter must be a string or NIL.
//   if (arg2 == NIL)
//     {
//       // "If HOST is NIL, DEFAULT-PATHNAME is a logical pathname, and THING is
//       // a syntactically valid logical pathname namestring without an explicit
//       // host, then it is parsed as a logical pathname namestring on the host
//       // that is the host component of DEFAULT-PATHNAME."
//       arg3 = coerce_to_pathname(arg3);
//       return thread->set_values(parse_namestring(namestring),
//                                 make_fixnum(namestring->length()));
//     }
//   AbstractString * host = check_string(arg2);
//   return thread->set_values(parse_namestring(namestring, host),
//                             make_fixnum(namestring->length()));
// }

bool Pathname::is_wild() const
{
  if (_host == K_wild || _host == K_wild_inferiors)
    return true;
  if (_device == K_wild || _device == K_wild_inferiors)
    return true;
  if (consp(_directory))
      {
        Value component = xcar(_directory);
        if (component == K_wild || component == K_wild_inferiors)
          return true;
        Value list = xcdr(_directory);
        while (list != NIL)
          {
            component = car(list);
            if (component == K_wild || component == K_wild_inferiors)
              return true;
            list = xcdr(list);
          }
      }
  if (_name == K_wild || _name == K_wild_inferiors)
    return true;
  if (_type == K_wild || _type == K_wild_inferiors)
    return true;
  if (_version == K_wild || _version == K_wild_inferiors)
    return true;
  return false;
}

// ### wild-pathname-p pathname &optional field-key => generalized-boolean
Value CL_wild_pathname_p(unsigned int numargs, Value args[])
{
  if (numargs < 1 || numargs > 2)
    return wrong_number_of_arguments(S_wild_pathname_p, numargs, 1, 2);
  if (!pathnamep(args[0]))
    args[0] = coerce_to_pathname(args[0]);
  Pathname * pathname = the_pathname(args[0]);
  if (numargs == 1 || args[1] == NIL)
    return pathname->is_wild() ? T : NIL;
  Value field_key = args[1];
  if (field_key == K_directory)
    {
      Value directory = pathname->directory();
      if (consp(directory))
        {
          Value component = xcar(directory);
          if (component == K_wild || component == K_wild_inferiors)
            return T;
          directory = xcdr(directory);
          while (directory != NIL)
            {
              component = car(directory);
              if (component == K_wild || component == K_wild_inferiors)
                return T;
              directory = xcdr(directory);
            }
        }
      return NIL;
    }
  Value value;
  if (field_key == K_host)
    value = pathname->host();
  else if (field_key == K_device)
    value = pathname->device();
  else if (field_key == K_name)
    value = pathname->name();
  else if (field_key == K_type)
    value = pathname->type();
  else if (field_key == K_version)
    value = pathname->version();
  else
    {
      String * message = new String("Unrecognized field key ");
      message->append(::prin1_to_string(field_key));
      return signal_lisp_error(new ProgramError(message));
    }
  return (value == K_wild || value == K_wild_inferiors) ? T : NIL;
}

// ### user-homedir-pathname &optional host => pathname
Value CL_user_homedir_pathname(unsigned int numargs, Value args[])
{
  // "If it is impossible to determine the user's home directory on HOST, then
  // NIL is returned. USER-HOMEDIR-PATHNAME never returns NIL if HOST is not
  // supplied."
  if (numargs > 1)
    return wrong_number_of_arguments(S_user_homedir_pathname, numargs, 0, 1);
  if (numargs == 1)
    {
      if (args[0] != NIL)
        return NIL; // FIXME
    }
#ifdef WIN32
  char * s = getenv("USERPROFILE");
#else
  char * s = getenv("HOME");
#endif
  if (s == NULL || strlen(s) == 0)
    return NIL; // FIXME
  String * string = new String(s);
  if (string->char_at(string->length() - 1) != '/')
    string->append_char('/');
  return make_value(new Pathname(string));
}

// ### mkdir
Value SYS_mkdir(Value arg)
{
  Pathname * pathname = the_pathname(coerce_to_pathname(arg));
  if (pathname->is_wild())
    signal_lisp_error(new FileError("Bad place for a wild pathname.", make_value(pathname)));
  Pathname * default_pathname =
    the_pathname(coerce_to_pathname(current_thread()->symbol_value(S_default_pathname_defaults)));
  Pathname * defaulted_pathname = merge_pathnames(pathname, default_pathname, NIL);
  AbstractString * namestring = defaulted_pathname->namestring();
  if (namestring == NULL || namestring->length() == 0)
    return NIL;
#ifdef WIN32
  return mkdir(namestring->as_c_string()) == 0 ? T : NIL;
#else
  return mkdir(namestring->as_c_string(), 0777) == 0 ? T : NIL;
#endif
}
