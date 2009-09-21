// Symbol.cpp
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

#include <ctype.h>      // toupper, tolower
#include "lisp.hpp"
#include "primitives.hpp"
#include "Macro.hpp"
#include "Mutex.hpp"
#include "Package.hpp"
#include "Readtable.hpp"
#include "SpecialOperator.hpp"
#include "UnboundVariable.hpp"
#include "WrongNumberOfArgumentsError.hpp"
#include "keywordp.hpp"

Symbol::Symbol(const char * name)
  : _hash((unsigned long)-1), _name(new_simple_string(name)), _package(NIL),
    _value(NULL_VALUE), _plist(NULL_VALUE), _binding_index(0)
{
}

Symbol::Symbol(AbstractString * name)
  : _hash((unsigned long)-1), _name(new_simple_string(name)), _package(NIL),
    _value(NULL_VALUE), _plist(NULL_VALUE), _binding_index(0)
{
}

Symbol::Symbol(const char * name, Package * package)
  : _hash((unsigned long)-1), _name(new_simple_string(name)), _package(make_value(package)),
    _value(NULL_VALUE), _plist(NULL_VALUE), _binding_index(0)
{
}

Symbol::Symbol(AbstractString * name, Package * package)
  : _hash((unsigned long)-1), _name(new_simple_string(name)), _package(make_value(package)),
    _value(NULL_VALUE), _plist(NULL_VALUE), _binding_index(0)
{
}

void Symbol::set_special_operator(class SpecialOperator * special_operator)
{
  _function = special_operator;
  _flags &= ~FLAG_MACRO;
  _flags |= FLAG_SPECIAL_OPERATOR;
}

unsigned long Symbol::hash()
{
  if (_hash == (unsigned long)-1)
    _hash = _name->hash();
  return _hash;
}

void Symbol::set_macro_function(Function * function)
{
  assert(function->widetag() != WIDETAG_MACRO);
  if (_flags & FLAG_SPECIAL_OPERATOR)
    put(S_macro, make_value(new Macro(make_value(this), function)));
  else
    {
      _function = new Macro(make_value(this), function);
      _flags |= FLAG_MACRO;
    }
  _flags &= ~FLAG_AUTOLOAD;
}

extern Mutex * binding_index_mutex;

static volatile INDEX next_binding_index = 1;

INDEX Symbol::assign_binding_index()
{
  if (binding_index_mutex->lock())
    {
      if (_binding_index == 0)
        _binding_index = next_binding_index++;
      binding_index_mutex->unlock();
    }
  return _binding_index;
}

Value Symbol::get(Value indicator) const
{
  if (_plist != NULL_VALUE)
    {
      Value list = _plist;
      while (list != NIL)
        {
          if (car(list) == indicator)
            return car(xcdr(list));
          list = cdr(xcdr(list));
        }
    }
  return NIL;
}

void Symbol::put(Value indicator, Value value)
{
  if (_plist == NULL_VALUE)
    _plist = NIL;
  Value list = _plist;
  while (list != NIL)
    {
      if (car(list) == indicator)
        {
          // found it
          check_cons(xcdr(list))->setcar(value);
          return;
        }
      list = cdr(xcdr(list));
    }
  // not found
  _plist = make_cons(indicator, make_cons(value, _plist));
}

Value Symbol::remprop(Value indicator)
{
  if (_plist == NULL_VALUE)
    return NIL;
  Value list = _plist;
  Value prev = NULL_VALUE;
  while (list != NIL)
    {
      if (!consp(cdr(list)))
        {
          String * string = new String("The symbol ");
          string->append(prin1_to_string());
          string->append(" has an odd number of items in its property list.");
          signal_lisp_error(new ProgramError(string));
        }
      if (xcar(list) == indicator)
        {
          // found it
          if (prev != NULL_VALUE)
            setcdr(prev, CL_cddr(list));
          else
            _plist = CL_cddr(list);
          return T;
        }
      prev = cdr(list);
      list = CL_cddr(list);
    }
  // not found
  return NIL;
}

bool Symbol::apropos(const char * s)
{
#ifdef WIN32
  // REVIEW
//   String * name = new String(_name);
//   name->upcase();
  AbstractString * name = _name->upcase();
//   String * pattern = new String(s);
//   pattern->upcase();
  SimpleString * pattern = (new_simple_string(s))->nupcase();
  return strstr(name->as_c_string(), pattern->as_c_string()) != NULL;
#else
  return strcasestr(_name->as_c_string(), s) != NULL;
#endif
}

AbstractString * Symbol::princ_to_string()
{
  Thread * const thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_print_escape, NIL);
  thread->bind_special(S_print_readably, NIL);
  AbstractString * s = write_to_string();
  thread->set_last_special_binding(last_special_binding);
  return s;
}

AbstractString * Symbol::prin1_to_string()
{
  Thread * const thread = current_thread();
  void * last_special_binding = thread->last_special_binding();
  thread->bind_special(S_print_escape, T);
  AbstractString * s = write_to_string();
  thread->set_last_special_binding(last_special_binding);
  return s;
}

static bool needs_escape(AbstractString * s, Value readtable_case, Thread * thread)
{
  bool escape = false;
  INDEX len = s->length();
  if (len == 0)
    return true;
  if (s->fast_char_at(0) == '#')
    return true;
  int radix = check_index(thread->symbol_value(S_print_base), 2, 36);
  bool seen_non_digit = false;
  for (int i = len; i-- > 0;)
    {
      BASE_CHAR c = s->fast_char_at(i);
//       if ("(),|\\`'\";:".indexOf(c) >= 0)
//         return true;
      if (strchr("(),|\\`'\";:", c))
        return true;
//       if (Character.isWhitespace(c))
//         return true;
      if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
        return true;
      if (readtable_case == K_upcase)
        {
          if (islower(c))
            return true;
        }
      else if (readtable_case == K_downcase)
        {
          if (isupper(c))
            return true;
        }
      if (!escape && !seen_non_digit)
        {
          if (!digit_char_p(c, radix))
            seen_non_digit = true;
        }
    }
  if (!seen_non_digit)
    return true;
  if (s->fast_char_at(0) == '.')
    {
      bool all_dots = true;
      for (INDEX i = len; i-- > 1;)
        {
          if (s->fast_char_at(i) != '.')
            {
              all_dots = false;
              break;
            }
        }
      if (all_dots)
        return true;
    }
  return false;
}

static AbstractString * multiple_escape(AbstractString * s)
{
  String * string = new String("|");
  INDEX len = s->length();
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = s->fast_char_at(i);
      if (c == '|' || c == '\\')
        string->append_char('\\');
      string->append_char(c);
    }
  string->append_char('|');
  return string;
}

static AbstractString * capitalize(AbstractString * s, Value readtable_case)
{
  // FIXME
//   return s;

  if (readtable_case == K_invert || readtable_case == K_preserve)
    return s;
  INDEX len = s->length();
  SimpleString * string = new_simple_string(len);
  bool last_char_was_alphanumeric = false;
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = s->fast_char_at(i);
      if (islower(c))
        {
          if (readtable_case == K_upcase)
            string->fast_set_char_at(i, c);
          else // DOWNCASE
//             sb.append(last_char_was_alphanumeric ? c : LispCharacter.toUpperCase(c));
            string->fast_set_char_at(i, last_char_was_alphanumeric ? c : toupper(c));
          last_char_was_alphanumeric = true;
        }
      else if (isupper(c))
        {
          if (readtable_case == K_upcase)
//             sb.append(last_char_was_alphanumeric ? LispCharacter.toLowerCase(c) : c);
            string->fast_set_char_at(i, last_char_was_alphanumeric ? tolower(c) : c);
          else // DOWNCASE
            string->fast_set_char_at(i, c);
          last_char_was_alphanumeric = true;
        }
      else
        {
          string->fast_set_char_at(i, c);
          last_char_was_alphanumeric = (c >= '0' && c <= '9');
        }
    }
  return string;
}

static AbstractString * invert(AbstractString * s)
{
  // "When the readtable case is :INVERT, the case of all alphabetic characters
  // in single case symbol names is inverted. Mixed-case symbol names are
  // printed as is." (22.1.3.3.2)
  INDEX len = s->length();
  const int LOWER = 1;
  const int UPPER = 2;
  int state = 0;
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = s->fast_char_at(i);
      if (isupper(c))
        {
          if (state == LOWER)
            return s; // Mixed case.
          state = UPPER;
        }
      if (islower(c))
        {
          if (state == UPPER)
            return s; // Mixed case.
          state = LOWER;
        }
    }
  SimpleString * string = new_simple_string(len);
  for (INDEX i = 0; i < len; i++)
    {
      BASE_CHAR c = s->fast_char_at(i);
      if (isupper(c))
        string->fast_set_char_at(i, tolower(c));
      else if (islower(c))
        string->fast_set_char_at(i, toupper(c));
      else
        string->fast_set_char_at(i, c);
    }
  return string;
}

static AbstractString * casify(String * string, Value readtable_case, Value print_case)
{
  if (readtable_case == K_preserve)
    return string;
  if (readtable_case == K_invert)
    return invert(string);
  if (print_case == K_downcase)
    return string->ndowncase();
  if (print_case == K_upcase)
    return string->nupcase();
  if (print_case == K_capitalize)
    return capitalize(string, readtable_case);
  return string;
}

AbstractString * Symbol::write_to_string()
{
  Thread * thread = current_thread();
  bool print_escape = (thread->symbol_value(S_print_escape) != NIL);
  bool print_readably = (thread->symbol_value(S_print_readably) != NIL);
  Value readtable_case =
    check_readtable(thread->symbol_value(S_current_readtable))->readtable_case();
  Value print_case = thread->symbol_value(S_print_case);
  Package * package = _package != NIL ? check_package(_package) : NULL;
  if (print_readably)
    {
      if (readtable_case != K_upcase || print_case != K_upcase)
        {
          String * string = new String();
          if (package == PACKAGE_KEYWORD)
            string->append_char(':');
          else if (_package == NIL)
            string->append("#:");
          else
            {
              string->append(multiple_escape(package->name()));
              string->append("::");
            }
          string->append(multiple_escape(_name));
          return string;
        }
      print_escape = true;
    }
  if (print_escape)
    {
      bool escape = needs_escape(_name, readtable_case, thread);
      String * string = new String();
      if (package == PACKAGE_KEYWORD)
        {
          string->append_char(':');
          if (escape)
            string->append(multiple_escape(_name));
          else
            string->append(_name);
          return string;
        }
      if (package == NULL)
        {
          if (thread->symbol_value(S_print_fasl) != NIL)
            string->append("#$");
          else if (print_readably || thread->symbol_value(S_print_gensym) != NIL)
            string->append("#:");
          if (escape)
            string->append(multiple_escape(_name));
          else
            string->append(_name);
          return string;
        }
      Package * current_package = ::current_package(thread);
      if (current_package == package)
        {
          if (escape)
            {
              string->append(multiple_escape(_name));
              return string;
            }
          else
            {
              string->append(_name);
              return casify(string, readtable_case, print_case);
            }
        }
      bool external = package->find_external_symbol(_name);
      if (current_package->uses(package))
        {
          if (external)
            {
              // check for name conflict in current package
              if (current_package->find_external_symbol(_name) == NULL
                  && current_package->find_internal_symbol(_name) == NULL)
                {
                  // no conflict
                  if (escape)
                    string->append(multiple_escape(_name));
                  else
                    string->append(_name);
                  return casify(string, readtable_case, print_case);
                }
            }
        }
      // Has this symbol been imported into the current package?
      if (current_package->find_external_symbol(_name) == this)
        return _name;
      if (current_package->find_internal_symbol(_name) == this)
        return _name;
      SimpleString * package_name = package->name();
      if (package_name != NULL)
        {
          if (needs_escape(package_name, readtable_case, thread))
            string->append(multiple_escape(package_name));
          else
            string->append(package_name);
          string->append(external ? ":" : "::");
        }
      else
        string->append("#:");
      if (escape)
        string->append(multiple_escape(_name));
      else
        string->append(_name);
      return string;
    }
  else
    {
      // *PRINT-ESCAPE* is NIL
//       return _name; // REVIEW possible to modify name
      if (package == PACKAGE_KEYWORD)
        {
          if (print_case == K_downcase)
            return _name->downcase();
          if (print_case == K_capitalize)
            return capitalize(_name, readtable_case);
          return _name;
        }
      // Printer escaping is disabled.
      if (readtable_case == K_upcase)
        {
          if (print_case == K_downcase)
            return _name->downcase();
          if (print_case == K_capitalize)
            return capitalize(_name, readtable_case);
          return _name;
        }
      else if (readtable_case == K_downcase)
        {
          // "When the readtable case is :DOWNCASE, uppercase characters
          // are printed in their own case, and lowercase characters are
          // printed in the case specified by *PRINT-CASE*." (22.1.3.3.2)
          if (print_case == K_downcase)
            return _name;
          if (print_case == K_upcase)
            return _name->upcase();
          if (print_case == K_capitalize)
            return capitalize(_name, readtable_case);
          return _name;
        }
      else if (readtable_case == K_preserve)
        {
          return _name;
        }
      else // INVERT
        return invert(_name);
    }
}

// ### symbolp
Value CL_symbolp(Value arg)
{
  return symbolp(arg) ? T : NIL;
}

// ### symbol-flags
Value SYS_symbol_flags(Value arg)
{
  if (!symbolp(arg))
    return signal_type_error(arg, S_symbol);
  return make_number(the_symbol(arg)->flags());
}

// ### symbol-value
Value CL_symbol_value(Value arg)
{
  if (!symbolp(arg))
    return signal_type_error(arg, S_symbol);
  Value value = current_thread()->symbol_value(arg);
  if (value != NULL_VALUE)
    return value;
  return signal_lisp_error(new UnboundVariable(arg));
}

// ### symbol-global-value symbol => value
Value SYS_symbol_global_value(Value arg)
{
  return check_symbol(arg)->value();
}

// ### set-symbol-global-value symbol value => value
Value SYS_set_symbol_global_value(Value symbol, Value value)
{
  check_symbol(symbol)->set_value(value);
  return value;
}

// ### symbol-plist
Value CL_symbol_plist(Value arg)
{
  return check_symbol(arg)->plist();
}

// ### set-symbol-plist
Value SYS_set_symbol_plist(Value symbol, Value plist)
{
  check_symbol(symbol)->set_plist(plist);
  return plist;
}

// ### symbol-name
Value CL_symbol_name(Value arg)
{
  return make_value(check_symbol(arg)->name());
}

// ### get2
Value SYS_get2(Value symbol, Value indicator)
{
  Value list = check_symbol(symbol)->plist();
  while (list != NIL)
    {
      if (car(list) == indicator)
        return car(xcdr(list));
      list = cdr(xcdr(list));
    }
  return NIL;
}

// ### get3
Value SYS_get3(Value symbol, Value indicator, Value default_value)
{
  Value list = check_symbol(symbol)->plist();
  while (list != NIL)
    {
      if (car(list) == indicator)
        return car(xcdr(list));
      list = cdr(xcdr(list));
    }
  return default_value;
}

// ### get
Value CL_get(unsigned int numargs, Value args[])
{
  switch (numargs)
    {
    case 2:
      return SYS_get2(args[0], args[1]);
    case 3:
      return SYS_get3(args[0], args[1], args[2]);
    default:
      return signal_lisp_error(new WrongNumberOfArgumentsError(S_get, numargs, 2, 3));
    }
}

// ### put
// (setf (get symbol indicator &optional default) new-value) => new-value
// put symbol indicator new-value => new-value
// put symbol indicator default new-value => new-value
Value SYS_put(unsigned int numargs, Value args[])
{
  Value new_value;
  switch (numargs)
    {
    case 3:
      new_value = args[2];
      break;
    case 4:
      // "When a GET form is used as a SETF place, any default which is
      // supplied is evaluated according to normal left-to-right evaluation
      // rules, but its value is ignored."
      new_value = args[3];
      break;
    default:
      return wrong_number_of_arguments(S_put, numargs, 3, 4);
    }
  Symbol * sym = check_symbol(args[0]);
  Value indicator = args[1];
  Value list = sym->plist();
  while (list != NIL)
    {
      if (car(list) == indicator)
        {
          // found it!
          check_cons(xcdr(list))->setcar(new_value);
          return new_value;
        }
      list = cdr(xcdr(list));
    }
  // not found
  sym->set_plist(make_cons(indicator, make_cons(new_value, sym->plist())));
  return new_value;
}

// ### remprop symbol indicator => generalized-boolean
Value CL_remprop(Value arg1, Value arg2)
{
  Symbol * symbol = check_symbol(arg1);
  Value list = check_list(symbol->plist());
  Value prev = NULL_VALUE;
  while (list != NIL)
    {
      if (!(consp(cdr(list))))
        {
          String * s = new String("The symbol ");
          s->append(symbol->prin1_to_string());
          s->append(" has an odd number of items in its property list.");
          return signal_lisp_error(new ProgramError(s));
        }
      if (xcar(list) == arg2)
        {
          // found it!
          if (prev != NULL_VALUE)
            SYS_setcdr(prev, CL_cddr(list));
          else
            symbol->set_plist(CL_cddr(list));
          return T;
        }
      prev = xcdr(list);
      list = CL_cddr(list);
    }
  // not found
  return NIL;
}

// ### keywordp object => boolean
Value CL_keywordp(Value arg)
{
  return keywordp(arg) ? T : NIL;
}
