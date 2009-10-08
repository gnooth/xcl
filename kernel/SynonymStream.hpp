// SynonymStream.hpp
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

#ifndef __SYNONYM_STREAM_HPP
#define __SYNONYM_STREAM_HPP

class SynonymStream : public Stream
{
private:
  Value _symbol;

public:
  SynonymStream(Value symbol)
    : Stream(WIDETAG_SYNONYM_STREAM), _symbol(symbol)
  {
  }

  Value symbol() const
  {
    return _symbol;
  }

  virtual bool is_input_stream () const
  {
    return check_stream(current_thread()->symbol_value(_symbol))->is_input_stream();
  }

  virtual bool is_output_stream () const
  {
    return check_stream(current_thread()->symbol_value(_symbol))->is_output_stream();
  }

  virtual Value element_type() const
  {
    return check_stream(current_thread()->symbol_value(_symbol))->element_type();
  }

  virtual Value type_of() const
  {
    return S_synonym_stream;
  }

  virtual Value class_of() const
  {
    return C_synonym_stream;
  }

  virtual bool typep(Value type) const;

  virtual bool is_char_ready()
  {
    return check_stream(current_thread()->symbol_value(_symbol))->is_char_ready();
  }

  virtual int read_char()
  {
    return check_stream(current_thread()->symbol_value(_symbol))->read_char();
  }

  virtual void unread_char(BASE_CHAR c)
  {
    check_stream(current_thread()->symbol_value(_symbol))->unread_char(c);
  }

  virtual void write_char(BASE_CHAR c)
  {
    check_stream(current_thread()->symbol_value(_symbol))->write_char(c);
  }

  virtual long read_byte()
  {
    return check_stream(current_thread()->symbol_value(_symbol))->read_byte();
  }

  virtual void write_byte(unsigned char n)
  {
    check_stream(current_thread()->symbol_value(_symbol))->write_byte(n);
  }

  virtual Value close()
  {
    return check_stream(current_thread()->symbol_value(_symbol))->close();
  }

  virtual Value fresh_line()
  {
    return check_stream(current_thread()->symbol_value(_symbol))->fresh_line();
  }

  virtual Value file_length() const
  {
    return check_stream(current_thread()->symbol_value(_symbol))->file_length();
  }
};

inline bool synonym_stream_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_SYNONYM_STREAM);
}

inline SynonymStream * the_synonym_stream(Value value)
{
  assert(synonym_stream_p(value));
  return reinterpret_cast<SynonymStream *>(value - LOWTAG_TYPED_OBJECT);
}

inline SynonymStream * check_synonym_stream(Value value)
{
  if (synonym_stream_p(value))
    return the_synonym_stream(value);
  signal_type_error(value, S_synonym_stream);
  // Not reached.
  return NULL;
}

#endif // SynonymStream.hpp
