// AbstractArray.hpp
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

#ifndef __ABSTRACT_ARRAY_HPP
#define __ABSTRACT_ARRAY_HPP

class AbstractArray : public TypedObject
{
protected:
  AbstractArray(long widetag) : TypedObject(widetag)
  {
  }

  static unsigned long compute_total_size(unsigned long rank,
                                          unsigned long dimensions[]);

  static void copy_array(AbstractArray * a1, AbstractArray * a2);

  static void copy_array_1(AbstractArray * a1, AbstractArray * a2,
                           INDEX subscripts[], int axis);
public:
  virtual Value class_of() const;

  virtual Value element_type() const = 0;
  virtual unsigned int rank() const = 0;
  virtual Value dimensions() const = 0;
  virtual INDEX dimension(unsigned int n) const = 0;
  virtual INDEX total_size() const = 0;

  virtual bool is_adjustable() const = 0;

  virtual bool is_displaced() const;
  virtual Value displacement() const;

  // row-major-aref
  virtual Value aref(INDEX i) const = 0;
  virtual Value aset(INDEX i, Value new_value) = 0;

  INDEX row_major_index(unsigned int nsubs, INDEX subscripts[]);

  virtual bool equalp(Value value) const;

  virtual AbstractArray * adjust_array(unsigned long rank, unsigned long dimensions[],
                                       Value initial_element, Value initial_contents)
  {
    signal_lisp_error("Not implemented.");
    // Not reached.
    return NULL;
  }

  virtual AbstractArray * displace_array(unsigned long rank, unsigned long dimensions[],
                                         AbstractArray * displaced_to, unsigned long offset)
  {
    signal_lisp_error("Not implemented.");
    // Not reached.
    return NULL;
  }

  AbstractString * write_to_string_internal(int ndims, INDEX dimv[]);

  void append_contents(int ndims, INDEX dimensions[], int index, String * s, Thread * thread);
};

inline bool arrayp(Value value)
{
  if (typed_object_p(value))
    {
      if (the_typed_object(value)->widetag() & WIDETAG_ARRAY_BIT)
        return true;
    }
  return false;
}

inline AbstractArray * the_array(Value value)
{
  assert(arrayp(value));
  return reinterpret_cast<AbstractArray *>(value - LOWTAG_TYPED_OBJECT);
}

inline AbstractArray * check_array(Value value)
{
  if (arrayp(value))
    return the_array(value);
  signal_type_error(value, S_array);
  // Not reached.
  return NULL;
}

extern Value upgraded_array_element_type(Value type);

#endif
