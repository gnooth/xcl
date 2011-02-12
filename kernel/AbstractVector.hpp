// AbstractVector.hpp
//
// Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

#ifndef __ABSTRACT_VECTOR_HPP
#define __ABSTRACT_VECTOR_HPP

class AbstractVector : public AbstractArray
{
protected:
  INDEX _capacity;
#ifdef __x86_64__
  // add padding so code vectors (in particular) will always be 16-byte aligned
  long _padding;
#endif

  AbstractVector(long widetag)
    : AbstractArray(widetag)
  {
  }

  AbstractVector(long widetag, INDEX capacity)
    : AbstractArray(widetag), _capacity(capacity)
  {
  }

  Value no_fill_pointer()
  {
    return signal_type_error(make_value(this),
                             list3(S_and, S_vector,
                                   list2(S_satisfies,
                                         S_array_has_fill_pointer_p)));
  }

public:
  long capacity_offset()
  {
    return ((long)(&(this->_capacity))) - ((long)this);
  }

  virtual Value class_of() const;

  void check_fill_pointer()
  {
    if (!has_fill_pointer())
      no_fill_pointer();
  }

  INDEX capacity() const
  {
    return _capacity;
  }

  virtual bool has_fill_pointer() const = 0;

  virtual INDEX length() const = 0;

  virtual void set_length(INDEX length) = 0;

  virtual unsigned int rank() const;

  virtual Value dimensions() const;

  virtual INDEX dimension(unsigned int n) const;

  virtual INDEX total_size() const;

  virtual bool equalp(Value value) const;

  virtual void fill(Value value) = 0;

  virtual Value elt(INDEX i) const = 0;

  virtual Value push(Value new_element) = 0;
  virtual Value push_extend(Value new_element, INDEX extension) = 0;

  // FIXME this should be pure
  virtual Value push_extend(Value new_element)
  {
    // REVIEW array-dimension-limit
    return push_extend(new_element, length() + 1);
  }

  virtual Value pop() = 0;

  virtual Value reverse() const = 0;
  virtual Value nreverse() = 0;

  virtual Value subseq(INDEX start, INDEX end) const = 0;

  virtual AbstractVector * adjust_vector(INDEX new_capacity,
                                         Value initial_element,
                                         Value initial_contents)
  {
    signal_lisp_error("Not implemented.");
    // not reached
    return NULL;
  }

  virtual AbstractVector * displace_vector(INDEX new_capacity,
                                           AbstractArray * displaced_to,
                                           INDEX offset)
  {
    signal_lisp_error("Not implemented.");
    // not reached
    return NULL;
  }

  virtual AbstractString * write_to_string();

  // not virtual
  Value bad_index(INDEX i) const;
};

inline bool vectorp(Value value)
{
  if (typed_object_p(value))
    {
      if (the_typed_object(value)->widetag() & WIDETAG_VECTOR_BIT)
        return true;
    }
  return false;
}

inline AbstractVector * the_vector(Value value)
{
  assert(vectorp(value));
  return reinterpret_cast<AbstractVector *>(value - LOWTAG_TYPED_OBJECT);
}

inline AbstractVector * check_vector(Value value)
{
  if (vectorp(value))
    return the_vector(value);
  signal_type_error(value, S_vector);
  // not reached
  return NULL;
}

inline Value no_fill_pointer(Value arg)
{
  return signal_type_error(arg,
                           list3(S_and, S_vector,
                                 list2(S_satisfies,
                                       S_array_has_fill_pointer_p)));
}

#endif
