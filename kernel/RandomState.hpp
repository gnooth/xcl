// RandomState.hpp
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

#ifndef __RANDOM_STATE_HPP
#define __RANDOM_STATE_HPP

class RandomState : public TypedObject, public gc_cleanup
{
private:
  gmp_randstate_t _state;

public:
  RandomState(RandomState * rs);

  virtual ~RandomState();

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  Value random(Value arg);

  virtual AbstractString * write_to_string();
};

inline bool random_state_p(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_RANDOM_STATE);
}

inline RandomState * the_random_state(Value value)
{
  assert(random_state_p(value));
  return reinterpret_cast<RandomState *>(value - LOWTAG_TYPED_OBJECT);
}

inline RandomState * check_random_state(Value value)
{
  if (random_state_p(value))
    return the_random_state(value);
  signal_type_error(value, S_random_state);
  // Not reached.
  return NULL;
}

#endif
