// Bignum.hpp
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

#ifndef __BIGNUM_HPP
#define __BIGNUM_HPP

class Bignum : public TypedObject //, public gc_cleanup
{
public:
  mpz_t _z;

  Bignum(long n) : TypedObject(WIDETAG_BIGNUM)
  {
    mpz_init_set_si(_z, n);
  }

  Bignum(unsigned long n) : TypedObject(WIDETAG_BIGNUM)
  {
    mpz_init_set_ui(_z, n);
  }

  Bignum(mpz_t z) : TypedObject(WIDETAG_BIGNUM)
  {
    mpz_init_set(_z, z);
  }

  Bignum(mpz_t z, bool copy) : TypedObject(WIDETAG_BIGNUM)
  {
    if (copy)
      mpz_init_set(_z, z);
    else
      {
        _z->_mp_alloc = z->_mp_alloc;
        _z->_mp_size  = z->_mp_size;
        _z->_mp_d     = z->_mp_d;

        z->_mp_alloc = 0;
        z->_mp_size  = 0;
        z->_mp_d     = 0;
      }
  }

  virtual Value type_of() const;

  virtual Value class_of() const;

  virtual bool typep(Value type) const;

  Value add(long n) const;

  Value add(Bignum * b) const;

  Value subtract(long n) const;

  Value subtract(Bignum * b) const;

  Value multiply(long n);

  Value multiply(Bignum * b);

  Value negate() const;

  bool plusp()
  {
    return mpz_sgn(_z) > 0;
  }

  bool minusp()
  {
    return mpz_sgn(_z) < 0;
  }

  long evenp()
  {
    return mpz_even_p(_z);
  }

  long oddp()
  {
    return mpz_odd_p(_z);
  }

  virtual bool eql(Value value) const;

  virtual bool equal(Value value) const;

  virtual unsigned long hash();

  virtual AbstractString * write_to_string();
};

inline bool bignump(Value value)
{
  return (typed_object_p(value)
          && the_typed_object(value)->widetag() == WIDETAG_BIGNUM);
}

inline Bignum * the_bignum(Value value)
{
  assert(bignump(value));
  return reinterpret_cast<Bignum *>(value - LOWTAG_TYPED_OBJECT);
}

inline long compare(Bignum * b1, Bignum * b2)
{
  assert(b1 != NULL);
  assert(b2 != NULL);
  return mpz_cmp(b1->_z, b2->_z);
}

#endif // Bignum.hpp
