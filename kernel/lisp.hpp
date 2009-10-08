// lisp.hpp
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

#ifndef __LISP_HPP
#define __LISP_HPP

#include <setjmp.h>

#if !defined(WIN32) && !defined(__CYGWIN__)
#include <pthread.h>
#endif

#ifdef WIN32
#define GC_WIN32_THREADS
#include "../gc/include/gc_cpp.h"
#else
#define GC_PTHREADS
#define GC_LINUX_THREADS
#define _REENTRANT
// #include <gc/gc_cpp.h>
#include "../gc/include/gc_cpp.h"
#endif

#include "platform.hpp"

#ifdef _MSC_VER
#pragma warning(push)
// "warning C4146: unary minus operator applied to unsigned type, result still unsigned"
#pragma warning(disable : 4146)
#endif
#include "../gmp/gmp.h"
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#if 1
// let gc clean up
#define MPZ_CLEAR(x)
#define MPQ_CLEAR(x)
#define MPF_CLEAR(x)
#else
#define MPZ_CLEAR(x)    mpz_clear(x)
#define MPQ_CLEAR(x)    mpq_clear(x)
#define MPF_CLEAR(x)    mpf_clear(x)
#endif

#include "Value.h"

// class LispObject
class LispObject : public gc
{
};

inline long integer(Value value)
{
  return value;
}

extern long mix(long x, long y);
extern unsigned long hash(Value value);
extern unsigned long equalp_hash(Value value);

extern Value NIL;
extern Value T;

#include "symbols.hpp"

// Tagged objects.

// #ifdef __x86_64__
// #define LOWTAG_BITS 4
// #define LOWTAG_MASK 15
// #define FIXNUM_SHIFT 3
// #define FIXNUM_TAG_MASK 7
// #define ALIGNED __attribute__ ((aligned(16)))
// #else
#define LOWTAG_BITS 3
#define LOWTAG_MASK 7
#define FIXNUM_SHIFT 2
#define FIXNUM_TAG_MASK 3
#define CHARACTER_SHIFT 3
#define ALIGNED
// #endif

#ifdef __x86_64__
#define BYTES_PER_WORD 8
#define BITS_PER_WORD 64
const long MOST_POSITIVE_FIXNUM = 2305843009213693951L; // REVIEW
const long MOST_NEGATIVE_FIXNUM = -2305843009213693952L; // REVIEW
#else
// x86
#define BYTES_PER_WORD 4
#define BITS_PER_WORD 32
const long MOST_POSITIVE_FIXNUM = 536870911;
const long MOST_NEGATIVE_FIXNUM = -536870912;
#endif

const unsigned int CALL_ARGUMENTS_LIMIT = 256; // REVIEW

#define MANY    CALL_ARGUMENTS_LIMIT

const long LAMBDA_PARAMETERS_LIMIT = 256; // REVIEW

typedef unsigned int BIT;
typedef unsigned char BYTE;
typedef unsigned long INDEX;

extern Value signal_type_error(Value datum, Value expectedType);
extern Value bad_index(Value datum, Value expectedType);
extern Value bad_index(INDEX index, long min, long max);

const INDEX ARRAY_DIMENSION_LIMIT = MOST_POSITIVE_FIXNUM;
const INDEX ARRAY_TOTAL_SIZE_LIMIT = MOST_POSITIVE_FIXNUM;
const INDEX ARRAY_RANK_LIMIT = 8; // REVIEW

const int CHAR_CODE_LIMIT = 256;

typedef unsigned char BASE_CHAR;

// lowtags
const long LOWTAG_EVEN_FIXNUM                   =   0;
const long LOWTAG_LIST                          =   1;
const long LOWTAG_TYPED_OBJECT                  =   2;
const long LOWTAG_ODD_FIXNUM                    =   4;
const long LOWTAG_NULL_VALUE                    =   5;
const long LOWTAG_CHARACTER                     =   6;
const long LOWTAG_SYMBOL                        =   7;

// widetags
const long WIDETAG_BIGNUM                       =   8;
const long WIDETAG_RATIO                        =   9;
const long WIDETAG_SINGLE_FLOAT                 =  10;
const long WIDETAG_DOUBLE_FLOAT                 =  11;
const long WIDETAG_COMPLEX                      =  12;

// const long WIDETAG_FUNCTION                     =  13;
const long WIDETAG_PACKAGE                      =  14;
const long WIDETAG_HASH_TABLE                   =  15;
const long WIDETAG_STREAM                       =  16;
const long WIDETAG_SPECIAL_OPERATOR             =  17;
const long WIDETAG_THREAD                       =  18;
// const long WIDETAG_CONDITION                  =  19;
const long WIDETAG_ENVIRONMENT                  =  20;
const long WIDETAG_STRUCTURE_OBJECT             =  21;
// const long WIDETAG_CLOSURE                      =  22;
// const long WIDETAG_AUTOLOAD                     =  23;
// const long WIDETAG_MACRO                        =  24;
const long WIDETAG_RANDOM_STATE                 =  25;
// const long WIDETAG_PRIMITIVE                    =  26;
const long WIDETAG_PATHNAME                     =  27;
const long WIDETAG_STRING_INPUT_STREAM          =  28;
const long WIDETAG_STRING_OUTPUT_STREAM         =  29;
// const long WIDETAG_BUILT_IN_CLASS             =  30;
// const long WIDETAG_STRUCTURE_CLASS            =  31;
// const long WIDETAG_STANDARD_CLASS             =  32;
const long WIDETAG_SYMBOL_MACRO                 =  33;
const long WIDETAG_STRUCTURE_SLOT_DEFINITION    =  34;
const long WIDETAG_FILE_STREAM                  =  35;
// const long WIDETAG_STANDARD_GENERIC_FUNCTION  =  36;
const long WIDETAG_STANDARD_METHOD              =  37;
// const long WIDETAG_COMPILED_CLOSURE             =  38;
const long WIDETAG_LAYOUT                       =  39;
// const long WIDETAG_STANDARD_OBJECT            =  40;
const long WIDETAG_UNBOUND_MARKER               =  41;
const long WIDETAG_BROADCAST_STREAM             =  42;
const long WIDETAG_TWO_WAY_STREAM               =  43;
const long WIDETAG_SOCKET_STREAM                =  44;
const long WIDETAG_SERVER_SOCKET                =  45;
const long WIDETAG_CONCATENATED_STREAM          =  46;
const long WIDETAG_ECHO_STREAM                  =  47;
const long WIDETAG_SYNONYM_STREAM               =  48;
const long WIDETAG_RESTART                      =  49;
const long WIDETAG_READTABLE                    =  50;
// const long WIDETAG_COMPILED_FUNCTION            =  51;
const long WIDETAG_MUTEX                        =  52;
const long WIDETAG_CONDITION_VARIABLE           =  53;
const long WIDETAG_LOGICAL_PATHNAME             =  54;
const long WIDETAG_SLIME_INPUT_STREAM           =  55;
const long WIDETAG_SLIME_OUTPUT_STREAM          =  56;

const long WIDETAG_ARRAY_BIT                    =   64;
const long WIDETAG_SIMPLE_ARRAY_BIT             =  128;
const long WIDETAG_VECTOR_BIT                   =  256;
const long WIDETAG_INSTANCE_BIT                 =  512;
const long WIDETAG_CLASS_BIT                    = 1024;
const long WIDETAG_FUNCTION_BIT                 = 2048;

const long WIDETAG_VECTOR                       = WIDETAG_ARRAY_BIT + WIDETAG_VECTOR_BIT;

const long WIDETAG_ARRAY_T                      = WIDETAG_ARRAY_BIT + 1;
const long WIDETAG_SIMPLE_ARRAY_T               = WIDETAG_ARRAY_BIT + WIDETAG_SIMPLE_ARRAY_BIT + 1;

const long WIDETAG_STRING                       = WIDETAG_VECTOR + 1;
const long WIDETAG_VECTOR_T                     = WIDETAG_VECTOR + 2;
const long WIDETAG_VECTOR_UB8                   = WIDETAG_VECTOR + 3;
const long WIDETAG_VECTOR_UB16                  = WIDETAG_VECTOR + 4;
const long WIDETAG_VECTOR_UB32                  = WIDETAG_VECTOR + 5;
const long WIDETAG_BIT_VECTOR                   = WIDETAG_VECTOR + 6;

const long WIDETAG_SIMPLE_STRING                = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 1;
const long WIDETAG_SIMPLE_VECTOR                = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 2;
const long WIDETAG_SIMPLE_ARRAY_UB8_1           = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 3;
const long WIDETAG_SIMPLE_ARRAY_UB16_1          = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 4;
const long WIDETAG_SIMPLE_ARRAY_UB32_1          = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 5;
const long WIDETAG_SIMPLE_BIT_VECTOR            = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 6;
const long WIDETAG_NIL_VECTOR                   = WIDETAG_VECTOR + WIDETAG_SIMPLE_ARRAY_BIT + 7;

const long WIDETAG_STANDARD_OBJECT              = WIDETAG_INSTANCE_BIT + 1;
const long WIDETAG_CONDITION                    = WIDETAG_INSTANCE_BIT + 2;
// const long WIDETAG_FUNCALLABLE_STANDARD_OBJECT  = WIDETAG_INSTANCE_BIT + 3;
// const long WIDETAG_STANDARD_GENERIC_FUNCTION    = WIDETAG_INSTANCE_BIT + 4;

const long WIDETAG_BUILT_IN_CLASS               = WIDETAG_INSTANCE_BIT + WIDETAG_CLASS_BIT + 1;
const long WIDETAG_STRUCTURE_CLASS              = WIDETAG_INSTANCE_BIT + WIDETAG_CLASS_BIT + 2;
const long WIDETAG_STANDARD_CLASS               = WIDETAG_INSTANCE_BIT + WIDETAG_CLASS_BIT + 3;
const long WIDETAG_FUNCALLABLE_STANDARD_CLASS   = WIDETAG_INSTANCE_BIT + WIDETAG_CLASS_BIT + 4;

const long WIDETAG_FUNCTION                     = WIDETAG_FUNCTION_BIT + 1;
const long WIDETAG_PRIMITIVE                    = WIDETAG_FUNCTION_BIT + 2;
const long WIDETAG_CLOSURE                      = WIDETAG_FUNCTION_BIT + 3;
const long WIDETAG_AUTOLOAD                     = WIDETAG_FUNCTION_BIT + 4;
const long WIDETAG_MACRO                        = WIDETAG_FUNCTION_BIT + 5;
const long WIDETAG_FUNCALLABLE_STANDARD_OBJECT  = WIDETAG_FUNCTION_BIT + WIDETAG_INSTANCE_BIT + 6;
const long WIDETAG_STANDARD_GENERIC_FUNCTION    = WIDETAG_FUNCTION_BIT + WIDETAG_INSTANCE_BIT + 7;
const long WIDETAG_COMPILED_CLOSURE             = WIDETAG_FUNCTION_BIT + 8;
const long WIDETAG_COMPILED_FUNCTION            = WIDETAG_FUNCTION_BIT + 9;

typedef long TYPECODE;

// typecodes
const TYPECODE TYPECODE_FIXNUM                    = 0;

const TYPECODE TYPECODE_LIST                      = LOWTAG_LIST;
const TYPECODE TYPECODE_CHARACTER                 = LOWTAG_CHARACTER;
const TYPECODE TYPECODE_SYMBOL                    = LOWTAG_SYMBOL;

const TYPECODE TYPECODE_BIGNUM                    = WIDETAG_BIGNUM;
const TYPECODE TYPECODE_RATIO                     = WIDETAG_RATIO;
const TYPECODE TYPECODE_SINGLE_FLOAT              = WIDETAG_SINGLE_FLOAT;
const TYPECODE TYPECODE_DOUBLE_FLOAT              = WIDETAG_DOUBLE_FLOAT;
const TYPECODE TYPECODE_COMPLEX                   = WIDETAG_COMPLEX;

const TYPECODE TYPECODE_FUNCTION                  = WIDETAG_FUNCTION;
const TYPECODE TYPECODE_PACKAGE                   = WIDETAG_PACKAGE;
const TYPECODE TYPECODE_HASH_TABLE                = WIDETAG_HASH_TABLE;
const TYPECODE TYPECODE_STREAM                    = WIDETAG_STREAM;
const TYPECODE TYPECODE_SPECIAL_OPERATOR          = WIDETAG_SPECIAL_OPERATOR;
const TYPECODE TYPECODE_THREAD                    = WIDETAG_THREAD;
const TYPECODE TYPECODE_CONDITION                 = WIDETAG_CONDITION;
const TYPECODE TYPECODE_ENVIRONMENT               = WIDETAG_ENVIRONMENT;
const TYPECODE TYPECODE_STRUCTURE_OBJECT          = WIDETAG_STRUCTURE_OBJECT;
const TYPECODE TYPECODE_CLOSURE                   = WIDETAG_CLOSURE;
const TYPECODE TYPECODE_AUTOLOAD                  = WIDETAG_AUTOLOAD;
const TYPECODE TYPECODE_MACRO                     = WIDETAG_MACRO;
const TYPECODE TYPECODE_RANDOM_STATE              = WIDETAG_RANDOM_STATE;
const TYPECODE TYPECODE_PRIMITIVE                 = WIDETAG_PRIMITIVE;
const TYPECODE TYPECODE_PATHNAME                  = WIDETAG_PATHNAME;
const TYPECODE TYPECODE_STRING_OUTPUT_STREAM      = WIDETAG_STRING_OUTPUT_STREAM;
// const TYPECODE TYPECODE_CLASS                     = WIDETAG_CLASS;
const TYPECODE TYPECODE_BUILT_IN_CLASS            = WIDETAG_BUILT_IN_CLASS;
const TYPECODE TYPECODE_STRUCTURE_CLASS           = WIDETAG_STRUCTURE_CLASS;
const TYPECODE TYPECODE_SYMBOL_MACRO              = WIDETAG_SYMBOL_MACRO;
const TYPECODE TYPECODE_STRUCTURE_SLOT_DEFINITION = WIDETAG_STRUCTURE_SLOT_DEFINITION;
const TYPECODE TYPECODE_FILE_STREAM               = WIDETAG_FILE_STREAM;
const TYPECODE TYPECODE_STANDARD_GENERIC_FUNCTION = WIDETAG_STANDARD_GENERIC_FUNCTION;
const TYPECODE TYPECODE_STANDARD_METHOD           = WIDETAG_STANDARD_METHOD;
const TYPECODE TYPECODE_COMPILED_CLOSURE          = WIDETAG_COMPILED_CLOSURE;
const TYPECODE TYPECODE_LAYOUT                    = WIDETAG_LAYOUT;
const TYPECODE TYPECODE_STANDARD_OBJECT           = WIDETAG_STANDARD_OBJECT;
const TYPECODE TYPECODE_UNBOUND_MARKER            = WIDETAG_UNBOUND_MARKER;
const TYPECODE TYPECODE_BROADCAST_STREAM          = WIDETAG_BROADCAST_STREAM;
const TYPECODE TYPECODE_TWO_WAY_STREAM            = WIDETAG_TWO_WAY_STREAM;
const TYPECODE TYPECODE_SOCKET_STREAM             = WIDETAG_SOCKET_STREAM;
const TYPECODE TYPECODE_SERVER_SOCKET             = WIDETAG_SERVER_SOCKET;
const TYPECODE TYPECODE_CONCATENATED_STREAM       = WIDETAG_CONCATENATED_STREAM;
const TYPECODE TYPECODE_ECHO_STREAM               = WIDETAG_ECHO_STREAM;
const TYPECODE TYPECODE_SYNONYM_STREAM            = WIDETAG_SYNONYM_STREAM;

const TYPECODE TYPECODE_MUTEX                     = WIDETAG_MUTEX;
const TYPECODE TYPECODE_CONDITION_VARIABLE        = WIDETAG_CONDITION_VARIABLE;

const TYPECODE TYPECODE_STRING                    = WIDETAG_STRING;
const TYPECODE TYPECODE_SIMPLE_STRING             = WIDETAG_SIMPLE_STRING;
const TYPECODE TYPECODE_SIMPLE_VECTOR             = WIDETAG_SIMPLE_VECTOR;
const TYPECODE TYPECODE_VECTOR_T                  = WIDETAG_VECTOR_T;
const TYPECODE TYPECODE_VECTOR_UB8                = WIDETAG_VECTOR_UB8;
const TYPECODE TYPECODE_SIMPLE_BIT_VECTOR         = WIDETAG_SIMPLE_BIT_VECTOR;
const TYPECODE TYPECODE_BIT_VECTOR                = WIDETAG_BIT_VECTOR;
const TYPECODE TYPECODE_NIL_VECTOR                = WIDETAG_NIL_VECTOR;
const TYPECODE TYPECODE_SIMPLE_ARRAY_UB8_1        = WIDETAG_SIMPLE_ARRAY_UB8_1;

#define NULL_VALUE (0 | LOWTAG_NULL_VALUE)

// REVIEW
#define NO_THREAD_LOCAL_VALUE (16 | LOWTAG_NULL_VALUE)

inline Value make_value(LispObject * p, long tag)
{
  assert((((long)p) & LOWTAG_MASK) == 0);
  return (Value) (((long)p) | tag);
}

inline Value make_value(long n, long tag)
{
#ifndef NDEBUG
  if (tag != LOWTAG_EVEN_FIXNUM && tag != LOWTAG_ODD_FIXNUM)
    assert((n & LOWTAG_MASK) == 0);
#endif
  return (Value) (n | tag);
}

inline long lowtag_of(Value value)
{
  return (value & LOWTAG_MASK);
}

// fixnums
inline Value make_fixnum(long n)
{
  assert(n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM);
  return (Value) (n << FIXNUM_SHIFT);
}

inline Value make_unsigned_fixnum(unsigned long n)
{
  assert(n <= (unsigned long)MOST_POSITIVE_FIXNUM);
  return (Value) (n << FIXNUM_SHIFT);
}

inline bool fixnump(Value value)
{
  return (value & FIXNUM_TAG_MASK) == 0;
}

inline long fixnum_value(Value value)
{
  if (fixnump(value))
    return value >> FIXNUM_SHIFT;
  signal_type_error(value, S_fixnum);
  // Not reached.
  return 0;
}

inline long xlong(Value value)
{
  assert(fixnump(value));
  return value >> FIXNUM_SHIFT;
}

// const Value FIXNUM_MINUS_ONE = make_fixnum(-1);
// const Value FIXNUM_ZERO      = make_fixnum(0);
// const Value FIXNUM_ONE       = make_fixnum(1);
// const Value FIXNUM_TWO       = make_fixnum(2);
// const Value FIXNUM_TEN       = make_fixnum(10);
#define FIXNUM_MINUS_ONE        make_fixnum(-1)
#define FIXNUM_ZERO             0
#define FIXNUM_ONE              make_fixnum(1)
#define FIXNUM_TWO              make_fixnum(2)
#define FIXNUM_TEN              make_fixnum(10)

extern Value list2(Value arg1, Value arg2);
extern Value list3(Value arg1, Value arg2, Value arg3);
extern Value list4(Value arg1, Value arg2, Value arg3, Value arg4);
extern Value list5(Value arg1, Value arg2, Value arg3, Value arg4, Value arg5);

inline bool indexp(Value value)
{
  if (fixnump(value))
    {
      long n = value >> FIXNUM_SHIFT;
      if (n >= 0)
        return true;
    }
  return false;
}

inline INDEX check_index(Value value)
{
  if (fixnump(value))
    {
      long n = value >> FIXNUM_SHIFT;
      if (n >= 0)
        return n;
    }
  signal_type_error(value, list3(S_integer, make_fixnum(0),
                                 make_fixnum(MOST_POSITIVE_FIXNUM)));
  // Not reached.
  return 0;
}

inline INDEX check_index(Value value, INDEX min, INDEX max)
{
  assert(min >= 0);
  assert(max <= (INDEX) MOST_POSITIVE_FIXNUM); // REVIEW
  if (fixnump(value))
    {
      long n = value >> FIXNUM_SHIFT;
      if (n >= 0)
        {
          INDEX index = n;
          if (index >= min && index <= max)
            return index;
        }
    }
  signal_type_error(value, list3(S_integer, make_fixnum(min),
                                 make_fixnum(max)));
  // Not reached.
  return 0;
}

inline BIT check_bit(Value value)
{
  if (value == 0)
    return 0;
  if (value == (1 << FIXNUM_SHIFT))
    return 1;
  signal_type_error(value, S_bit);
  // Not reached.
  return 0;
}

// characters
inline Value make_character(BASE_CHAR c)
{
  return (Value) (c << LOWTAG_BITS) | LOWTAG_CHARACTER;
}

inline bool characterp(Value value)
{
  return lowtag_of(value) == LOWTAG_CHARACTER;
}

inline BASE_CHAR char_value(Value value)
{
  if (characterp(value))
      return value >> LOWTAG_BITS;
  signal_type_error(value, S_character);
  // Not reached.
  return 0;
}

inline BASE_CHAR xchar(Value value)
{
  assert(characterp(value));
  return value >> LOWTAG_BITS;
}

inline bool standard_char_p(Value value)
{
  if (characterp(value))
    {
      BASE_CHAR c = xchar(value);
      if (c >= 32 && c < 127)
        return true;
      if (c == 10)
        return true;
    }
  return false;
}

#include "TypedObject.hpp"
#include "Operator.hpp"
#include "Function.hpp"
#include "Symbol.hpp"
#include "Autoload.hpp"
#include "Cons.hpp"

inline Value list1(Value arg)
{
  return make_cons(arg);
}

inline bool null(Value obj)
{
  return obj == NIL;
}

inline bool listp(Value value)
{
  return (value & LOWTAG_MASK) == LOWTAG_LIST;
}

inline Cons * the_list(Value obj)
{
  assert(listp(obj));
  // internally, NIL is implemented as a cons
  return reinterpret_cast<Cons *>(obj - LOWTAG_LIST);
}

inline Value check_list(Value obj)
{
  if (listp(obj))
    return obj;
  return signal_type_error(obj, S_list);
}

inline void setcar(Value obj, Value car)
{
  assert(consp(obj));
  the_cons(obj)->setcar(car);
}

inline void setcdr(Value obj, Value cdr)
{
  assert(consp(obj));
  the_cons(obj)->setcdr(cdr);
}

inline Value xcar(Value arg)
{
  return the_list(arg)->xcar();
}

inline Value xcdr(Value arg)
{
  return the_list(arg)->xcdr();
}

inline Value xcadr(Value arg)
{
  return xcar(xcdr(arg));
}

inline Value car(Value arg)
{
  if (listp(arg))
    return xcar(arg);
  signal_type_error(arg, S_list);
  // not reached
  return 0;
}

inline Value cdr(Value arg)
{
  if (listp(arg))
    return xcdr(arg);
  signal_type_error(arg, S_list);
  // not reached
  return 0;
}

extern bool equal(Value first, Value second);
extern bool equalp(Value first, Value second);

#include "Bignum.hpp"
// #include "Complex.hpp"
#include "SingleFloat.hpp"
#include "DoubleFloat.hpp"

inline bool float_p(Value value)
{
  if (typed_object_p(value))
    {
      long widetag = the_typed_object(value)->widetag();
      if (widetag == WIDETAG_SINGLE_FLOAT || widetag == WIDETAG_DOUBLE_FLOAT)
        return true;
    }
  return false;
}

inline Value SingleFloat::add(DoubleFloat * df) const
{
  return make_value(new DoubleFloat(_f + df->_d));
}

inline Value DoubleFloat::add(SingleFloat * sf) const
{
  return make_value(new DoubleFloat(_d + sf->_f));
}

inline Value SingleFloat::subtract(DoubleFloat * df) const
{
  return make_value(new DoubleFloat(_f - df->_d));
}

inline Value DoubleFloat::subtract(SingleFloat * sf) const
{
  return make_value(new DoubleFloat(_d - sf->_f));
}

inline bool eql(Value x, Value y)
{
  if (x == y)
    return true;
  if (typed_object_p(x))
    return the_typed_object(x)->eql(y);
  return false;
}

inline Value make_number(long n)
{
  if (n >= MOST_NEGATIVE_FIXNUM && n <= MOST_POSITIVE_FIXNUM)
    return make_fixnum(n);
  return make_value(new Bignum(n));
}

inline Value make_unsigned_integer(unsigned long n)
{
  if (n <= (unsigned long) MOST_POSITIVE_FIXNUM)
    return make_fixnum(n);
  return make_value(new Bignum(n));
}

inline Value make_ub32(unsigned long n)
{
#ifdef __x86_64__
  return make_fixnum(n);
#else
  return make_unsigned_integer(n);
#endif
}

#define MULTIPLE_VALUES_LIMIT 20

#include "Binding.hpp"
#include "Thread.hpp"

extern Value signal_lisp_error(const char * s);

extern Value wrong_number_of_arguments(Value op, long numargs, long min, long max);

extern Value UB8_TYPE;
extern Value UB16_TYPE;
extern Value UB32_TYPE;
extern Value BIT_TYPE;

#include "AbstractArray.hpp"
#include "AbstractVector.hpp"

extern unsigned long length(Value obj);

#include "SimpleVector.hpp"
#include "Vector_T.hpp"
#include "Vector_UB8.hpp"
#include "Vector_UB16.hpp"

#include "AbstractString.hpp"
#include "SimpleString.hpp"
#include "String.hpp"

#include "AbstractBitVector.hpp"
#include "SimpleBitVector.hpp"
#include "BitVector.hpp"

#include "Stream.hpp"

extern Value make_number(AbstractString * string, int base, Stream * stream);
extern bool digit_char_p(BASE_CHAR c, int radix);

inline bool sequencep(Value value)
{
  return listp(value) || vectorp(value);
}

inline Value make_symbol(AbstractString * name)
{
  return make_value(new Symbol(name));
}

class Environment;

extern Value eval(Value form, Environment * env, Thread * thread);

class UnboundMarker : public TypedObject
{
public:
  UnboundMarker()
    : TypedObject(WIDETAG_UNBOUND_MARKER)
  {
  }

  virtual AbstractString * write_to_string()
  {
    return new_simple_string("#<unbound>");
  }
};

extern Value UNBOUND_VALUE;

extern AbstractString * princ_to_string(Value arg);
extern AbstractString * prin1_to_string(Value arg);
extern AbstractString * write_to_string(Value arg);

// #include "Environment.hpp"

// enum { MANY = -2 };

typedef Value (*SpecialOperatorFunction)(Value, Environment *, Thread *);

extern Value progn(Value body, Environment * env, Thread * thread);

#include "classes.hpp"
#include "Error.hpp"

extern Value out_of_memory();

extern void initialize_uptime();
extern double uptime();
extern String * uptime_as_string();

extern void initialize_lisp();
extern void initialize_symbols();

extern TypedObject * coerce_to_function(Value value);
extern bool typep(Value object, Value type);

extern bool memq(Value item, Value list);

extern Value funcall(TypedObject * function, long numargs, Value args[], Thread * thread);

extern bool lt(Value arg1, Value arg2);
extern bool le(Value arg1, Value arg2);
extern bool gt(Value arg1, Value arg2);
extern bool ge(Value arg1, Value arg2);

extern Value gensym(Thread * thread);
extern Value gensym(const char * prefix, Thread * thread);

extern Value parse_body(Value forms, bool doc_string_allowed, Thread * thread);

extern bool is_valid_setf_function_name(Value arg);

extern Value FUNCTION_NAME;

#define UNSPECIFIED S_star

inline bool integerp(Value arg)
{
  return (fixnump(arg) || bignump(arg));
}

extern long constantp(Value value);

extern Value name_to_char(AbstractString * s);

// numbers.cpp
extern bool rationalp(Value value);
extern bool realp(Value value);
extern bool numberp(Value value);
extern Value normalize(mpz_t z);
extern Value normalize(mpq_t z);
extern Value negate(Value arg);
extern bool equals(Value arg1, Value arg2);
extern bool minusp(Value arg);
extern bool plusp(Value arg);
extern Value coerce_to_single_float(Value arg);
extern Value coerce_to_double_float(Value arg);

extern String * format_to_string(Value format_control, Value format_arguments);

extern void initialize_packages_1();
extern void initialize_packages_2();

extern volatile int interrupted;

extern Thread * primordial_thread;

#endif // lisp.hpp
