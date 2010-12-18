// initialize_symbols.cpp
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

#include <stdlib.h>     // getenv()

#include <float.h>
#include <math.h>

#include "lisp.hpp"
#include "xcl_home.hpp"
#include "primitives.hpp"
#include "FaslReadtable.hpp"
#include "HashTable.hpp"
#include "LogicalPathname.hpp"
#include "Package.hpp"
#include "Pathname.hpp"
#include "RandomState.hpp"
#include "Readtable.hpp"
#include "SimpleString.hpp"
#include "StructureObject.hpp"
#include "TwoWayStream.hpp"
#include "documentation.hpp"

Value S_nil;

static Value add_keyword(const char * name)
{
  Value value = PACKAGE_KEYWORD->add_external_symbol(name);
  the_symbol(value)->set_constant_value(value);
  return value;
}

#define DEFINE_SYMBOL(identifier, package, name) \
  S_ ## identifier = package->add_external_symbol(name)

#define DEFINE_KEYWORD(identifier, name) \
  K_ ## identifier = add_keyword(name)

void initialize_symbols()
{
  S_nil = PACKAGE_CL->add_nil();

#include "symdefs.hpp"

  the_symbol(S_nil)->initialize_constant(NIL);

  the_symbol(S_t)->initialize_constant(S_t);
  T = S_t;

  the_symbol(S_call_arguments_limit)->initialize_constant(make_fixnum(CALL_ARGUMENTS_LIMIT));
  the_symbol(S_lambda_parameters_limit)->initialize_constant(make_fixnum(LAMBDA_PARAMETERS_LIMIT));
  the_symbol(S_multiple_values_limit)->initialize_constant(make_fixnum(MULTIPLE_VALUES_LIMIT));

  the_symbol(S_most_positive_fixnum)->initialize_constant(make_fixnum(MOST_POSITIVE_FIXNUM));
  the_symbol(S_most_negative_fixnum)->initialize_constant(make_fixnum(MOST_NEGATIVE_FIXNUM));

  the_symbol(S_most_positive_short_float)->initialize_constant(make_single_float(FLT_MAX));
  the_symbol(S_most_positive_single_float)->initialize_constant(make_single_float(FLT_MAX));
  the_symbol(S_most_positive_double_float)->initialize_constant(make_double_float(DBL_MAX));
  the_symbol(S_most_positive_long_float)->initialize_constant(make_double_float(DBL_MAX));

  the_symbol(S_least_positive_short_float)->initialize_constant(make_single_float(FLT_MIN));
  the_symbol(S_least_positive_single_float)->initialize_constant(make_single_float(FLT_MIN));
  the_symbol(S_least_positive_double_float)->initialize_constant(make_double_float(DBL_MIN));
  the_symbol(S_least_positive_long_float)->initialize_constant(make_double_float(DBL_MIN));

  the_symbol(S_least_positive_normalized_short_float)->initialize_constant(make_single_float(FLT_MIN));
  the_symbol(S_least_positive_normalized_single_float)->initialize_constant(make_single_float(FLT_MIN));
  the_symbol(S_least_positive_normalized_double_float)->initialize_constant(make_double_float(DBL_MIN));
  the_symbol(S_least_positive_normalized_long_float)->initialize_constant(make_double_float(DBL_MIN));

  the_symbol(S_most_negative_short_float)->initialize_constant(make_single_float(-FLT_MAX));
  the_symbol(S_most_negative_single_float)->initialize_constant(make_single_float(-FLT_MAX));
  the_symbol(S_most_negative_double_float)->initialize_constant(make_double_float(-DBL_MAX));
  the_symbol(S_most_negative_long_float)->initialize_constant(make_double_float(-DBL_MAX));

  the_symbol(S_least_negative_short_float)->initialize_constant(make_single_float(-FLT_MIN));
  the_symbol(S_least_negative_single_float)->initialize_constant(make_single_float(-FLT_MIN));
  the_symbol(S_least_negative_double_float)->initialize_constant(make_double_float(-DBL_MIN));
  the_symbol(S_least_negative_long_float)->initialize_constant(make_double_float(-DBL_MIN));

  the_symbol(S_least_negative_normalized_short_float)->initialize_constant(make_single_float(-FLT_MIN));
  the_symbol(S_least_negative_normalized_single_float)->initialize_constant(make_single_float(-FLT_MIN));
  the_symbol(S_least_negative_normalized_double_float)->initialize_constant(make_double_float(-DBL_MIN));
  the_symbol(S_least_negative_normalized_long_float)->initialize_constant(make_double_float(-DBL_MIN));

  the_symbol(S_read_default_float_format)->initialize_special(S_single_float);

  the_symbol(S_pi)->initialize_constant(make_double_float(M_PI));

  the_symbol(S_array_dimension_limit)->initialize_constant(make_fixnum(ARRAY_DIMENSION_LIMIT));
  the_symbol(S_array_total_size_limit)->initialize_constant(make_fixnum(ARRAY_TOTAL_SIZE_LIMIT));
  the_symbol(S_array_rank_limit)->initialize_constant(make_fixnum(ARRAY_RANK_LIMIT));

  the_symbol(S_char_code_limit)->initialize_constant(make_fixnum(CHAR_CODE_LIMIT));

#ifdef WIN32
  STANDARD_INPUT = new Stream(DIRECTION_INPUT, FILENO(stdin));
//   STANDARD_INPUT = new Stream(DIRECTION_INPUT, GetStdHandle(STD_INPUT_HANDLE));
  STANDARD_OUTPUT = new Stream(DIRECTION_OUTPUT, GetStdHandle(STD_OUTPUT_HANDLE));
  ERROR_OUTPUT = new Stream(DIRECTION_OUTPUT, GetStdHandle(STD_OUTPUT_HANDLE));
#else
  STANDARD_INPUT = new Stream(DIRECTION_INPUT, FILENO(stdin));
  STANDARD_OUTPUT = new Stream(DIRECTION_OUTPUT, FILENO(stdout));
  ERROR_OUTPUT = new Stream(DIRECTION_OUTPUT, FILENO(stderr)); // REVIEW stdout
#endif

  the_symbol(S_standard_input)->initialize_special(make_value(STANDARD_INPUT));
  the_symbol(S_standard_output)->initialize_special(make_value(STANDARD_OUTPUT));
  the_symbol(S_error_output)->initialize_special(make_value(ERROR_OUTPUT));
  the_symbol(S_trace_output)->initialize_special(make_value(STANDARD_OUTPUT));
  the_symbol(S_terminal_io)->initialize_special(make_value(new TwoWayStream(STANDARD_INPUT, STANDARD_OUTPUT, true)));
  the_symbol(S_query_io)->initialize_special(make_value(new TwoWayStream(STANDARD_INPUT, STANDARD_OUTPUT, true)));
  the_symbol(S_debug_io)->initialize_special(make_value(new TwoWayStream(STANDARD_INPUT, STANDARD_OUTPUT, true)));

  the_symbol(S_print_array)->initialize_special(T);
  the_symbol(S_print_base)->initialize_special(FIXNUM_TEN);
  the_symbol(S_print_case)->initialize_special(K_upcase);
  the_symbol(S_print_circle)->initialize_special(NIL);
  the_symbol(S_print_escape)->initialize_special(T);
  the_symbol(S_print_gensym)->initialize_special(T);
  the_symbol(S_print_length)->initialize_special(NIL);
  the_symbol(S_print_level)->initialize_special(NIL);
  the_symbol(S_print_lines)->initialize_special(NIL);
  the_symbol(S_print_miser_width)->initialize_special(NIL);
  the_symbol(S_print_pprint_dispatch)->initialize_special(NIL);
  the_symbol(S_print_pretty)->initialize_special(NIL);
  the_symbol(S_print_radix)->initialize_special(NIL);
  the_symbol(S_print_readably)->initialize_special(NIL);
  the_symbol(S_print_right_margin)->initialize_special(NIL);
  the_symbol(S_print_structure)->initialize_special(NIL);

  the_symbol(S_current_print_length)->initialize_special(FIXNUM_ZERO);
  the_symbol(S_current_print_level)->initialize_special(FIXNUM_ZERO);

  the_symbol(S_read_base)->initialize_special(FIXNUM_TEN);

  the_symbol(S_current_readtable)->initialize_special(make_value(new Readtable()));
  the_symbol(S_standard_readtable)->initialize_constant(make_value(new Readtable()));

  FASL_READTABLE = new FaslReadtable();
  the_symbol(S_fasl_readtable)->initialize_constant(make_value(FASL_READTABLE));

  the_symbol(S_gensym_counter)->initialize_special(make_fixnum(0));

  the_symbol(S_current_package)->initialize_special(make_value(PACKAGE_CL_USER));

  the_symbol(S_load_verbose)->initialize_special(NIL);
  the_symbol(S_autoload_verbose)->initialize_special(NIL);
  the_symbol(S_load_print)->initialize_special(NIL);
  the_symbol(S_load_pathname)->initialize_special(NIL);
  the_symbol(S_load_truename)->initialize_special(NIL);
  the_symbol(S_source_file)->initialize_special(NIL);
  the_symbol(S_source_position)->initialize_special(NIL);

  the_symbol(S__random_state_)->initialize_special(make_value(new RandomState(NULL)));

  the_symbol(S_read_eval)->initialize_special(T);
  the_symbol(S_read_suppress)->initialize_special(NIL);

  the_symbol(S_star)->initialize_special(NIL);
  the_symbol(S_star_star)->initialize_special(NIL);
  the_symbol(S_star_star_star)->initialize_special(NIL);
  the_symbol(S_plus)->initialize_special(NIL);
  the_symbol(S_plus_plus)->initialize_special(NIL);
  the_symbol(S_plus_plus_plus)->initialize_special(NIL);
  the_symbol(S_slash)->initialize_special(NIL);
  the_symbol(S_slash_slash)->initialize_special(NIL);
  the_symbol(S_slash_slash_slash)->initialize_special(NIL);

  the_symbol(S_debugger_hook)->initialize_special(NIL);
  the_symbol(S_debug_condition)->initialize_special(NIL);
  the_symbol(S_debug_level)->initialize_special(FIXNUM_ZERO);
  the_symbol(S_saved_backtrace)->initialize_special(NIL);
  the_symbol(S_saved_stack)->initialize_special(NIL);

  the_symbol(S_argv)->initialize_special(NIL);

  the_symbol(S_default_pathname_defaults)->initialize_special(get_current_directory());

  the_symbol(S_xcl_home)->initialize_special(make_value(xcl_home_pathname()));

  the_symbol(S_top_level_read_eval_print_loop)->initialize_special(NIL);

  the_symbol(S_fixnum_shift)->initialize_constant(make_fixnum(FIXNUM_SHIFT));
  the_symbol(S_fixnum_tag_mask)->initialize_constant(make_fixnum(FIXNUM_TAG_MASK));
  the_symbol(S_lowtag_mask)->initialize_constant(make_fixnum(LOWTAG_MASK));

  the_symbol(S_character_shift)->initialize_constant(make_fixnum(CHARACTER_SHIFT));
  the_symbol(S_character_lowtag)->initialize_constant(make_fixnum(LOWTAG_CHARACTER));
  the_symbol(S_list_lowtag)->initialize_constant(make_fixnum(LOWTAG_LIST));
  the_symbol(S_symbol_lowtag)->initialize_constant(make_fixnum(LOWTAG_SYMBOL));

  the_symbol(S_typed_object_lowtag)->initialize_constant(make_fixnum(LOWTAG_TYPED_OBJECT));

  the_symbol(S_widetag_stream_bit)->initialize_constant(make_unsigned_integer(WIDETAG_STREAM_BIT));
  the_symbol(S_widetag_vector_bit)->initialize_constant(make_unsigned_integer(WIDETAG_VECTOR_BIT));

  the_symbol(S_simple_string_widetag)->initialize_constant(make_unsigned_integer(WIDETAG_SIMPLE_STRING));
  the_symbol(S_simple_vector_widetag)->initialize_constant(make_unsigned_integer(WIDETAG_SIMPLE_VECTOR));

  the_symbol(S_modules)->initialize_special(NIL);

  UNBOUND_VALUE = make_value(new UnboundMarker());
  the_symbol(S_unbound_marker)->initialize_special(UNBOUND_VALUE);

  the_symbol(S_boole_clr)->initialize_constant(FIXNUM_ZERO);
  the_symbol(S_boole_set)->initialize_constant(FIXNUM_ONE);
  the_symbol(S_boole_1)->initialize_constant(FIXNUM_TWO);
  the_symbol(S_boole_2)->initialize_constant(make_fixnum(3));
  the_symbol(S_boole_c1)->initialize_constant(make_fixnum(4));
  the_symbol(S_boole_c2)->initialize_constant(make_fixnum(5));
  the_symbol(S_boole_and)->initialize_constant(make_fixnum(6));
  the_symbol(S_boole_ior)->initialize_constant(make_fixnum(7));
  the_symbol(S_boole_xor)->initialize_constant(make_fixnum(8));
  the_symbol(S_boole_eqv)->initialize_constant(make_fixnum(9));
  the_symbol(S_boole_nand)->initialize_constant(FIXNUM_TEN);
  the_symbol(S_boole_nor)->initialize_constant(make_fixnum(11));
  the_symbol(S_boole_andc1)->initialize_constant(make_fixnum(12));
  the_symbol(S_boole_andc2)->initialize_constant(make_fixnum(13));
  the_symbol(S_boole_orc1)->initialize_constant(make_fixnum(14));
  the_symbol(S_boole_orc2)->initialize_constant(make_fixnum(15));

  // backquote.lisp
  the_symbol(S_backquote_count)->initialize_special(FIXNUM_ZERO);
  the_symbol(S_backquote_comma_flag)->initialize_constant(list1(make_value(new Symbol(","))));
  the_symbol(S_backquote_at_flag)->initialize_constant(list1(make_value(new Symbol(",@"))));
  the_symbol(S_backquote_dot_flag)->initialize_constant(list1(make_value(new Symbol(",."))));
  the_symbol(S_backquote_vector_flag)->initialize_constant(list1(make_value(new Symbol("bqv"))));

  // reader.lisp
  the_symbol(S_sharp_equal_circle_table)->initialize_special(NIL);
  the_symbol(S_sharp_equal_alist)->initialize_special(NIL);
  the_symbol(S_sharp_sharp_alist)->initialize_special(NIL);

  // trace.lisp
  the_symbol(S_traced_names)->initialize_special(NIL);

#ifdef WIN32
  the_symbol(S_internal_time_units_per_second)->initialize_constant(make_fixnum(10000000));
#else
  the_symbol(S_internal_time_units_per_second)->initialize_constant(make_fixnum(1000000));
#endif

  the_symbol(S_macroexpand_hook)->initialize_special(S_funcall);

  the_symbol(S_compile_verbose)->initialize_special(T);
  the_symbol(S_compile_print)->initialize_special(T);

  the_symbol(S__compile_file_pathname_)->initialize_special(NIL);
  the_symbol(S_compile_file_truename)->initialize_special(NIL);

  the_symbol(S__compilation_speed_)->initialize_special(FIXNUM_ONE);
  the_symbol(S__debug_)->initialize_special(FIXNUM_ONE);
  the_symbol(S__safety_)->initialize_special(FIXNUM_ONE);
  the_symbol(S__space_)->initialize_special(FIXNUM_ONE);
  the_symbol(S__speed_)->initialize_special(FIXNUM_ONE);

  the_symbol(S_print_fasl)->initialize_special(NIL);

  the_symbol(S_simple_string_data_offset)->initialize_constant(make_fixnum(simple_string_data_offset()));
  the_symbol(S_simple_vector_data_offset)->initialize_constant(make_fixnum(simple_vector_data_offset()));
  the_symbol(S_vector_capacity_offset)->initialize_constant(make_fixnum(vector_capacity_offset()));
  the_symbol(S_structure_slots_offset)->initialize_constant(make_fixnum(structure_slots_offset()));
  the_symbol(S_symbol_name_offset)->initialize_constant(make_fixnum(SYMBOL_NAME_OFFSET));
  the_symbol(S_symbol_package_offset)->initialize_constant(make_fixnum(SYMBOL_PACKAGE_OFFSET));
  the_symbol(S_symbol_value_offset)->initialize_constant(make_fixnum(SYMBOL_VALUE_OFFSET));
  the_symbol(S_bytes_per_word)->initialize_constant(make_fixnum(BYTES_PER_WORD));
  the_symbol(S_bits_per_word)->initialize_constant(make_fixnum(BITS_PER_WORD));

  the_symbol(S_common_lisp_package)->initialize_constant(make_value(PACKAGE_CL));
  the_symbol(S_keyword_package)->initialize_constant(make_value(PACKAGE_KEYWORD));

  the_symbol(S_two_arg_operators)->initialize_constant(make_value(new EqHashTable()));

  LOGICAL_PATHNAME_TRANSLATION_TABLE = new EqualHashTable();
  the_symbol(S_logical_pathname_translation_table)->initialize_constant(make_value(LOGICAL_PATHNAME_TRANSLATION_TABLE));

  DOCUMENTATION_HASH_TABLE = new EqHashTable();
  the_symbol(S_documentation_hash_table)->initialize_constant(make_value(DOCUMENTATION_HASH_TABLE));

  // REVIEW
  the_symbol(S_enable_autocompile)->initialize_special(T);
  the_symbol(S_autocompile_verbose)->initialize_special(NIL);

  the_symbol(S_invoke_debugger_hook)->initialize_special(NIL);

  the_symbol(S_widetag_offset)->initialize_constant(make_fixnum(STANDARD_INPUT->widetag_offset()));

  the_symbol(S_samples)->initialize_special(NIL);
  the_symbol(S_sampling_mode)->initialize_special(NIL);
  the_symbol(S_sample_interval)->initialize_special(make_fixnum(10));
  the_symbol(S_max_samples)->initialize_special(NIL);
}
