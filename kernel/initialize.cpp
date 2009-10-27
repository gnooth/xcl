// initialize.cpp
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

#include "lisp.hpp"
#include "primitives.hpp"
#include "Package.hpp"
#include "Primitive.hpp"
#include "SpecialOperator.hpp"

Value UNBOUND_VALUE;

Value FUNCTION_NAME;
Value UB8_TYPE;
Value UB16_TYPE;
Value UB32_TYPE;
Value BIT_TYPE;

static void add_primitive(Value name, void * code, unsigned int minargs, unsigned int maxargs)
{
  the_symbol(name)->set_function(new Primitive(name, code, minargs, maxargs));
}

// add a primitive that will be redefined in Lisp
static void add_temporary(Value name, void * code, unsigned int minargs, unsigned int maxargs)
{
  the_symbol(name)->set_function(new Primitive(name, code, minargs, maxargs, false));
}

#define ADD_PRIMITIVE(name, code, minargs, maxargs) \
        add_primitive(name, (void *) code, minargs, maxargs)

#define ADD_TEMPORARY(name, code, minargs, maxargs) \
        add_temporary(name, (void *) code, minargs, maxargs)

static void add_special_operator(Value name, SpecialOperatorFunction fname)
{
  the_symbol(name)->set_special_operator(new SpecialOperator(name, fname));
}

static void * mp_alloc(size_t size)
{
  return GC_malloc_atomic(size);
}

static void * mp_realloc(void *p, size_t old_size, size_t new_size)
{
  void *p2 = GC_malloc_atomic(new_size);
  memcpy(p2, p, old_size < new_size ? old_size : new_size);
  return p2;
}

static void mp_free(void *ptr, size_t size)
{
}

static void initialize_features()
{
  Symbol * features = the_symbol(S_features);
  features->initialize_special(list4(K_xcl, K_ansi_cl, K_common_lisp, K_little_endian));

#ifdef __x86_64__
  features->set_value(make_cons(K_x86_64, features->value()));
#else
  features->set_value(make_cons(K_x86, features->value()));
#endif

#ifdef WIN32
  features->set_value(make_cons(K_windows, features->value()));
#endif

#ifdef __FreeBSD__
  features->set_value(make_cons(K_freebsd, features->value()));
  features->set_value(make_cons(K_bsd, features->value()));
  features->set_value(make_cons(K_unix, features->value()));
#endif

#ifdef __NetBSD__
  features->set_value(make_cons(K_netbsd, features->value()));
  features->set_value(make_cons(K_bsd, features->value()));
  features->set_value(make_cons(K_unix, features->value()));
#endif

#ifdef __linux__
  features->set_value(make_cons(K_linux, features->value()));
  features->set_value(make_cons(K_unix, features->value()));
#endif
}

void initialize_lisp()
{
  mp_set_memory_functions(mp_alloc, mp_realloc, mp_free);

  // NIL is a special case.
//   NIL = make_value(new Symbol("NIL"));
  Cons * cons = new Cons(0, 0);
  NIL = make_value(cons);
  cons->setcar(NIL);
  cons->setcdr(NIL);

  initialize_packages_1();

//   assert(the_symbol(NIL)->package() == 0);
//   assert(PACKAGE_CL != 0);
//   the_symbol(NIL)->set_package(make_value(PACKAGE_CL));
//   the_symbol(NIL)->set_constant_value(NIL);
//   PACKAGE_CL->add_external_symbol(the_symbol(NIL));

  initialize_symbols();

  initialize_features();

  initialize_packages_2();

  FUNCTION_NAME =
    list3(S_or, S_symbol,
          list3(S_cons, list2(S_eql, S_setf), list3(S_cons, S_symbol, S_null)));

  UB8_TYPE  = list2(S_unsigned_byte, make_fixnum(8));
  UB16_TYPE = list2(S_unsigned_byte, make_fixnum(16));
  UB32_TYPE = list2(S_unsigned_byte, make_fixnum(32));
  BIT_TYPE         = list3(S_integer, FIXNUM_ZERO, FIXNUM_ONE);

  // Common Lisp primitives
  ADD_PRIMITIVE(S_abs, CL_abs, 1, 1);
  ADD_PRIMITIVE(S_acons, CL_acons, 3, 3);
  ADD_PRIMITIVE(S_adjustable_array_p, CL_adjustable_array_p, 1, 1);
  ADD_PRIMITIVE(S_alpha_char_p, CL_alpha_char_p, 1, 1);
  ADD_PRIMITIVE(S_alphanumericp, CL_alphanumericp, 1, 1);
  ADD_PRIMITIVE(S_append, CL_append, 0, MANY);
  ADD_PRIMITIVE(S_apply, CL_apply, 2, MANY);
  ADD_PRIMITIVE(S_apropos_list, CL_apropos_list, 1, 2);
  ADD_PRIMITIVE(S_aref, CL_aref, 1, MANY);
  ADD_PRIMITIVE(S_arithmetic_error_operands, CL_arithmetic_error_operands, 1, 1);
  ADD_PRIMITIVE(S_arithmetic_error_operation, CL_arithmetic_error_operation, 1, 1);
  ADD_PRIMITIVE(S_array_dimension, CL_array_dimension, 2, 2);
  ADD_PRIMITIVE(S_array_dimensions, CL_array_dimensions, 1, 1);
  ADD_PRIMITIVE(S_array_displacement, CL_array_displacement, 1, 1);
  ADD_PRIMITIVE(S_array_element_type, CL_array_element_type, 1, 1);
  ADD_PRIMITIVE(S_array_has_fill_pointer_p, CL_array_has_fill_pointer_p, 1, 1);
  ADD_PRIMITIVE(S_array_in_bounds_p, CL_array_in_bounds_p, 1, MANY);
  ADD_PRIMITIVE(S_array_rank, CL_array_rank, 1, 1);
  ADD_PRIMITIVE(S_array_row_major_index, CL_array_row_major_index, 1, MANY);
  ADD_PRIMITIVE(S_array_total_size, CL_array_total_size, 1, 1);
  ADD_PRIMITIVE(S_arrayp, CL_arrayp, 1, 1);
  ADD_PRIMITIVE(S_ash, CL_ash, 2, 2);
  ADD_PRIMITIVE(S_atan, CL_atan, 1, 2);
  ADD_PRIMITIVE(S_atom, CL_atom, 1, 1);
  ADD_PRIMITIVE(S_bit_vector_p, CL_bit_vector_p, 1, 1);
  ADD_PRIMITIVE(S_both_case_p, CL_both_case_p, 1, 1);
  ADD_PRIMITIVE(S_broadcast_stream_streams, CL_broadcast_stream_streams, 1, 1);
  ADD_PRIMITIVE(S_byte_position, CL_byte_position, 1, 1);
  ADD_PRIMITIVE(S_byte_size, CL_byte_size, 1, 1);
  ADD_PRIMITIVE(S_byte, CL_byte, 2, 2);
  ADD_PRIMITIVE(S_boundp, CL_boundp, 1, 1);
  ADD_PRIMITIVE(S_caaar, CL_caaar, 1, 1);
  ADD_PRIMITIVE(S_caadr, CL_caadr, 1, 1);
  ADD_PRIMITIVE(S_caar, CL_caar, 1, 1);
  ADD_PRIMITIVE(S_cadar, CL_cadar, 1, 1);
  ADD_PRIMITIVE(S_caddr, CL_caddr, 1, 1);
  ADD_PRIMITIVE(S_caddr, CL_caddr, 1, 1);
  ADD_PRIMITIVE(S_cadr, CL_cadr, 1, 1);
  ADD_PRIMITIVE(S_car, CL_car, 1, 1);
  ADD_PRIMITIVE(S_cdaar, CL_cdaar, 1, 1);
  ADD_PRIMITIVE(S_cdadr, CL_cdadr, 1, 1);
  ADD_PRIMITIVE(S_cdar, CL_cdar, 1, 1);
  ADD_PRIMITIVE(S_cddar, CL_cddar, 1, 1);
  ADD_PRIMITIVE(S_cdddr, CL_cdddr, 1, 1);
  ADD_PRIMITIVE(S_cdddr, CL_cdddr, 1, 1);
  ADD_PRIMITIVE(S_cddr, CL_cddr, 1, 1);
  ADD_PRIMITIVE(S_cdr, CL_cdr, 1, 1);
  ADD_PRIMITIVE(S_ceiling, CL_ceiling, 1, 2);
  ADD_PRIMITIVE(S_cell_error_name, CL_cell_error_name, 1, 1);
  ADD_PRIMITIVE(S_char, CL_char, 2, 2);
  ADD_PRIMITIVE(S_char_code, CL_char_code, 1, 1);
  ADD_PRIMITIVE(S_char_downcase, CL_char_downcase, 1, 1);
  ADD_PRIMITIVE(S_char_equal, CL_char_equal, 1, MANY);
  ADD_PRIMITIVE(S_char_e, CL_char_e, 1, MANY);
  ADD_PRIMITIVE(S_char_ge, CL_char_ge, 1, MANY);
  ADD_PRIMITIVE(S_char_greaterp, CL_char_greaterp, 1, MANY);
  ADD_PRIMITIVE(S_char_not_greaterp, CL_char_not_greaterp, 1, MANY);
  ADD_PRIMITIVE(S_char_lessp, CL_char_lessp, 1, MANY);
  ADD_PRIMITIVE(S_char_ne, CL_char_ne, 1, MANY);
  ADD_PRIMITIVE(S_char_not_equal, CL_char_not_equal, 1, MANY);
  ADD_PRIMITIVE(S_char_not_lessp, CL_char_not_lessp, 1, MANY);
  ADD_PRIMITIVE(S_char_gt, CL_char_gt, 1, MANY);
  ADD_PRIMITIVE(S_char_int, CL_char_int, 1, 1);
  ADD_PRIMITIVE(S_char_le, CL_char_le, 1, MANY);
  ADD_PRIMITIVE(S_char_lt, CL_char_lt, 1, MANY);
  ADD_PRIMITIVE(S_char_name, CL_char_name, 1, 1);
  ADD_PRIMITIVE(S_char_upcase, CL_char_upcase, 1, 1);
  ADD_PRIMITIVE(S_character, CL_character, 1, 1);
  ADD_PRIMITIVE(S_characterp, CL_characterp, 1, 1);
  ADD_PRIMITIVE(S_cis, CL_cis, 1, 1);
  ADD_PRIMITIVE(S_class_of, CL_class_of, 1, 1);
  ADD_PRIMITIVE(S_clear_input, CL_clear_input, 0, 1);
  ADD_PRIMITIVE(S_clear_output, CL_clear_output, 0, 1);
  ADD_PRIMITIVE(S_close, CL_close, 1, 3);
  ADD_PRIMITIVE(S_clrhash, CL_clrhash, 1, 1);
  ADD_PRIMITIVE(S_code_char, CL_code_char, 1, 1);
  ADD_PRIMITIVE(S_compiled_function_p, CL_compiled_function_p, 1, 1);
  ADD_PRIMITIVE(S_complex, CL_complex, 1, 2);
  ADD_PRIMITIVE(S_complexp, CL_complexp, 1, 1);
  ADD_PRIMITIVE(S_concatenated_stream_streams, CL_concatenated_stream_streams, 1, 1);
  ADD_PRIMITIVE(S_cons, CL_cons, 2, 2);
  ADD_PRIMITIVE(S_consp, CL_consp, 1, 1);
  ADD_PRIMITIVE(S_constantp, CL_constantp, 1, 2);
  ADD_PRIMITIVE(S_copy_list, CL_copy_list, 1, 1);
  ADD_PRIMITIVE(S_copy_readtable, CL_copy_readtable, 0, 2);
  ADD_PRIMITIVE(S_copy_structure, CL_copy_structure, 1, 1);
  ADD_PRIMITIVE(S_copy_tree, CL_copy_tree, 1, 1);
  ADD_PRIMITIVE(S_cos, CL_cos, 1, 1);
  ADD_PRIMITIVE(S_decode_float, CL_decode_float, 1, 1);
  ADD_PRIMITIVE(S_delete_file, CL_delete_file, 1, 1);
  ADD_PRIMITIVE(S_slash, CL_divide, 1, MANY);
  ADD_PRIMITIVE(S_denominator, CL_denominator, 1, 1);
  ADD_PRIMITIVE(S_digit_char, CL_digit_char, 1, 2);
  ADD_PRIMITIVE(S_digit_char_p, CL_digit_char_p, 1, 2);
  ADD_PRIMITIVE(S_directory_namestring, CL_directory_namestring, 1, 1);
  ADD_PRIMITIVE(S_echo_stream_input_stream, CL_echo_stream_input_stream, 1, 1);
  ADD_PRIMITIVE(S_echo_stream_output_stream, CL_echo_stream_output_stream, 1, 1);
  ADD_PRIMITIVE(S_elt, CL_elt, 2, 2);
  ADD_PRIMITIVE(S_endp, CL_endp, 1, 1);
  ADD_PRIMITIVE(S_eq, CL_eq, 2, 2);
  ADD_PRIMITIVE(S_eql, CL_eql, 2, 2);
  ADD_PRIMITIVE(S_equal, CL_equal, 2, 2);
  ADD_PRIMITIVE(S_equalp, CL_equalp, 2, 2);
  ADD_PRIMITIVE(S_equals, CL_equals, 1, MANY);
  ADD_TEMPORARY(S_error, CL_error, 1, MANY);
  ADD_PRIMITIVE(S_eval, CL_eval, 1, 1);
  ADD_PRIMITIVE(S_evenp, CL_evenp, 1, 1);
  ADD_PRIMITIVE(S_export, CL_export, 1, MANY);
  ADD_PRIMITIVE(S_exp, CL_exp, 1, 1);
  ADD_PRIMITIVE(S_expt, CL_expt, 2, 2);
  ADD_PRIMITIVE(S_fboundp, CL_fboundp, 1, 1);
  ADD_PRIMITIVE(S_fdefinition, CL_fdefinition, 1, 1);
  ADD_PRIMITIVE(S_file_error_pathname, CL_file_error_pathname, 1, 1);
  ADD_PRIMITIVE(S_file_length, CL_file_length, 1, 1);
  ADD_PRIMITIVE(S_file_namestring, CL_file_namestring, 1, 1);
  ADD_PRIMITIVE(S_file_position, CL_file_position, 1, 2);
  ADD_PRIMITIVE(S_file_string_length, CL_file_string_length, 2, 2);
  ADD_PRIMITIVE(S_file_write_date, CL_file_write_date, 1, 1);
  ADD_PRIMITIVE(S_fill_pointer, CL_fill_pointer, 1, 1);
  ADD_PRIMITIVE(S_find_class, CL_find_class, 1, 3);
  ADD_PRIMITIVE(S_find_package, CL_find_package, 1, 1);
  ADD_PRIMITIVE(S_find_symbol, CL_find_symbol, 1, 2);
  ADD_PRIMITIVE(S_finish_output, CL_finish_output, 0, 1);
  ADD_PRIMITIVE(S_first, CL_first, 1, 1);
  ADD_PRIMITIVE(S_float, CL_float, 1, 2);
  ADD_PRIMITIVE(S_float_digits, CL_float_digits, 1, 1);
  ADD_PRIMITIVE(S_float_radix, CL_float_radix, 1, 1);
  ADD_PRIMITIVE(S_float_sign, CL_float_sign, 1, 2);
  ADD_PRIMITIVE(S_floatp, CL_floatp, 1, 1);
  ADD_PRIMITIVE(S_floor, CL_floor, 1, 2);
  ADD_PRIMITIVE(S_fmakunbound, CL_fmakunbound, 1, 1);
  ADD_PRIMITIVE(S_force_output, CL_force_output, 0, 1);
  ADD_TEMPORARY(S_format, SYS_primitive_format, 2, MANY);
  ADD_PRIMITIVE(S_fourth, CL_fourth, 1, 1);
  ADD_TEMPORARY(S_fresh_line, CL_fresh_line, 0, 1);
  ADD_PRIMITIVE(S_funcall, CL_funcall, 1, MANY);
  ADD_PRIMITIVE(S_function_lambda_expression, CL_function_lambda_expression, 1, 1);
  ADD_PRIMITIVE(S_functionp, CL_functionp, 1, 1);
  ADD_PRIMITIVE(S_ge, CL_ge, 1, MANY);
  ADD_PRIMITIVE(S_gensym, CL_gensym, 0, 1);
  ADD_PRIMITIVE(S_get, CL_get, 2, 3);
  ADD_PRIMITIVE(S_get_dispatch_macro_character, CL_get_dispatch_macro_character, 2, 3);
  ADD_PRIMITIVE(S_get_macro_character, CL_get_macro_character, 1, 2);
  ADD_PRIMITIVE(S_get_output_stream_string, CL_get_output_stream_string, 1, 1);
  ADD_PRIMITIVE(S_get_properties, CL_get_properties, 2, 2);
  ADD_PRIMITIVE(S_getf, CL_getf, 2, 3);
  ADD_PRIMITIVE(S_gethash, CL_gethash, 2, 3);
  ADD_PRIMITIVE(S_get_internal_real_time, CL_get_internal_real_time, 0, 0);
  ADD_PRIMITIVE(S_get_internal_run_time, CL_get_internal_run_time, 0, 0);
  ADD_PRIMITIVE(S_get_universal_time, CL_get_universal_time, 0, 0);
  ADD_PRIMITIVE(S_graphic_char_p, CL_graphic_char_p, 1, 1);
  ADD_PRIMITIVE(S_gt, CL_gt, 1, MANY);
  ADD_PRIMITIVE(S_hash_table_count, CL_hash_table_count, 1, 1);
  ADD_PRIMITIVE(S_hash_table_p, CL_hash_table_p, 1, 1);
  ADD_PRIMITIVE(S_hash_table_rehash_size, CL_hash_table_rehash_size, 1, 1);
  ADD_PRIMITIVE(S_hash_table_rehash_threshold, CL_hash_table_rehash_threshold, 1, 1);
  ADD_PRIMITIVE(S_hash_table_size, CL_hash_table_size, 1, 1);
  ADD_PRIMITIVE(S_hash_table_test, CL_hash_table_test, 1, 1);
  ADD_PRIMITIVE(S_host_namestring, CL_host_namestring, 1, 1);
  ADD_PRIMITIVE(S_identity, CL_identity, 1, 1);
  ADD_PRIMITIVE(S_imagpart, CL_imagpart, 1, 1);
  ADD_PRIMITIVE(S_import, CL_import, 1, 2);
  ADD_PRIMITIVE(S_input_stream_p, CL_input_stream_p, 1, 1);
  ADD_PRIMITIVE(S_integer_decode_float, CL_integer_decode_float, 1, 1);
  ADD_PRIMITIVE(S_integer_length, CL_integer_length, 1, 1);
  ADD_PRIMITIVE(S_integerp, CL_integerp, 1, 1);
  ADD_PRIMITIVE(S_interactive_stream_p, CL_interactive_stream_p, 1, 1);
  ADD_PRIMITIVE(S_intern, CL_intern, 1, 2);
  ADD_PRIMITIVE(S_isqrt, CL_isqrt, 1, 1);
  ADD_PRIMITIVE(S_keywordp, CL_keywordp, 1, 1);
  ADD_PRIMITIVE(S_last, CL_last, 1, 2);
  ADD_PRIMITIVE(S_le, CL_le, 1, MANY);
  ADD_PRIMITIVE(S_length, CL_length, 1, 1);
  ADD_PRIMITIVE(S_lisp_implementation_type, CL_lisp_implementation_type, 0, 0);
  ADD_PRIMITIVE(S_lisp_implementation_version, CL_lisp_implementation_version, 0, 0);
  ADD_PRIMITIVE(S_list, CL_list, 0, MANY);
  ADD_PRIMITIVE(S_list_all_packages, CL_list_all_packages, 0, 0);
  ADD_PRIMITIVE(S_list_star, CL_list_star, 1, MANY);
  ADD_PRIMITIVE(S_listen, CL_listen, 0, 1);
  ADD_PRIMITIVE(S_listp, CL_listp, 1, 1);
  ADD_TEMPORARY(S_load, CL_load, 1, 1);
  ADD_PRIMITIVE(S_log, CL_log, 1, 2);
  ADD_PRIMITIVE(S_logand, CL_logand, 0, MANY);
  ADD_PRIMITIVE(S_logandc1, CL_logandc1, 2, 2);
  ADD_PRIMITIVE(S_logandc2, CL_logandc2, 2, 2);
  ADD_PRIMITIVE(S_logbitp, CL_logbitp, 2, 2);
  ADD_PRIMITIVE(S_logcount, CL_logcount, 1, 1);
  ADD_PRIMITIVE(S_logeqv, CL_logeqv, 0, MANY);
  ADD_PRIMITIVE(S_logior, CL_logior, 0, MANY);
  ADD_PRIMITIVE(S_lognand, CL_lognand, 2, 2);
  ADD_PRIMITIVE(S_lognor, CL_lognor, 2, 2);
  ADD_PRIMITIVE(S_lognot, CL_lognot, 1, 1);
  ADD_PRIMITIVE(S_logorc1, CL_logorc1, 2, 2);
  ADD_PRIMITIVE(S_logorc2, CL_logorc2, 2, 2);
  ADD_PRIMITIVE(S_logtest, CL_logtest, 2, 2);
  ADD_PRIMITIVE(S_logxor, CL_logxor, 0, MANY);
  ADD_PRIMITIVE(S_long_site_name, CL_long_site_name, 0, 0);
  ADD_PRIMITIVE(S_lower_case_p, CL_lower_case_p, 1, 1);
  ADD_PRIMITIVE(S_lt, CL_lt, 1, MANY);
  ADD_PRIMITIVE(S_machine_instance, CL_machine_instance, 0, 0);
  ADD_PRIMITIVE(S_machine_type, CL_machine_type, 0, 0);
  ADD_PRIMITIVE(S_machine_version, CL_machine_version, 0, 0);
  ADD_PRIMITIVE(S_macro_function, CL_macro_function, 1, 2);
  ADD_PRIMITIVE(S_macroexpand, CL_macroexpand, 1, 2);
  ADD_PRIMITIVE(S_macroexpand_1, CL_macroexpand_1, 1, 2);
  ADD_PRIMITIVE(S_make_broadcast_stream, CL_make_broadcast_stream, 0, MANY);
  ADD_PRIMITIVE(S_make_concatenated_stream, CL_make_concatenated_stream, 0, MANY);
  ADD_TEMPORARY(S_make_condition, CL_make_condition, 1, MANY); // redefined in clos.lisp
  ADD_TEMPORARY(S_make_dispatch_macro_character, CL_make_dispatch_macro_character, 1, 3);
  ADD_PRIMITIVE(S_make_echo_stream, CL_make_echo_stream, 2, 2);
  ADD_PRIMITIVE(S_make_pathname, CL_make_pathname, 0, MANY);
  ADD_PRIMITIVE(S_make_random_state, CL_make_random_state, 0, 1);
  ADD_PRIMITIVE(S_make_string_input_stream, CL_make_string_input_stream, 1, 3);
  ADD_PRIMITIVE(S_make_two_way_stream, CL_make_two_way_stream, 2, 2);
  ADD_PRIMITIVE(S_make_symbol, CL_make_symbol, 1, 1);
  ADD_PRIMITIVE(S_make_synonym_stream, CL_make_synonym_stream, 1, 1);
  ADD_PRIMITIVE(S_makunbound, CL_makunbound, 1, 1);
  ADD_PRIMITIVE(S_mapc, CL_mapc, 2, MANY);
  ADD_PRIMITIVE(S_mapcar, CL_mapcar, 2, MANY);
  ADD_PRIMITIVE(S_maphash, CL_maphash, 2, 2);
  ADD_PRIMITIVE(S_max, CL_max, 1, MANY);
  ADD_PRIMITIVE(S_merge_pathnames, CL_merge_pathnames, 1, 3);
  ADD_PRIMITIVE(S_min, CL_min, 1, MANY);
  ADD_PRIMITIVE(S_minus, CL_subtract, 1, MANY);
  ADD_PRIMITIVE(S_minusp, CL_minusp, 1, 1);
  ADD_PRIMITIVE(S_mod, CL_mod, 2, 2);
  ADD_PRIMITIVE(S_name_char, CL_name_char, 1, 1);
  ADD_PRIMITIVE(S_namestring, CL_namestring, 1, 1);
  ADD_PRIMITIVE(S_nconc, CL_nconc, 0, MANY);
  ADD_PRIMITIVE(S_not, CL_not, 1, 1);
  ADD_PRIMITIVE(S_not_equals, CL_not_equals, 1, MANY);
  ADD_PRIMITIVE(S_nreconc, CL_nreconc, 2, 2);
  ADD_PRIMITIVE(S_nreverse, CL_nreverse, 1, 1);
  ADD_PRIMITIVE(S_nth, CL_nth, 2, 2);
  ADD_PRIMITIVE(S_nthcdr, CL_nthcdr, 2, 2);
  ADD_PRIMITIVE(S_null, CL_null, 1, 1);
  ADD_PRIMITIVE(S_numberp, CL_numberp, 1, 1);
  ADD_PRIMITIVE(S_numerator, CL_numerator, 1, 1);
  ADD_PRIMITIVE(S_oddp, CL_oddp, 1, 1);
  ADD_PRIMITIVE(S_one_minus, CL_one_minus, 1, 1);
  ADD_PRIMITIVE(S_one_plus, CL_one_plus, 1, 1);
  ADD_PRIMITIVE(S_open_stream_p, CL_open_stream_p, 1, 1);
  ADD_PRIMITIVE(S_output_stream_p, CL_output_stream_p, 1, 1);
  ADD_PRIMITIVE(S_package_error_package, CL_package_error_package, 1, 1);
  ADD_PRIMITIVE(S_package_name, CL_package_name, 1, 1);
  ADD_PRIMITIVE(S_package_nicknames, CL_package_nicknames, 1, 1);
  ADD_PRIMITIVE(S_package_shadowing_symbols, CL_package_shadowing_symbols, 1, 1);
  ADD_PRIMITIVE(S_package_use_list, CL_package_use_list, 1, 1);
  ADD_PRIMITIVE(S_package_used_by_list, CL_package_used_by_list, 1, 1);
  ADD_PRIMITIVE(S_packagep, CL_packagep, 1, 1);
  ADD_PRIMITIVE(S_pathname, CL_pathname, 1, 1);
  ADD_PRIMITIVE(S_pathname_device, CL_pathname_device, 1, MANY);
  ADD_PRIMITIVE(S_pathname_directory, CL_pathname_directory, 1, MANY);
  ADD_PRIMITIVE(S_pathname_host, CL_pathname_host, 1, MANY);
  ADD_PRIMITIVE(S_pathname_name, CL_pathname_name, 1, MANY);
  ADD_PRIMITIVE(S_pathname_type, CL_pathname_type, 1, MANY);
  ADD_PRIMITIVE(S_pathname_version, CL_pathname_version, 1, 1);
  ADD_PRIMITIVE(S_pathnamep, CL_pathnamep, 1, 1);
  ADD_PRIMITIVE(S_peek_char, CL_peek_char, 0, 5);
  ADD_PRIMITIVE(S_plus, CL_add, 0, MANY);
  ADD_PRIMITIVE(S_plusp, CL_plusp, 1, 1);
  ADD_TEMPORARY(S_prin1, CL_prin1, 1, 2);
  ADD_TEMPORARY(S_prin1_to_string, CL_prin1_to_string, 1, 1);
  ADD_TEMPORARY(S_princ, CL_princ, 1, 2);
  ADD_TEMPORARY(S_princ_to_string, CL_princ_to_string, 1, 1);
  ADD_TEMPORARY(S_print, CL_print, 1, 2);
  ADD_PRIMITIVE(S_print_not_readable_object, CL_print_not_readable_object, 1, 1);
  ADD_PRIMITIVE(S_probe_file, CL_probe_file, 1, 1);
  ADD_PRIMITIVE(S_random, CL_random, 1, 2);
  ADD_PRIMITIVE(S_random_state_p, CL_random_state_p, 1, 1);
  ADD_PRIMITIVE(S_rational, CL_rational, 1, 1);
  ADD_PRIMITIVE(S_rationalize, CL_rationalize, 1, 1);
  ADD_PRIMITIVE(S_rationalp, CL_rationalp, 1, 1);
  ADD_PRIMITIVE(S_read, CL_read, 0, 4);
  ADD_PRIMITIVE(S_read_char, CL_read_char, 0, 4);
  ADD_PRIMITIVE(S_read_char_no_hang, CL_read_char_no_hang, 0, 4);
  ADD_PRIMITIVE(S_read_delimited_list, CL_read_delimited_list, 1, 3);
  ADD_PRIMITIVE(S_read_line, CL_read_line, 0, 4);
  ADD_PRIMITIVE(S_read_preserving_whitespace, CL_read_preserving_whitespace, 0, 4);
  ADD_PRIMITIVE(S_readtable_case, CL_readtable_case, 1, 1);
  ADD_PRIMITIVE(S_readtablep, CL_readtablep, 1, 1);
  ADD_PRIMITIVE(S_realp, CL_realp, 1, 1);
  ADD_PRIMITIVE(S_realpart, CL_realpart, 1, 1);
  ADD_PRIMITIVE(S_rem, CL_rem, 2, 2);
  ADD_PRIMITIVE(S_remhash, CL_remhash, 2, 2);
  ADD_PRIMITIVE(S_remprop, CL_remprop, 2, 2);
  ADD_PRIMITIVE(S_rename_file, CL_rename_file, 2, 2);
  ADD_PRIMITIVE(S_rename_package, CL_rename_package, 2, 3);
  ADD_PRIMITIVE(S_rest, CL_rest, 1, 1);
  ADD_PRIMITIVE(S_restart_name, CL_restart_name, 1, 1);
  ADD_PRIMITIVE(S_reverse, CL_reverse, 1, 1);
  ADD_PRIMITIVE(S_room, CL_room, 0, 1);
  ADD_PRIMITIVE(S_row_major_aref, CL_row_major_aref, 2, 2);
  ADD_PRIMITIVE(S_rplaca, CL_rplaca, 2, 2);
  ADD_PRIMITIVE(S_rplacd, CL_rplacd, 2, 2);
  ADD_PRIMITIVE(S_schar, CL_schar, 2, 2);
  ADD_PRIMITIVE(S_second, CL_second, 1, 1);
  ADD_PRIMITIVE(S_set, CL_set, 2, 2);
  ADD_PRIMITIVE(S_set_dispatch_macro_character, CL_set_dispatch_macro_character, 3, 4);
  ADD_PRIMITIVE(S_set_macro_character, CL_set_macro_character, 2, 4);
  ADD_PRIMITIVE(S_set_syntax_from_char, CL_set_syntax_from_char, 2, 4);
  ADD_PRIMITIVE(S_shadow, CL_shadow, 1, 2);
  ADD_PRIMITIVE(S_shadowing_import, CL_shadowing_import, 1, 2);
  ADD_PRIMITIVE(S_short_site_name, CL_short_site_name, 0, 0);
  ADD_PRIMITIVE(S_signum, CL_signum, 1, 1);
  ADD_PRIMITIVE(S_simple_bit_vector_p, CL_simple_bit_vector_p, 1, 1);
  ADD_PRIMITIVE(S_simple_condition_format_arguments, CL_simple_condition_format_arguments, 1, 1);
  ADD_PRIMITIVE(S_simple_condition_format_control, CL_simple_condition_format_control, 1, 1);
  ADD_PRIMITIVE(S_simple_string_p, CL_simple_string_p, 1, 1);
  ADD_PRIMITIVE(S_simple_vector_p, CL_simple_vector_p, 1, 1);
  ADD_PRIMITIVE(S_sin, CL_sin, 1, 1);
  ADD_PRIMITIVE(S_sleep, CL_sleep, 1, 1);
  ADD_PRIMITIVE(S_software_type, CL_software_type, 0, 0);
  ADD_PRIMITIVE(S_software_version, CL_software_version, 0, 0);
  ADD_PRIMITIVE(S_special_operator_p, CL_special_operator_p, 1, 1);
  ADD_PRIMITIVE(S_special_variable_p, EXT_special_variable_p, 1, 1);
  ADD_PRIMITIVE(S_sqrt, CL_sqrt, 1, 1);
  ADD_PRIMITIVE(S_standard_char_p, CL_standard_char_p, 1, 1);
  ADD_PRIMITIVE(S_star, CL_multiply, 0, MANY);
  ADD_PRIMITIVE(S_stream_element_type, CL_stream_element_type, 1, 1);
  ADD_PRIMITIVE(S_stream_error_stream, CL_stream_error_stream, 1, 1);
  ADD_PRIMITIVE(S_stream_external_format, CL_stream_external_format, 1, 1);
  ADD_PRIMITIVE(S_streamp, CL_streamp, 1, 1);
  ADD_PRIMITIVE(S_string, CL_string, 1, 1);
  ADD_PRIMITIVE(S_stringp, CL_stringp, 1, 1);
  ADD_PRIMITIVE(S_subseq, CL_subseq, 2, 3);
  ADD_PRIMITIVE(S_svref, CL_svref, 2, 2);
  ADD_PRIMITIVE(S_sxhash, CL_sxhash, 1, 1);
  ADD_PRIMITIVE(S_symbol_function, CL_symbol_function, 1, 1);
  ADD_PRIMITIVE(S_symbol_name, CL_symbol_name, 1, 1);
  ADD_PRIMITIVE(S_symbol_package, CL_symbol_package, 1, 1);
  ADD_PRIMITIVE(S_symbol_plist, CL_symbol_plist, 1, 1);
  ADD_PRIMITIVE(S_symbol_value, CL_symbol_value, 1, 1);
  ADD_PRIMITIVE(S_symbolp, CL_symbolp, 1, 1);
  ADD_PRIMITIVE(S_synonym_stream_symbol, CL_synonym_stream_symbol, 1, 1);
  ADD_PRIMITIVE(S_tan, CL_tan, 1, 1);
  ADD_TEMPORARY(S_terpri, CL_terpri, 0, 1);
  ADD_PRIMITIVE(S_third, CL_third, 1, 1);
  ADD_PRIMITIVE(S_truename, CL_truename, 1, 1);
  ADD_PRIMITIVE(S_truncate, CL_truncate, 1, 2);
  ADD_PRIMITIVE(S_type_error_datum, CL_type_error_datum, 1, 1);
  ADD_PRIMITIVE(S_type_error_expected_type, CL_type_error_expected_type, 1, 1);
  ADD_PRIMITIVE(S_type_of, CL_type_of, 1, 1);
  ADD_PRIMITIVE(S_two_way_stream_input_stream, CL_two_way_stream_input_stream, 1, 1);
  ADD_PRIMITIVE(S_two_way_stream_output_stream, CL_two_way_stream_output_stream, 1, 1);
  ADD_PRIMITIVE(S_unbound_slot_instance, CL_unbound_slot_instance, 1, 1);
  ADD_PRIMITIVE(S_unexport, CL_unexport, 1, 2);
  ADD_PRIMITIVE(S_unintern, CL_unintern, 1, 2);
  ADD_PRIMITIVE(S_unread_char, CL_unread_char, 1, 2);
  ADD_PRIMITIVE(S_unuse_package, CL_unuse_package, 1, 2);
  ADD_PRIMITIVE(S_upper_case_p, CL_upper_case_p, 1, 1);
  ADD_PRIMITIVE(S_use_package, CL_use_package, 1, 2);
  ADD_PRIMITIVE(S_user_homedir_pathname, CL_user_homedir_pathname, 0, 1);
  ADD_PRIMITIVE(S_values, CL_values, 0, MANY);
  ADD_PRIMITIVE(S_values_list, CL_values_list, 1, 1);
  ADD_PRIMITIVE(S_vector, CL_vector, 0, MANY);
  ADD_PRIMITIVE(S_vector_pop, CL_vector_pop, 1, 1);
  ADD_PRIMITIVE(S_vector_push, CL_vector_push, 2, 2);
  ADD_PRIMITIVE(S_vector_push_extend, CL_vector_push_extend, 2, 3);
  ADD_PRIMITIVE(S_vectorp, CL_vectorp, 1, 1);
  ADD_PRIMITIVE(S_wild_pathname_p, CL_wild_pathname_p, 1, 2);
  ADD_TEMPORARY(S_write_char, CL_write_char, 1, 2);
  ADD_PRIMITIVE(S_zerop, CL_zerop, 1, 1);

  // non-CL primitives
  ADD_PRIMITIVE(S_accept_connection, EXT_accept_connection, 1, 1);
  ADD_PRIMITIVE(S_add_2, SYS_add_2, 2, 2);
  ADD_PRIMITIVE(S_address_of, SYS_address_of, 1, 1);
  ADD_PRIMITIVE(S_adjust_array_internal, SYS_adjust_array_internal, 10, 10);
  ADD_PRIMITIVE(S_allocate_funcallable_standard_instance, SYS_allocate_funcallable_standard_instance, 1, 1);
  ADD_PRIMITIVE(S_allocate_instance_funcallable_standard_class, SYS_allocate_instance_funcallable_standard_class, 0, 0);
  ADD_PRIMITIVE(S_allocate_instance_standard_class, SYS_allocate_instance_standard_class, 0, 0);
  ADD_PRIMITIVE(S_allocate_instance_standard_generic_function, SYS_allocate_instance_standard_generic_function, 1, 1);
  ADD_PRIMITIVE(S_allocate_standard_instance, SYS_allocate_standard_instance, 1, 1);
  ADD_PRIMITIVE(S_aset, SYS_aset, 2, MANY);
  ADD_PRIMITIVE(S_assq, EXT_assq, 2, 2);
  ADD_PRIMITIVE(S_assql, EXT_assql, 2, 2);
  ADD_PRIMITIVE(S_atan_1, SYS_atan_1, 1, 1);
  ADD_TEMPORARY(S_autocompile, SYS_autocompile, 1, 1);
  ADD_PRIMITIVE(S_autoload, EXT_autoload, 1, 2);
  ADD_PRIMITIVE(S_autoload_macro, EXT_autoload_macro, 1, 2);
  ADD_PRIMITIVE(S_autoloadp, EXT_autoloadp, 1, 1);
  ADD_PRIMITIVE(S_backtrace, EXT_backtrace, 0, 0);
  ADD_PRIMITIVE(S_backtrace_as_list, EXT_backtrace_as_list, 0, 1);
  ADD_PRIMITIVE(S_special_bindings, SYS_special_bindings, 0, 0);
  ADD_PRIMITIVE(S_bignump, EXT_bignump, 1, 1);
  ADD_PRIMITIVE(S_builtin_typep, SYS_builtin_typep, 2, 2);
  ADD_PRIMITIVE(S_canonicalize_logical_host, SYS_canonicalize_logical_host, 1, 1);
  ADD_PRIMITIVE(S_ceiling_1, SYS_ceiling_1, 1, 1);
  ADD_PRIMITIVE(S_ceiling_2, SYS_ceiling_2, 2, 2);
  ADD_PRIMITIVE(S_check_fixnum_bounds, SYS_check_fixnum_bounds, 3, 3);
  ADD_PRIMITIVE(S_check_subsequence, SYS_check_subsequence, 3, 3);
  ADD_PRIMITIVE(S_class_name_internal, SYS_class_name_internal, 1, 1);
  ADD_PRIMITIVE(S_classp, EXT_classp, 1, 1);
  ADD_PRIMITIVE(S_closure_environment, EXT_closure_environment, 1, 1);
  ADD_PRIMITIVE(S_closurep, EXT_closurep, 1, 1);
  ADD_PRIMITIVE(S_coerce_to_function, SYS_coerce_to_function, 1, 1);
  ADD_PRIMITIVE(S_compiled_function_constants, SYS_compiled_function_constants, 1, 1);
  ADD_PRIMITIVE(S_concatenate_to_string, SYS_concatenate_to_string, 1, 1);
  ADD_PRIMITIVE(S_condition_broadcast, EXT_condition_broadcast, 1, 1);
  ADD_PRIMITIVE(S_condition_notify, EXT_condition_notify, 1, 1);
  ADD_PRIMITIVE(S_condition_wait, EXT_condition_wait, 2, 2);
  ADD_PRIMITIVE(S_conditionp, SYS_conditionp, 1, 1);
  ADD_PRIMITIVE(S_copy_string, EXT_copy_string, 1, 1);
  ADD_PRIMITIVE(S_copy_structure_slot_definition, SYS_copy_structure_slot_definition, 1, 1);
  ADD_PRIMITIVE(S_create_new_file, SYS_create_new_file, 1, 1);
  ADD_PRIMITIVE(S_current_thread, EXT_current_thread, 0, 0);
  ADD_PRIMITIVE(S_default_time_zone, SYS_default_time_zone, 0, 0);
  ADD_PRIMITIVE(S_defconstant_internal, SYS_defconstant_internal, 2, 2);
  ADD_PRIMITIVE(S_defpackage_internal, SYS_defpackage_internal, 10, 10);
  ADD_PRIMITIVE(S_defparameter_internal, SYS_defparameter_internal, 4, 4);
  ADD_PRIMITIVE(S_defun_internal, SYS_defun_internal, 2, 2);
  ADD_PRIMITIVE(S_defvar_internal, SYS_defvar_internal, 1, 1);
  ADD_PRIMITIVE(S_delete_package_internal, SYS_delete_package_internal, 1, 1);
  ADD_PRIMITIVE(S_designator_input_stream, SYS_designator_input_stream, 1, 1);
  ADD_PRIMITIVE(S_designator_list, SYS_designator_list, 1, 1);
  ADD_PRIMITIVE(S_designator_output_stream, SYS_designator_output_stream, 1, 1);
  ADD_PRIMITIVE(S_divide_2, SYS_divide_2, 2, 2);
  ADD_PRIMITIVE(S_documentation_internal, SYS_documentation_internal, 2, 2);
  ADD_PRIMITIVE(S_double_float_add_internal, SYS_double_float_add_internal, 2, 2);
  ADD_PRIMITIVE(S_double_float_high_bits, SYS_double_float_high_bits, 1, 1);
  ADD_PRIMITIVE(S_double_float_low_bits, SYS_double_float_low_bits, 1, 1);
  ADD_PRIMITIVE(S_double_float_p, SYS_double_float_p, 1, 1);
  ADD_PRIMITIVE(S_double_float_subtract_internal, SYS_double_float_subtract_internal, 2, 2);
  ADD_PRIMITIVE(S_environment_add_function_definition, SYS_environment_add_function_definition, 3, 3);
  ADD_PRIMITIVE(S_environment_add_macro_definition, SYS_environment_add_macro_definition, 3, 3);
  ADD_PRIMITIVE(S_environment_empty_p, SYS_environment_empty_p, 1, 1);
  ADD_PRIMITIVE(S_environment_variables, EXT_environment_variables, 1, 1);
  ADD_PRIMITIVE(S_equals_2, SYS_equals_2, 2, 2);
  ADD_PRIMITIVE(S_error_not_list, SYS_error_not_list, 1, 1);
  ADD_PRIMITIVE(S_exit, EXT_exit, 0, 0);
  ADD_PRIMITIVE(S_fasl_read_backquote, SYS_fasl_read_backquote, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_comma, SYS_fasl_read_comma, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_comment, SYS_fasl_read_comment, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_dispatch_char, SYS_fasl_read_dispatch_char, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_list, SYS_fasl_read_list, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_quote, SYS_fasl_read_quote, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_right_paren, SYS_fasl_read_right_paren, 2, 2);
  ADD_PRIMITIVE(S_fasl_read_string, SYS_fasl_read_string, 2, 2);
  ADD_PRIMITIVE(S_fasl_sharp_a, SYS_fasl_sharp_a, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_b, SYS_fasl_sharp_b, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_backslash, SYS_fasl_sharp_backslash, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_c, SYS_fasl_sharp_c, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_colon, SYS_fasl_sharp_colon, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_dollar, SYS_fasl_sharp_dollar, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_dot, SYS_fasl_sharp_dot, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_illegal, SYS_fasl_sharp_illegal, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_left_paren, SYS_fasl_sharp_left_paren, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_o, SYS_fasl_sharp_o, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_p, SYS_fasl_sharp_p, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_percent, SYS_fasl_sharp_percent, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_quote, SYS_fasl_sharp_quote, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_r, SYS_fasl_sharp_r, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_s, SYS_fasl_sharp_s, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_star, SYS_fasl_sharp_star, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_vertical_bar, SYS_fasl_sharp_vertical_bar, 3, 3);
  ADD_PRIMITIVE(S_fasl_sharp_x, SYS_fasl_sharp_x, 3, 3);
  ADD_PRIMITIVE(S_fast_mapc2, SYS_fast_mapc2, 2, 2);
  ADD_PRIMITIVE(S_fast_mapcar2, SYS_fast_mapcar2, 2, 2);
  ADD_PRIMITIVE(S_fdefinition_block_name, SYS_fdefinition_block_name, 1, 1);
  ADD_PRIMITIVE(S_file_directory_p, EXT_file_directory_p, 1, 1);
  ADD_PRIMITIVE(S_find_class_1, SYS_find_class_1, 1, 1);
  ADD_PRIMITIVE(S_find_eql, SYS_find_eql, 2, 2);
  ADD_PRIMITIVE(S_fixnum_typep, SYS_fixnum_typep, 3, 3);
  ADD_PRIMITIVE(S_fixnump, EXT_fixnump, 1, 1);
  ADD_PRIMITIVE(S_float_string, SYS_float_string, 1, 1);
  ADD_PRIMITIVE(S_floor_1, SYS_floor_1, 1, 1);
  ADD_PRIMITIVE(S_floor_2, SYS_floor_2, 2, 2);
  ADD_PRIMITIVE(S_funcallable_instance_function, SYS_funcallable_instance_function, 1, 1);
  ADD_PRIMITIVE(S_function_arity, SYS_function_arity, 1, 1);
  ADD_PRIMITIVE(S_function_call_count, SYS_function_call_count, 1, 1);
  ADD_PRIMITIVE(S_function_code, SYS_function_code, 1, 1);
  ADD_PRIMITIVE(S_function_code_size, SYS_function_code_size, 1, 1);
  ADD_PRIMITIVE(S_function_name, SYS_function_name, 1, 1);
  ADD_PRIMITIVE(S_function_plist, SYS_function_plist, 1, 1);
  ADD_PRIMITIVE(S_gc, EXT_gc, 0, 0);
  ADD_PRIMITIVE(S_gc_total_bytes, SYS_gc_total_bytes, 0, 0);
  ADD_PRIMITIVE(S_gc_total_cons_cells, SYS_gc_total_cons_cells, 0, 0);
  ADD_PRIMITIVE(S_gcd_2, SYS_gcd_2, 2, 2);
  ADD_PRIMITIVE(S_ge_2, SYS_ge_2, 2, 2);
  ADD_PRIMITIVE(S_get2, SYS_get2, 2, 2);
  ADD_PRIMITIVE(S_get3, SYS_get3, 3, 3);
  ADD_PRIMITIVE(S_get_mutex, EXT_get_mutex, 1, 3);
  ADD_PRIMITIVE(S_get_process_times, SYS_get_process_times, 0, 0);
  ADD_PRIMITIVE(S_getenv, EXT_getenv, 1, 1);
  ADD_PRIMITIVE(S_gethash2, SYS_gethash2, 2, 2);
  ADD_PRIMITIVE(S_gethash2_1, SYS_gethash2_1, 2, 2);
  ADD_PRIMITIVE(S_gethash3, SYS_gethash3, 3, 3);
  ADD_PRIMITIVE(S_getpid, EXT_getpid, 0, 0);
  ADD_PRIMITIVE(S_gt_2, SYS_gt_2, 2, 2);
  ADD_PRIMITIVE(S_hash_table_entries, SYS_hash_table_entries, 1, 1);
  ADD_PRIMITIVE(S_heap_free, SYS_heap_free, 0, 0);
  ADD_PRIMITIVE(S_heap_size, SYS_heap_size, 0, 0);
  ADD_PRIMITIVE(S_heap_used, SYS_heap_used, 0, 0);
  ADD_PRIMITIVE(S_in_package_internal, SYS_in_package_internal, 1, 1);
  ADD_PRIMITIVE(S_init_fasl, SYS_init_fasl, 1, 1);
  ADD_PRIMITIVE(S_int3, SYS_int3, 0, 0);
  ADD_PRIMITIVE(S_interactive_eval, SYS_interactive_eval, 1, 1);
  ADD_PRIMITIVE(S_interrupt_lisp, SYS_interrupt_lisp, 0, 0);
  ADD_PRIMITIVE(S_intersection_eql, SYS_intersection_eql, 2, 2);
  ADD_PRIMITIVE(S_iref, SYS_iref, 2, 2);
  ADD_PRIMITIVE(S_iset, SYS_iset, 3, 3);
  ADD_PRIMITIVE(S_kernel_function_p, SYS_kernel_function_p, 1, 1);
  ADD_PRIMITIVE(S_lambda_list_names, SYS_lambda_list_names, 1, 1);
  ADD_PRIMITIVE(S_lambda_expression_p, SYS_lambda_expression_p, 1, 1);
  ADD_PRIMITIVE(S_last1, SYS_last1, 1, 1);
  ADD_PRIMITIVE(S_layout_class, SYS_layout_class, 1, 1);
  ADD_PRIMITIVE(S_layout_invalid_p, SYS_layout_invalid_p, 1, 1);
  ADD_PRIMITIVE(S_layout_slot_location, SYS_layout_slot_location, 2, 2);
  ADD_PRIMITIVE(S_layout_slot_names, SYS_layout_slot_names, 1, 1);
  ADD_PRIMITIVE(S_le_2, SYS_le_2, 2, 2);
  ADD_PRIMITIVE(S_length_eql, SYS_length_eql, 2, 2);
  ADD_PRIMITIVE(S_list1, SYS_list1, 1, 1);
  ADD_PRIMITIVE(S_list2, SYS_list2, 2, 2);
  ADD_PRIMITIVE(S_list3, SYS_list3, 3, 3);
  ADD_PRIMITIVE(S_list4, SYS_list4, 4, 4);
  ADD_PRIMITIVE(S_list5, SYS_list5, 5, 5);
  ADD_PRIMITIVE(S_list_delete_eq, SYS_list_delete_eq, 2, 2);
  ADD_PRIMITIVE(S_list_delete_eql, SYS_list_delete_eql, 2, 2);
  ADD_PRIMITIVE(S_list_directory, SYS_list_directory, 1, 1);
  ADD_PRIMITIVE(S_list_find_eq, SYS_list_find_eq, 2, 2);
  ADD_PRIMITIVE(S_list_find_eql, SYS_list_find_eql, 2, 2);
  ADD_PRIMITIVE(S_list_position_eql, SYS_list_position_eql, 2, 2);
  ADD_PRIMITIVE(S_load_stream, SYS_load_stream, 4, 4);
  ADD_PRIMITIVE(S_load_system_file, SYS_load_system_file, 1, 1);
  ADD_PRIMITIVE(S_local_port, EXT_local_port, 1, 1);
  ADD_PRIMITIVE(S_logical_pathname_p, SYS_logical_pathname_p, 1, 1);
  ADD_PRIMITIVE(S_lt_2, SYS_lt_2, 2, 2);
  ADD_PRIMITIVE(S_make_array_internal, SYS_make_array_internal, 9, 9);
  ADD_PRIMITIVE(S_make_closure_template_function, SYS_make_closure_template_function, 5, 5);
  ADD_PRIMITIVE(S_make_code_vector, SYS_make_code_vector, 1, 1);
  ADD_PRIMITIVE(S_make_compiled_function, SYS_make_compiled_function, 5, 5);
  ADD_PRIMITIVE(S_make_condition_internal, SYS_make_condition_internal, 2, 2);
  ADD_PRIMITIVE(S_make_condition_variable, EXT_make_condition_variable, 0, 0);
  ADD_PRIMITIVE(S_make_environment, SYS_make_environment, 0, 1);
  ADD_PRIMITIVE(S_make_file_stream, SYS_make_file_stream, 5, 5);
  ADD_PRIMITIVE(S_make_fill_pointer_output_stream, SYS_make_fill_pointer_output_stream, 1, 1);
  ADD_PRIMITIVE(S_make_hash_table_internal, SYS_make_hash_table_internal, 4, 4);
  ADD_PRIMITIVE(S_make_instances_obsolete_internal, SYS_make_instances_obsolete_internal, 1, 1);
  ADD_PRIMITIVE(S_make_keyword, SYS_make_keyword, 1, 1);
  ADD_PRIMITIVE(S_make_layout, SYS_make_layout, 3, 3);
  ADD_PRIMITIVE(S_make_list_internal, SYS_make_list_internal, 2, 2);
  ADD_PRIMITIVE(S_make_logical_pathname, SYS_make_logical_pathname, 1, 1);
  ADD_PRIMITIVE(S_make_macro, SYS_make_macro, 2, 2);
  ADD_PRIMITIVE(S_make_mutex_internal, SYS_make_mutex_internal, 1, 1);
  ADD_PRIMITIVE(S_make_package_internal, SYS_make_package_internal, 3, 3);
  ADD_PRIMITIVE(S_make_primitive, SYS_make_primitive, 4, 4);
  ADD_PRIMITIVE(S_make_restart_internal, SYS_make_restart_internal, 5, 5);
  ADD_PRIMITIVE(S_make_simple_vector, SYS_make_simple_vector, 1, 1);
  ADD_PRIMITIVE(S_make_slime_input_stream, EXT_make_slime_input_stream, 2, 2);
  ADD_PRIMITIVE(S_make_slime_output_stream, EXT_make_slime_output_stream, 1, 1);
  ADD_PRIMITIVE(S_make_socket_internal, SYS_make_socket_internal, 7, 7);
  ADD_PRIMITIVE(S_make_string_internal, SYS_make_string_internal, 3, 3);
  ADD_PRIMITIVE(S_make_string_output_stream_internal, SYS_make_string_output_stream_internal, 1, 1);
  ADD_PRIMITIVE(S_make_structure_class, SYS_make_structure_class, 3, 3);
  ADD_PRIMITIVE(S_make_structure_slot_definition_internal, SYS_make_structure_slot_definition_internal, 6, 6);
  ADD_PRIMITIVE(S_make_thread_internal, SYS_make_thread_internal, 2, 2);
  ADD_PRIMITIVE(S_mapc2, SYS_mapc2, 2, 2);
  ADD_PRIMITIVE(S_mapcar2, SYS_mapcar2, 2, 2);
  ADD_PRIMITIVE(S_maybe_load_system_file, SYS_maybe_load_system_file, 1, 1);
  ADD_PRIMITIVE(S_member_internal, SYS_member_internal, 5, 5);
  ADD_PRIMITIVE(S_memq, EXT_memq, 2, 2);
  ADD_PRIMITIVE(S_memql, EXT_memql, 2, 2);
  ADD_PRIMITIVE(S_mkdir, SYS_mkdir, 1, 1);
  ADD_PRIMITIVE(S_mref_32, SYS_mref_32, 2, 2);
  ADD_PRIMITIVE(S_mref_32_signed, SYS_mref_32_signed, 2, 2);
#ifdef __x86_64__
  ADD_PRIMITIVE(S_mref_64, SYS_mref_64, 2, 2);
#endif
  ADD_PRIMITIVE(S_mref_8, SYS_mref_8, 2, 2);
  ADD_PRIMITIVE(S_mref_8_signed, SYS_mref_8_signed, 2, 2);
  ADD_PRIMITIVE(S_multiply_2, SYS_multiply_2, 2, 2);
  ADD_PRIMITIVE(S_neq, EXT_neq, 2, 2);
  ADD_PRIMITIVE(S_not_equals_2, SYS_not_equals_2, 2, 2);
  ADD_PRIMITIVE(S_nstring_capitalize_internal, SYS_nstring_capitalize_internal, 3, 3);
  ADD_PRIMITIVE(S_nstring_downcase_internal, SYS_nstring_downcase_internal, 3, 3);
  ADD_PRIMITIVE(S_nstring_upcase_internal, SYS_nstring_upcase_internal, 3, 3);
  ADD_PRIMITIVE(S_package_external_symbols, SYS_package_external_symbols, 1, 1);
  ADD_PRIMITIVE(S_package_inherited_symbols, SYS_package_inherited_symbols, 1, 1);
  ADD_PRIMITIVE(S_package_internal_symbols, SYS_package_internal_symbols, 1, 1);
  ADD_PRIMITIVE(S_package_symbols, SYS_package_symbols, 1, 1);
  ADD_PRIMITIVE(S_parse_body, SYS_parse_body, 1, 2);
  ADD_PRIMITIVE(S_parse_namestring_internal, SYS_parse_namestring_internal, 3, 3);
  ADD_PRIMITIVE(S_position_eql, SYS_position_eql, 2, 2);
  ADD_PRIMITIVE(S_primitive_format, SYS_primitive_format, 2, MANY);
  ADD_PRIMITIVE(S_write_to_string_internal, SYS_write_to_string_internal, 1, 1);
  ADD_PRIMITIVE(S_probe_directory, EXT_probe_directory, 1, 1);
  ADD_PRIMITIVE(S_proclaim_special, SYS_proclaim_special, 1, 1);
  ADD_PRIMITIVE(S_psxhash, SYS_psxhash, 1, 1);
  ADD_PRIMITIVE(S_put, SYS_put, 3, 4);
  ADD_PRIMITIVE(S_putf, SYS_putf, 3, 3);
  ADD_PRIMITIVE(S_puthash, SYS_puthash, 3, 4);
  ADD_PRIMITIVE(S_puthash3, SYS_puthash3, 3, 3);
  ADD_PRIMITIVE(S_puthash4, SYS_puthash4, 4, 4);
  ADD_PRIMITIVE(S_quit, EXT_quit, 0, 0);
  ADD_PRIMITIVE(S_quoted_form_p, SYS_quoted_form_p, 1, 1);
  ADD_PRIMITIVE(S_ratiop, SYS_ratiop, 1, 1);
  ADD_PRIMITIVE(S_read_8_bits, SYS_read_8_bits, 1, 3);
  ADD_PRIMITIVE(S_read_backquote, SYS_read_backquote, 2, 2);
  ADD_PRIMITIVE(S_read_comma, SYS_read_comma, 2, 2);
  ADD_PRIMITIVE(S_read_comment, SYS_read_comment, 2, 2);
  ADD_PRIMITIVE(S_read_dispatch_char, SYS_read_dispatch_char, 2, 2);
  ADD_PRIMITIVE(S_read_from_string_internal, SYS_read_from_string_internal, 6, 6);
  ADD_PRIMITIVE(S_read_list, SYS_read_list, 2, 2);
  ADD_PRIMITIVE(S_read_quote, SYS_read_quote, 2, 2);
  ADD_PRIMITIVE(S_read_right_paren, SYS_read_right_paren, 2, 2);
  ADD_PRIMITIVE(S_read_string, SYS_read_string, 2, 2);
  ADD_PRIMITIVE(S_real_acos, SYS_real_acos, 1, 1);
  ADD_PRIMITIVE(S_real_acosh, SYS_real_acosh, 1, 1);
  ADD_PRIMITIVE(S_real_asin, SYS_real_asin, 1, 1);
  ADD_PRIMITIVE(S_real_asinh, SYS_real_asinh, 1, 1);
  ADD_PRIMITIVE(S_real_atanh, SYS_real_atanh, 1, 1);
  ADD_PRIMITIVE(S_real_cosh, SYS_real_cosh, 1, 1);
  ADD_PRIMITIVE(S_real_sinh, SYS_real_sinh, 1, 1);
  ADD_PRIMITIVE(S_real_tanh, SYS_real_tanh, 1, 1);
  ADD_PRIMITIVE(S_record_source_information, SYS_record_source_information, 1, 1);
  ADD_PRIMITIVE(S_release_mutex, EXT_release_mutex, 1, 1);
  ADD_PRIMITIVE(S_require_boolean, SYS_require_boolean, 1, 1);
  ADD_PRIMITIVE(S_require_character, SYS_require_character, 1, 1);
  ADD_PRIMITIVE(S_require_cons, SYS_require_cons, 1, 1);
  ADD_PRIMITIVE(S_require_fixnum, SYS_require_fixnum, 1, 1);
  ADD_PRIMITIVE(S_require_hash_table, SYS_require_hash_table, 1, 1);
  ADD_PRIMITIVE(S_require_integer, SYS_require_integer, 1, 1);
  ADD_PRIMITIVE(S_require_keyword, SYS_require_keyword, 1, 1);
  ADD_PRIMITIVE(S_require_list, SYS_require_list, 1, 1);
  ADD_PRIMITIVE(S_require_number, SYS_require_number, 1, 1);
  ADD_PRIMITIVE(S_require_simple_string, SYS_require_simple_string, 1, 1);
  ADD_PRIMITIVE(S_require_simple_vector, SYS_require_simple_vector, 1, 1);
  ADD_PRIMITIVE(S_require_stream, SYS_require_stream, 1, 1);
  ADD_PRIMITIVE(S_require_string, SYS_require_string, 1, 1);
  ADD_PRIMITIVE(S_require_structure_type, SYS_require_structure_type, 2, 2);
  ADD_PRIMITIVE(S_require_symbol, SYS_require_symbol, 1, 1);
  ADD_PRIMITIVE(S_require_ub32, SYS_require_ub32, 1, 1);
  ADD_PRIMITIVE(S_require_vector, SYS_require_vector, 1, 1);
  ADD_PRIMITIVE(S_reset, EXT_reset, 0, 0);
  ADD_PRIMITIVE(S_resolve, EXT_resolve, 1, 1);
  ADD_PRIMITIVE(S_restart_function, SYS_restart_function, 1, 1);
  ADD_PRIMITIVE(S_restart_report_function, SYS_restart_report_function, 1, 1);
  ADD_PRIMITIVE(S_restart_interactive_function, SYS_restart_interactive_function, 1, 1);
  ADD_PRIMITIVE(S_restart_test_function, SYS_restart_test_function, 1, 1);
  ADD_PRIMITIVE(S_row_major_aset, SYS_row_major_aset, 3, 3);
  ADD_PRIMITIVE(S_run_program, EXT_run_program, 2, 2);
  ADD_PRIMITIVE(S_sbit1, SYS_sbit1, 2, 2);
  ADD_PRIMITIVE(S_sequencep, EXT_sequencep, 1, 1);
  ADD_PRIMITIVE(S_set_char, SYS_set_char, 3, 3);
  ADD_PRIMITIVE(S_set_documentation_internal, SYS_set_documentation_internal, 3, 3);
  ADD_PRIMITIVE(S_set_fdefinition, SYS_set_fdefinition, 2, 2);
  ADD_PRIMITIVE(S_set_fill_pointer, SYS_set_fill_pointer, 2, 2);
  ADD_PRIMITIVE(S_set_find_class, SYS_set_find_class, 2, 2);
  ADD_PRIMITIVE(S_set_funcallable_instance_function, MOP_set_funcallable_instance_function, 2, 2);
  ADD_PRIMITIVE(S_set_function_call_count, SYS_set_function_call_count, 2, 2);
  ADD_PRIMITIVE(S_set_function_code_size, SYS_set_function_code_size, 2, 2);
  ADD_PRIMITIVE(S_set_function_plist, SYS_set_function_plist, 2, 2);
  ADD_PRIMITIVE(S_set_macro_function, SYS_set_macro_function, 2, 3);
  ADD_PRIMITIVE(S_set_nth, SYS_set_nth, 3, 3);
  ADD_PRIMITIVE(S_set_readtable_case, SYS_set_readtable_case, 2, 2);
  ADD_PRIMITIVE(S_set_sbit1, SYS_set_sbit1, 3, 3);
  ADD_PRIMITIVE(S_set_schar, SYS_set_schar, 3, 3);
  ADD_PRIMITIVE(S_set_slot_index, SYS_set_slot_index, 2, 2);
  ADD_PRIMITIVE(S_set_slot_initform, SYS_set_slot_initform, 2, 2);
  ADD_PRIMITIVE(S_set_slot_reader, SYS_set_slot_reader, 2, 2);
  ADD_PRIMITIVE(S_set_std_instance_layout, SYS_set_std_instance_layout, 2, 2);
  ADD_PRIMITIVE(S_set_std_instance_slot_value, SYS_set_std_instance_slot_value, 3, 3);
  ADD_PRIMITIVE(S_set_symbol_function, SYS_set_symbol_function, 2, 2);
  ADD_PRIMITIVE(S_set_symbol_global_value, SYS_set_symbol_global_value, 2, 2);
  ADD_PRIMITIVE(S_set_symbol_plist, SYS_set_symbol_plist, 2, 2);
  ADD_PRIMITIVE(S_setcar, SYS_setcar, 2, 2);
  ADD_PRIMITIVE(S_setcdr, SYS_setcdr, 2, 2);
  ADD_PRIMITIVE(S_setelt, SYS_setelt, 3, 3);
  ADD_PRIMITIVE(S_setf_function_name_p, SYS_setf_function_name_p, 1, 1);
  ADD_PRIMITIVE(S_sharp_a, SYS_sharp_a, 3, 3);
  ADD_PRIMITIVE(S_sharp_b, SYS_sharp_b, 3, 3);
  ADD_PRIMITIVE(S_sharp_backslash, SYS_sharp_backslash, 3, 3);
  ADD_PRIMITIVE(S_sharp_c, SYS_sharp_c, 3, 3);
  ADD_PRIMITIVE(S_sharp_colon, SYS_sharp_colon, 3, 3);
  ADD_PRIMITIVE(S_sharp_dot, SYS_sharp_dot, 3, 3);
  ADD_PRIMITIVE(S_sharp_illegal, SYS_sharp_illegal, 3, 3);
  ADD_PRIMITIVE(S_sharp_left_paren, SYS_sharp_left_paren, 3, 3);
  ADD_PRIMITIVE(S_sharp_o, SYS_sharp_o, 3, 3);
  ADD_PRIMITIVE(S_sharp_p, SYS_sharp_p, 3, 3);
  ADD_PRIMITIVE(S_sharp_quote, SYS_sharp_quote, 3, 3);
  ADD_PRIMITIVE(S_sharp_r, SYS_sharp_r, 3, 3);
  ADD_PRIMITIVE(S_sharp_s, SYS_sharp_s, 3, 3);
  ADD_PRIMITIVE(S_sharp_star, SYS_sharp_star, 3, 3);
  ADD_PRIMITIVE(S_sharp_vertical_bar, SYS_sharp_vertical_bar, 3, 3);
  ADD_PRIMITIVE(S_sharp_x, SYS_sharp_x, 3, 3);
  ADD_PRIMITIVE(S_simple_array_p, SYS_simple_array_p, 1, 1);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_and, SYS_simple_bit_vector_bit_and, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_andc1, SYS_simple_bit_vector_bit_andc1, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_andc2, SYS_simple_bit_vector_bit_andc2, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_eqv, SYS_simple_bit_vector_bit_eqv, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_ior, SYS_simple_bit_vector_bit_ior, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_nand, SYS_simple_bit_vector_bit_nand, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_nor, SYS_simple_bit_vector_bit_nor, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_not, SYS_simple_bit_vector_bit_not, 2, 2);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_orc1, SYS_simple_bit_vector_bit_orc1, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_orc2, SYS_simple_bit_vector_bit_orc2, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_bit_xor, SYS_simple_bit_vector_bit_xor, 3, 3);
  ADD_PRIMITIVE(S_simple_bit_vector_fill, SYS_simple_bit_vector_fill, 2, 2);
  ADD_PRIMITIVE(S_single_float_bits, SYS_single_float_bits, 1, 1);
  ADD_PRIMITIVE(S_single_float_p, SYS_single_float_p, 1, 1);
  ADD_PRIMITIVE(S_slot_index, SYS_slot_index, 1, 1);
  ADD_PRIMITIVE(S_slot_initform, SYS_slot_initform, 1, 1);
  ADD_PRIMITIVE(S_slot_name, SYS_slot_name, 1, 1);
  ADD_PRIMITIVE(S_slot_read_only_p, SYS_slot_read_only_p, 1, 1);
  ADD_PRIMITIVE(S_slot_reader, SYS_slot_reader, 1, 1);
  ADD_PRIMITIVE(S_slot_type, SYS_slot_type, 1, 1);
  ADD_PRIMITIVE(S_standard_object_p, SYS_standard_object_p, 1, 1);
  ADD_PRIMITIVE(S_start_profiler, SYS_start_profiler, 1, 1);
  ADD_PRIMITIVE(S_std_instance_layout, SYS_std_instance_layout, 1, 1);
  ADD_PRIMITIVE(S_std_instance_slot_value, SYS_std_instance_slot_value, 2, 2);
  ADD_PRIMITIVE(S_profiler_sample_count, SYS_profiler_sample_count, 0, 0);
  ADD_PRIMITIVE(S_stop_profiler, SYS_stop_profiler, 0, 0);
  ADD_PRIMITIVE(S_stream_charpos_internal, SYS_stream_charpos_internal, 1, 1);
  ADD_PRIMITIVE(S_stream_fresh_line_internal, SYS_stream_fresh_line_internal, 1, 1);
  ADD_PRIMITIVE(S_stream_princ_internal, SYS_stream_princ_internal, 2, 2);
  ADD_PRIMITIVE(S_stream_set_charpos_internal, SYS_stream_set_charpos_internal, 2, 2);
  ADD_PRIMITIVE(S_stream_terpri_internal, SYS_stream_terpri_internal, 1, 1);
  ADD_PRIMITIVE(S_stream_write_char_internal, SYS_stream_write_char_internal, 2, 2);
  ADD_PRIMITIVE(S_stream_write_object_internal, SYS_stream_write_object_internal, 2, 2);
  ADD_PRIMITIVE(S_stream_write_string_internal, SYS_stream_write_string_internal, 4, 4);
  ADD_PRIMITIVE(S_string_capitalize_internal, SYS_string_capitalize_internal, 3, 3);
  ADD_PRIMITIVE(S_string_cmp, SYS_string_cmp, 6, 6);
  ADD_PRIMITIVE(S_string_compare, SYS_string_compare, 6, 6);
  ADD_PRIMITIVE(S_string_downcase_internal, SYS_string_downcase_internal, 3, 3);
  ADD_PRIMITIVE(S_string_find, SYS_string_find, 2, 2);
  ADD_PRIMITIVE(S_string_input_stream_current, SYS_string_input_stream_current, 1, 1);
  ADD_PRIMITIVE(S_string_upcase_internal, SYS_string_upcase_internal, 3, 3);
  ADD_PRIMITIVE(S_structure_object_p, SYS_structure_object_p, 1, 1);
  ADD_PRIMITIVE(S_structure_ref, SYS_structure_ref, 2, 2);
  ADD_PRIMITIVE(S_structure_set, SYS_structure_set, 3, 3);
  ADD_PRIMITIVE(S_structure_typep, SYS_structure_typep, 2, 2);
  ADD_PRIMITIVE(S_subclassp, EXT_subclassp, 2, 2);
  ADD_PRIMITIVE(S_subseq2, SYS_subseq2, 2, 2);
  ADD_PRIMITIVE(S_subseq3, SYS_subseq3, 3, 3);
  ADD_PRIMITIVE(S_subsetp_eql, SYS_subsetp_eql, 2, 2);
  ADD_PRIMITIVE(S_subtract_2, SYS_subtract_2, 2, 2);
  ADD_PRIMITIVE(S_svset, SYS_svset, 3, 3);
  ADD_PRIMITIVE(S_swap_slots, SYS_swap_slots, 2, 2);
  ADD_PRIMITIVE(S_symbol_flags, SYS_symbol_flags, 1, 1);
  ADD_PRIMITIVE(S_symbol_global_value, SYS_symbol_global_value, 1, 1);
  ADD_PRIMITIVE(S_thread_name, EXT_thread_name, 1, 1);
  ADD_PRIMITIVE(S_threadp, EXT_threadp, 1, 1);
  ADD_TEMPORARY(S_trace_redefined_update, SYS_trace_redefined_update, 2, 2);
  ADD_PRIMITIVE(S_truncate_1, SYS_truncate_1, 1, 1);
  ADD_PRIMITIVE(S_truncate_2, SYS_truncate_2, 2, 2);
  ADD_PRIMITIVE(S_two_arg_append, SYS_two_arg_append, 2, 2);
  ADD_PRIMITIVE(S_two_arg_char_e, SYS_two_arg_char_e, 2, 2);
  ADD_PRIMITIVE(S_two_arg_char_ge, SYS_two_arg_char_ge, 2, 2);
  ADD_PRIMITIVE(S_two_arg_char_gt, SYS_two_arg_char_gt, 2, 2);
  ADD_PRIMITIVE(S_two_arg_char_le, SYS_two_arg_char_le, 2, 2);
  ADD_PRIMITIVE(S_two_arg_char_lt, SYS_two_arg_char_lt, 2, 2);
  ADD_PRIMITIVE(S_two_arg_char_ne, SYS_two_arg_char_ne, 2, 2);
  ADD_PRIMITIVE(S_two_arg_logand, SYS_two_arg_logand, 2, 2);
  ADD_PRIMITIVE(S_two_arg_logior, SYS_two_arg_logior, 2, 2);
  ADD_PRIMITIVE(S_two_arg_logxor, SYS_two_arg_logxor, 2, 2);
  ADD_PRIMITIVE(S_two_arg_max, SYS_two_arg_max, 2, 2);
  ADD_PRIMITIVE(S_two_arg_min, SYS_two_arg_min, 2, 2);
  ADD_PRIMITIVE(S_type_error_internal, SYS_type_error_internal, 2, 2);
  ADD_PRIMITIVE(S_untraced_function, SYS_untraced_function, 1, 1);
  ADD_PRIMITIVE(S_upgraded_array_element_type_internal, SYS_upgraded_array_element_type_internal, 1, 1);
#ifdef __x86_64__
  ADD_PRIMITIVE(S_value_to_ub64, SYS_value_to_ub64, 1, 1);
#else
  ADD_PRIMITIVE(S_value_to_ub32, SYS_value_to_ub32, 1, 1);
#endif
  ADD_PRIMITIVE(S_vector2, SYS_vector2, 2, 2);
  ADD_PRIMITIVE(S_vector3, SYS_vector3, 3, 3);
  ADD_PRIMITIVE(S_vector_data, SYS_vector_data, 1, 1);
  ADD_PRIMITIVE(S_vector_length, SYS_vector_length, 1, 1);
  ADD_PRIMITIVE(S_vector_length_internal, SYS_vector_length_internal, 1, 1);
  ADD_PRIMITIVE(S_vector_position_eql, SYS_vector_position_eql, 2, 2);
  ADD_PRIMITIVE(S_vector_push_extend_2, SYS_vector_push_extend_2, 2, 2);
  ADD_PRIMITIVE(S_vector_push_extend_3, SYS_vector_push_extend_3, 3, 3);
  ADD_PRIMITIVE(S_vector_ref, SYS_vector_ref, 2, 2);
  ADD_PRIMITIVE(S_vector_set, SYS_vector_set, 3, 3);
  ADD_PRIMITIVE(S_whitespacep, SYS_whitespacep, 1, 1);
  ADD_PRIMITIVE(S_write_8_bits, SYS_write_8_bits, 2, 2);
  ADD_PRIMITIVE(S_xcaddr, SYS_xcaddr, 1, 1);
  ADD_PRIMITIVE(S_xcadr, SYS_xcadr, 1, 1);
  ADD_PRIMITIVE(S_xcar, SYS_xcar, 1, 1);
  ADD_PRIMITIVE(S_xcddr, SYS_xcddr, 1, 1);
  ADD_PRIMITIVE(S_xcdr, SYS_xcdr, 1, 1);
  ADD_PRIMITIVE(S_xclass_precedence_list, SYS_xclass_precedence_list, 1, 1);
  ADD_PRIMITIVE(S_xgethash2_1, SYS_xgethash2_1, 2, 2);
  ADD_PRIMITIVE(S_xinspected_parts, SYS_xinspected_parts, 1, 1);
  ADD_PRIMITIVE(S_xlist_length, SYS_xlist_length, 1, 1);
  ADD_PRIMITIVE(S_xmake_structure, SYS_xmake_structure, 3, 3);
  ADD_PRIMITIVE(S_xrplaca, SYS_xrplaca, 2, 2);
  ADD_PRIMITIVE(S_xrplacd, SYS_xrplacd, 2, 2);
  ADD_PRIMITIVE(S_xschar, SYS_xschar, 2, 2);
  ADD_PRIMITIVE(S_xsetcar, SYS_xsetcar, 2, 2);
  ADD_PRIMITIVE(S_xsetcdr, SYS_xsetcdr, 2, 2);
  ADD_PRIMITIVE(S_xstandard_char_p, SYS_xstandard_char_p, 1, 1);
  ADD_PRIMITIVE(S_xsvref, SYS_xsvref, 2, 2);
  ADD_PRIMITIVE(S_xsvset, SYS_xsvset, 3, 3);
  ADD_PRIMITIVE(S_xvector_ref, SYS_xvector_ref, 2, 2);
  ADD_PRIMITIVE(S_xvector_set, SYS_xvector_set, 3, 3);
  ADD_PRIMITIVE(S_xxmake_structure, SYS_xxmake_structure, 2, 2);

  // Special operators and macros.
  add_special_operator(S_and, CL_and);
//   add_special_operator(S_assert, CL_assert);
  add_special_operator(S_aver, EXT_aver);
  add_special_operator(S_block, CL_block);
  add_special_operator(S_case, CL_case);
  add_special_operator(S_catch, CL_catch);
  add_special_operator(S_cond, CL_cond);
  add_special_operator(S_defmacro, CL_defmacro);
  add_special_operator(S_defun, CL_defun);
  add_special_operator(S_defvar, CL_defvar);
  add_special_operator(S_do, CL_do);
  add_special_operator(S_do_star, CL_do_star);
  add_special_operator(S_dolist, CL_dolist);
  add_special_operator(S_dotimes, CL_dotimes);
  add_special_operator(S_ecase, CL_ecase);
  add_special_operator(S_eval_when, CL_eval_when);
  add_special_operator(S_flet, CL_flet);
  add_special_operator(S_function, CL_function);
  add_special_operator(S_go, CL_go);
  add_special_operator(S_if, CL_if);
  add_special_operator(S_incq, SYS_incq); // experimental
  add_special_operator(S_labels, CL_labels);
  add_special_operator(S_lambda, CL_lambda);
  add_special_operator(S_let, CL_let);
  add_special_operator(S_let_star, CL_let_star);
  add_special_operator(S_load_time_value, CL_load_time_value);
  add_special_operator(S_locally, CL_locally);
  add_special_operator(S_macrolet, CL_macrolet);
  add_special_operator(S_multiple_value_bind, CL_multiple_value_bind);
  add_special_operator(S_multiple_value_call, CL_multiple_value_call);
  add_special_operator(S_multiple_value_list, CL_multiple_value_list);
  add_special_operator(S_multiple_value_prog1, CL_multiple_value_prog1);
  add_special_operator(S_or, CL_or);
  add_special_operator(S_progn, CL_progn);
  add_special_operator(S_progv, CL_progv);
  add_special_operator(S_quote, CL_quote);
  add_special_operator(S_return, CL_return);
  add_special_operator(S_return_from, CL_return_from);
  add_special_operator(S_setq, CL_setq);
  add_special_operator(S_symbol_macrolet, CL_symbol_macrolet);
  add_special_operator(S_tagbody, CL_tagbody);
  add_special_operator(S_the, CL_the);
  add_special_operator(S_throw, CL_throw);
  add_special_operator(S_truly_the, SYS_truly_the);
  add_special_operator(S_unless, CL_unless);
  add_special_operator(S_unwind_protect, CL_unwind_protect);
  add_special_operator(S_when, CL_when);

  initialize_classes();

  extern void initialize_runtime();
  initialize_runtime();

  extern void initialize_threads();
  initialize_threads();
}
