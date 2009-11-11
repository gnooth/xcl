// classdefs.hpp
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

DEFINE_BUILT_IN_CLASS(array);
DEFINE_BUILT_IN_CLASS(bit_vector);
DEFINE_BUILT_IN_CLASS(broadcast_stream);
DEFINE_BUILT_IN_CLASS(character);
DEFINE_BUILT_IN_CLASS(complex);
DEFINE_BUILT_IN_CLASS(concatenated_stream);
DEFINE_BUILT_IN_CLASS(cons);
DEFINE_BUILT_IN_CLASS(echo_stream);
DEFINE_BUILT_IN_CLASS(environment);
DEFINE_BUILT_IN_CLASS(file_stream);
DEFINE_BUILT_IN_CLASS(float);
DEFINE_BUILT_IN_CLASS(function);
DEFINE_BUILT_IN_CLASS(hash_table);
DEFINE_BUILT_IN_CLASS(integer);
DEFINE_BUILT_IN_CLASS(layout);
DEFINE_BUILT_IN_CLASS(list);
DEFINE_BUILT_IN_CLASS(logical_pathname);
DEFINE_BUILT_IN_CLASS(nil_vector);
DEFINE_BUILT_IN_CLASS(null);
DEFINE_BUILT_IN_CLASS(number);
DEFINE_BUILT_IN_CLASS(package);
DEFINE_BUILT_IN_CLASS(pathname);
DEFINE_BUILT_IN_CLASS(random_state);
DEFINE_BUILT_IN_CLASS(ratio);
DEFINE_BUILT_IN_CLASS(rational);
DEFINE_BUILT_IN_CLASS(readtable);
DEFINE_BUILT_IN_CLASS(real);
DEFINE_BUILT_IN_CLASS(restart);
DEFINE_BUILT_IN_CLASS(sequence);
DEFINE_BUILT_IN_CLASS(server_socket);
DEFINE_BUILT_IN_CLASS(slime_input_stream);
DEFINE_BUILT_IN_CLASS(slime_output_stream);
DEFINE_BUILT_IN_CLASS(socket_stream);
DEFINE_BUILT_IN_CLASS(stream);
DEFINE_BUILT_IN_CLASS(string);
DEFINE_BUILT_IN_CLASS(string_stream);
DEFINE_BUILT_IN_CLASS(symbol);
DEFINE_BUILT_IN_CLASS(synonym_stream);
DEFINE_BUILT_IN_CLASS(t);
DEFINE_BUILT_IN_CLASS(thread);
DEFINE_BUILT_IN_CLASS(two_way_stream);
DEFINE_BUILT_IN_CLASS(vector);

DEFINE_CONDITION_CLASS(arithmetic_error);
DEFINE_CONDITION_CLASS(cell_error);
DEFINE_CONDITION_CLASS(compiler_error);
DEFINE_CONDITION_CLASS(compiler_unsupported_feature_error);
DEFINE_CONDITION_CLASS(condition);
DEFINE_CONDITION_CLASS(control_error);
DEFINE_CONDITION_CLASS(division_by_zero);
DEFINE_CONDITION_CLASS(end_of_file);
DEFINE_CONDITION_CLASS(error);
DEFINE_CONDITION_CLASS(file_error);
DEFINE_CONDITION_CLASS(floating_point_inexact);
DEFINE_CONDITION_CLASS(floating_point_invalid_operation);
DEFINE_CONDITION_CLASS(floating_point_overflow);
DEFINE_CONDITION_CLASS(floating_point_underflow);
DEFINE_CONDITION_CLASS(package_error);
DEFINE_CONDITION_CLASS(parse_error);
DEFINE_CONDITION_CLASS(print_not_readable);
DEFINE_CONDITION_CLASS(program_error);
DEFINE_CONDITION_CLASS(reader_error);
DEFINE_CONDITION_CLASS(serious_condition);
DEFINE_CONDITION_CLASS(simple_condition);
DEFINE_CONDITION_CLASS(simple_error);
DEFINE_CONDITION_CLASS(simple_type_error);
DEFINE_CONDITION_CLASS(simple_warning);
DEFINE_CONDITION_CLASS(storage_condition);
DEFINE_CONDITION_CLASS(stream_error);
DEFINE_CONDITION_CLASS(style_warning);
DEFINE_CONDITION_CLASS(type_error);
DEFINE_CONDITION_CLASS(unbound_slot);
DEFINE_CONDITION_CLASS(unbound_variable);
DEFINE_CONDITION_CLASS(undefined_function);
DEFINE_CONDITION_CLASS(warning);

DEFINE_STANDARD_CLASS(method);
DEFINE_STANDARD_CLASS(standard_method);
DEFINE_STANDARD_CLASS(standard_object);

DEFINE_FUNCALLABLE_STANDARD_CLASS(funcallable_standard_object);
DEFINE_FUNCALLABLE_STANDARD_CLASS(generic_function);
DEFINE_FUNCALLABLE_STANDARD_CLASS(standard_generic_function);

DEFINE_STANDARD_CLASS(metaobject);
DEFINE_STANDARD_CLASS(slot_definition);
DEFINE_STANDARD_CLASS(direct_slot_definition);
DEFINE_STANDARD_CLASS(effective_slot_definition);
DEFINE_STANDARD_CLASS(standard_slot_definition);
DEFINE_STANDARD_CLASS(standard_direct_slot_definition);
DEFINE_STANDARD_CLASS(standard_effective_slot_definition);
DEFINE_STANDARD_CLASS(specializer);
DEFINE_STANDARD_CLASS(eql_specializer);
DEFINE_STANDARD_CLASS(class);
DEFINE_STANDARD_CLASS(built_in_class);
DEFINE_STANDARD_CLASS(forward_referenced_class);
DEFINE_STANDARD_CLASS(standard_class);
DEFINE_STANDARD_CLASS(structure_class);
DEFINE_STANDARD_CLASS(funcallable_standard_class);

DEFINE_STANDARD_CLASS(method_combination);
DEFINE_STANDARD_CLASS(standard_method_combination);
DEFINE_STANDARD_CLASS(short_method_combination);
// DEFINE_STANDARD_CLASS(long_method_combination);

DEFINE_STRUCTURE_CLASS(structure_object);
