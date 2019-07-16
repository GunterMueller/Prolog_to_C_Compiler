%%% global settings


default_setting(output_file, user).
default_setting(entry_point, '$start').
default_setting(library_dir, 'lib').
default_setting(integer_table_index_threshold, 5).  % only used in fact-blocks
default_setting(atom_table_index_threshold, 10).
default_setting(structure_table_index_threshold, 5).
default_setting(dispatch_table_size_factor, 3).
default_setting(literal_fixnum_range, -1073741824 - 1073741823).
default_setting(unify_argument_list_threshold, 3).
default_setting(fact_block_threshold, 5).
