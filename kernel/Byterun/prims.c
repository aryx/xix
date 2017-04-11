#include "mlvalues.h"
#include "prims.h"
extern value array_get();
extern value array_set();
extern value make_vect();
extern value compare();
extern value equal();
extern value notequal();
extern value lessthan();
extern value lessequal();
extern value greaterthan();
extern value greaterequal();
extern value output_value();
extern value output_value_to_string();
extern value output_value_to_buffer();
extern value format_float();
extern value float_of_string();
extern value int_of_float();
extern value float_of_int();
extern value neg_float();
extern value abs_float();
extern value add_float();
extern value sub_float();
extern value mul_float();
extern value div_float();
extern value exp_float();
extern value floor_float();
extern value fmod_float();
extern value frexp_float();
extern value ldexp_float();
extern value log_float();
extern value log10_float();
extern value modf_float();
extern value sqrt_float();
extern value power_float();
extern value sin_float();
extern value sinh_float();
extern value cos_float();
extern value cosh_float();
extern value tan_float();
extern value tanh_float();
extern value asin_float();
extern value acos_float();
extern value atan_float();
extern value atan2_float();
extern value ceil_float();
extern value eq_float();
extern value neq_float();
extern value le_float();
extern value lt_float();
extern value ge_float();
extern value gt_float();
extern value gc_stat();
extern value gc_get();
extern value gc_set();
extern value gc_minor();
extern value gc_major();
extern value gc_full_major();
extern value gc_compaction();
extern value hash_univ_param();
extern value input_value();
extern value input_value_from_string();
extern value marshal_data_size();
extern value int_of_string();
extern value format_int();
extern value caml_open_descriptor();
extern value channel_descriptor();
extern value caml_close_channel();
extern value caml_channel_size();
extern value caml_flush_partial();
extern value caml_flush();
extern value caml_output_char();
extern value caml_output_int();
extern value caml_output_partial();
extern value caml_output();
extern value caml_seek_out();
extern value caml_pos_out();
extern value caml_input_char();
extern value caml_input_int();
extern value caml_input();
extern value caml_seek_in();
extern value caml_pos_in();
extern value caml_input_scan_line();
extern value lex_engine();
extern value md5_string();
extern value md5_chan();
extern value get_global_data();
extern value reify_bytecode();
extern value realloc_global();
extern value available_primitives();
extern value get_current_environment();
extern value static_alloc();
extern value static_free();
extern value static_resize();
extern value obj_is_block();
extern value obj_tag();
extern value obj_block();
extern value parse_engine();
extern value install_signal_handler();
extern value ml_string_length();
extern value create_string();
extern value string_get();
extern value string_set();
extern value string_equal();
extern value string_notequal();
extern value blit_string();
extern value fill_string();
extern value is_printable();
extern value bitvect_test();
extern value sys_exit();
extern value sys_open();
extern value sys_close();
extern value sys_file_exists();
extern value sys_remove();
extern value sys_rename();
extern value sys_chdir();
extern value sys_getcwd();
extern value sys_getenv();
extern value sys_get_argv();
extern value sys_system_command();
extern value sys_get_config();
extern value terminfo_setup();
extern value terminfo_getstr();
extern value terminfo_getnum();
extern value terminfo_puts();
extern value register_named_value();
extern value weak_create();
extern value weak_set();
extern value weak_get();
extern value caml_get_exception_backtrace();
c_primitive cprim[] = {
	array_get,
	array_set,
	make_vect,
	compare,
	equal,
	notequal,
	lessthan,
	lessequal,
	greaterthan,
	greaterequal,
	output_value,
	output_value_to_string,
	output_value_to_buffer,
	format_float,
	float_of_string,
	int_of_float,
	float_of_int,
	neg_float,
	abs_float,
	add_float,
	sub_float,
	mul_float,
	div_float,
	exp_float,
	floor_float,
	fmod_float,
	frexp_float,
	ldexp_float,
	log_float,
	log10_float,
	modf_float,
	sqrt_float,
	power_float,
	sin_float,
	sinh_float,
	cos_float,
	cosh_float,
	tan_float,
	tanh_float,
	asin_float,
	acos_float,
	atan_float,
	atan2_float,
	ceil_float,
	eq_float,
	neq_float,
	le_float,
	lt_float,
	ge_float,
	gt_float,
	gc_stat,
	gc_get,
	gc_set,
	gc_minor,
	gc_major,
	gc_full_major,
	gc_compaction,
	hash_univ_param,
	input_value,
	input_value_from_string,
	marshal_data_size,
	int_of_string,
	format_int,
	caml_open_descriptor,
	channel_descriptor,
	caml_close_channel,
	caml_channel_size,
	caml_flush_partial,
	caml_flush,
	caml_output_char,
	caml_output_int,
	caml_output_partial,
	caml_output,
	caml_seek_out,
	caml_pos_out,
	caml_input_char,
	caml_input_int,
	caml_input,
	caml_seek_in,
	caml_pos_in,
	caml_input_scan_line,
	lex_engine,
	md5_string,
	md5_chan,
	get_global_data,
	reify_bytecode,
	realloc_global,
	available_primitives,
	get_current_environment,
	static_alloc,
	static_free,
	static_resize,
	obj_is_block,
	obj_tag,
	obj_block,
	parse_engine,
	install_signal_handler,
	ml_string_length,
	create_string,
	string_get,
	string_set,
	string_equal,
	string_notequal,
	blit_string,
	fill_string,
	is_printable,
	bitvect_test,
	sys_exit,
	sys_open,
	sys_close,
	sys_file_exists,
	sys_remove,
	sys_rename,
	sys_chdir,
	sys_getcwd,
	sys_getenv,
	sys_get_argv,
	sys_system_command,
	sys_get_config,
	terminfo_setup,
	terminfo_getstr,
	terminfo_getnum,
	terminfo_puts,
	register_named_value,
	weak_create,
	weak_set,
	weak_get,
	caml_get_exception_backtrace,
	 0 };
char * names_of_cprim[] = {
	"array_get",
	"array_set",
	"make_vect",
	"compare",
	"equal",
	"notequal",
	"lessthan",
	"lessequal",
	"greaterthan",
	"greaterequal",
	"output_value",
	"output_value_to_string",
	"output_value_to_buffer",
	"format_float",
	"float_of_string",
	"int_of_float",
	"float_of_int",
	"neg_float",
	"abs_float",
	"add_float",
	"sub_float",
	"mul_float",
	"div_float",
	"exp_float",
	"floor_float",
	"fmod_float",
	"frexp_float",
	"ldexp_float",
	"log_float",
	"log10_float",
	"modf_float",
	"sqrt_float",
	"power_float",
	"sin_float",
	"sinh_float",
	"cos_float",
	"cosh_float",
	"tan_float",
	"tanh_float",
	"asin_float",
	"acos_float",
	"atan_float",
	"atan2_float",
	"ceil_float",
	"eq_float",
	"neq_float",
	"le_float",
	"lt_float",
	"ge_float",
	"gt_float",
	"gc_stat",
	"gc_get",
	"gc_set",
	"gc_minor",
	"gc_major",
	"gc_full_major",
	"gc_compaction",
	"hash_univ_param",
	"input_value",
	"input_value_from_string",
	"marshal_data_size",
	"int_of_string",
	"format_int",
	"caml_open_descriptor",
	"channel_descriptor",
	"caml_close_channel",
	"caml_channel_size",
	"caml_flush_partial",
	"caml_flush",
	"caml_output_char",
	"caml_output_int",
	"caml_output_partial",
	"caml_output",
	"caml_seek_out",
	"caml_pos_out",
	"caml_input_char",
	"caml_input_int",
	"caml_input",
	"caml_seek_in",
	"caml_pos_in",
	"caml_input_scan_line",
	"lex_engine",
	"md5_string",
	"md5_chan",
	"get_global_data",
	"reify_bytecode",
	"realloc_global",
	"available_primitives",
	"get_current_environment",
	"static_alloc",
	"static_free",
	"static_resize",
	"obj_is_block",
	"obj_tag",
	"obj_block",
	"parse_engine",
	"install_signal_handler",
	"ml_string_length",
	"create_string",
	"string_get",
	"string_set",
	"string_equal",
	"string_notequal",
	"blit_string",
	"fill_string",
	"is_printable",
	"bitvect_test",
	"sys_exit",
	"sys_open",
	"sys_close",
	"sys_file_exists",
	"sys_remove",
	"sys_rename",
	"sys_chdir",
	"sys_getcwd",
	"sys_getenv",
	"sys_get_argv",
	"sys_system_command",
	"sys_get_config",
	"terminfo_setup",
	"terminfo_getstr",
	"terminfo_getnum",
	"terminfo_puts",
	"register_named_value",
	"weak_create",
	"weak_set",
	"weak_get",
	"caml_get_exception_backtrace",
	 0 };
