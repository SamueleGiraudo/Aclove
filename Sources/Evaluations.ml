(* Author: Samuele Giraudo
 * Creation: dec. 2023
 * Modifications: dec. 2023, jan. 2024
 *)

module D = DataErrors
module E = Expressions
module Er = Errors
module F = Files
module L = Lexer
module Op = Operations
module P = Properties
module Pa = Paths
module R = Rewrites
module S = Shadows

(* Returns the data errors on the expression at path path. *)
let from_path path =
    assert (Sys.file_exists path);
    try
        D.Data (path |> Pa.canonicalize |> F.path_to_expression)
    with
        |L.Error err -> D.Errors [Er.syntax_error_from_lexer err]

(* Returns the data errors on the expression obtained by pre-processing the possible
 * expression of the data errors ee.
 *
 * This passes by the following steps:
 *
 * |=======================================|===============================================|
 * |               PROCESSING              |             ERROR DETECTION                   |
 * |=======================================|===============================================|
 * |                                       | Syntax errors in all included files and paths |
 * |---------------------------------------|-----------------------------------------------|
 * | Resolution of inclusions              |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Name errors                                   |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Unknown alias errors                          |
 * |---------------------------------------|-----------------------------------------------|
 * | Resolution of aliases                 |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Invalid variable positions                    |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Invalid self positions                        |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Invalid map positions                         |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Unknown variables in rules                    |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Nonlinear errors                              |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Ambiguous constant errors                     |
 * |---------------------------------------|-----------------------------------------------|
 * | Settings indices                      |                                               |
 * |=======================================|===============================================|
 *)
let pre_process ee =
    ee
    |> D.check_errors Er.inclusion_errors
    |> D.map_data Op.resolve_inclusions
    |> D.check_errors Er.invalid_name_errors
    |> D.check_errors Er.unknown_alias_errors
    |> D.map_data Op.resolve_alias_definitions
    |> D.check_errors Er.invalid_variable_position_errors
    |> D.check_errors Er.invalid_self_position_errors
    |> D.check_errors Er.invalid_map_position_errors
    |> D.check_errors Er.unknown_variable_errors
    |> D.check_errors Er.nonlinear_errors
    |> D.check_errors Er.ambiguous_constant_errors
    |> D.map_data S.set_indices

(* Returns the data errors on the expression obtained by processing the possible expression
 * of the data errors ee. The possible expression of ee has to be inclusion free, alias
 * free, indexed, have no invalid name errors, no invalid self position errors, no unknown
 * variable errors, and no nonlinear errors.
 *
 * This passes by the following steps:
 *
 * |=======================================|===============================================|
 * |               PROCESSING              |             ERROR DETECTION                   |
 * |=======================================|===============================================|
 * | Shadow computation (recursively)      |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Normal form of shadow                 |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Shadow errors                                 |
 * |---------------------------------------|-----------------------------------------------|
 * | Suppression of shadows                |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Normal forms of left rule members     |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Conflict errors                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Normal form                           |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Adding shadow                         |                                               |
 * |=======================================|===============================================|
 *)
let rec process ee =
    assert (ee |> D.test_data P.is_inclusion_free);
    assert (ee |> D.test_data P.is_alias_free);
    assert (ee |> D.test_data P.is_indexed);
    assert (ee |> D.test_data (fun e -> Er.invalid_name_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.invalid_self_position_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.unknown_variable_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.nonlinear_errors e = []));
    let she =
        ee
        |> S.infer_shadow process
        |> D.map_data R.normal_form
    in
    if D.has_errors she then
        D.Errors (D.errors she)
    else
        ee
        |> D.map_data Op.remove_shadows
        |> D.map_data R.resolve_left_members_rules
        |> D.check_errors Er.conflict_errors
        |> D.map_data R.normal_form
        |> D.map_data (Fun.flip Op.add_shadow (D.data she |> Option.get))

(* Returns the data errors on the expression obtained by post-processing the possible
 * expression of the data errors ee. This remove its rules if with_rules is false, remove
 * its shadows if with_shadows is false, and abbreviates its constant names if
 * with_full_names is false. The possible expression of ee has to be inclusion free, alias
 * free, have no invalid name errors, no variable position errors, no invalid self position
 * errors, no map position errors, no unknown variable errors, no nonlinear errors, and no
 * ambiguous constant errors.
 *
 * This passes by the following steps:
 *
 * |=======================================|===============================================|
 * |               PROCESSING              |             ERROR DETECTION                   |
 * |=======================================|===============================================|
 * | Suppression of rules (if requested)   |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Suppression of shadow (if requested)  |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Clean constant names (if requested)   |                                               |
 * |=======================================|===============================================|
 *)
let post_process with_rules with_shadows with_full_names ee =
    assert (ee |> D.test_data P.is_inclusion_free);
    assert (ee |> D.test_data P.is_alias_free);
    assert (ee |> D.test_data (fun e -> Er.invalid_name_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.invalid_variable_position_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.invalid_self_position_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.invalid_map_position_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.unknown_variable_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.nonlinear_errors e = []));
    assert (ee |> D.test_data (fun e -> Er.ambiguous_constant_errors e = []));
    ee
    |> (if with_rules then Fun.id else D.map_data Op.remove_rules)
    |> (if with_shadows then Fun.id else D.map_data Op.remove_shadows)
    |> (if with_full_names then Fun.id else D.map_data Op.clean_constant_names)

