(* Author: Samuele Giraudo
 * Creation: jan. 2021
 * Modifications: jan. 2021, feb. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022,
 * aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun. 2023, jul. 2023
 *)

(* A type to contain an option on an input expression, an option of its output, an option on
 * its shadow, and its list of errors. *)
type processings = {
    (* The input expression. This is the expression obtained after each step of processing
     * before the evaluation. *)
    input: Expressions.expressions option;

    (* The output expression. This is the expression obtained by evaluating the input
     * expression. *)
    output: Expressions.expressions option;

    (* The shadow of the input and output expressions. *)
    shadow: Expressions.shadows option;

    (* The list of errors detected during the processing, just before the evaluation. *)
    errors: Errors.errors list
}

(* A type to represent a transformation, which is a map from processings to processings. *)
type transformations = Transformation of (processings -> processings)

(* Returns the empty processing. *)
let empty =
    {input = None; output = None; shadow = None; errors = []}

(* Returns the input expression of the processing pr. The input of pr must be different from
 * None. *)
let input pr =
    assert (Option.is_some pr.input);
    Option.get pr.input

(* Returns the output expression of the processing pr. The output of pr must be different
 * from None. *)
let output pr =
    assert (Option.is_some pr.output);
    Option.get pr.output

(* Returns the shadow of the processing pr. The shadow of pr must be different from None. *)
let shadow pr =
    assert (Option.is_some pr.shadow);
    Option.get pr.shadow

(* Returns the list of the errors of the processing pr. *)
let errors pr =
    pr.errors

(* Tests if there are errors in the processing pr. *)
let has_errors pr =
    pr.errors <> []

(* Returns the input expression with it shadow of the processing pr. The processing pr has
 * to have no errors, has to have an input, and has have a shadow. *)
let input_with_shadow pr =
    assert (not (has_errors pr));
    assert (Option.is_some pr.input);
    assert (Option.is_some pr.shadow);
    Expressions.Shadow (Information.empty, input pr, shadow pr)

(* Returns the output expression with it shadow of the processing pr. The processing pr has
 * to have no errors, has to have an input, and has have a shadow. *)
let output_with_shadow pr =
    assert (not (has_errors pr));
    assert (Option.is_some pr.output);
    assert (Option.is_some pr.shadow);
    Expressions.Shadow (Information.empty, output pr, shadow pr)

(* Returns the processing obtained by applying the processing transformation tr on the
 * processing pr. *)
let apply_transformation pr tr =
    let Transformation map = tr in
    map pr

(* Returns the processing transformation which detects the errors computed by the function f
 * sending expressions to error lists. *)
let error_transformation f =
    Transformation
        (fun pr ->
            if has_errors pr then
                pr
            else
                let errs =
                    pr.input
                    |> Options.bind (fun e -> Some (f e))
                    |> Options.value []
                in
                {pr with errors = errs})

(* Returns the processing transformation which applies the resolution computed by the
 * function f sending expressions to expressions. *)
let resolution_transformation f =
    Transformation
        (fun pr ->
            if has_errors pr then
                pr
            else
                let e' = pr.input |> Options.bind (fun e -> Some (f e)) in
                {pr with input = e'})

(* Returns the processing transformation which infers the shadow and detects the related
 * errors. *)
let typing_transformation =
    Transformation
        (fun pr ->
            if has_errors pr then
                pr
            else
                let (errs, sh) =
                    pr.input
                    |> Options.bind (fun e -> Some (Errors.shadow_errors_and_shadow e))
                    |> Options.value ([], None)
                in
                {pr with shadow = sh; errors = errs})

(* Returns the processing of the expression e obtained by resolving its inclusions and
 * its aliases, and by constructing the list of errors contained in e.
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
 * |                                       | Identifier errors                             |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Undefined alias errors                        |
 * |---------------------------------------|-----------------------------------------------|
 * | Resolution of aliases                 |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * | Resolution of shifts                  |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Invalid dominant leaves                       |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Invalid selfs                                 |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Unknown variables in rules                    |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Invalid levels                                |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Nonlinear errors                              |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Ambiguous constant errors                     |
 * |---------------------------------------|-----------------------------------------------|
 * |                        Shadow computations and errors                                 |
 * |---------------------------------------|-----------------------------------------------|
 * | Suppression of shadows                |                                               |
 * |---------------------------------------|-----------------------------------------------|
 * |                                       | Conflict errors                               |
 * |=======================================|===============================================|
 *)
let process e =
    let trs = [
        error_transformation Errors.inclusion_errors;
        resolution_transformation Resolutions.resolve_inclusions;
        error_transformation Errors.invalid_identifier_errors;
        error_transformation Errors.undefined_alias_errors;
        resolution_transformation Resolutions.resolve_alias_definitions;
        resolution_transformation Resolutions.resolve_shifts;
        error_transformation Errors.invalid_dominant_leaf_errors;
        error_transformation Errors.invalid_self_errors;
        error_transformation Errors.unknown_variable_errors;
        error_transformation Errors.invalid_level_errors;
        error_transformation Errors.nonlinear_errors;
        error_transformation Errors.ambiguous_constant_errors;
        typing_transformation;
        resolution_transformation Shadows.remove_shadows;
        error_transformation Errors.conflict_errors
    ] in
    let pr1 = {empty with input = Some e} in
    let pr2 = trs |> List.fold_left apply_transformation pr1 in
    let output =
        if not (has_errors pr2) then Some (Evaluations.compute (input pr2)) else None
    in
    {pr2 with output = output}

(* Returns the processing of the expression contained in the file at path path. *)
let process_path path =
    try
        process (Files.path_to_expression path)
    with
        |Lexer.Error err ->
            let errs = [Errors.syntax_error_from_lexer err] in
            {empty with errors = errs}

