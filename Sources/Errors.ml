(* Author: Samuele Giraudo
 * Creation: may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022,
 * apr. 2023, jun. 2023, jul. 2023
 *)

(* The error kinds an Aclove program can contain. *)
type kinds =
    |InvalidVariable of Variables.variables
    |InvalidConstant of Constants.constants
    |InvalidAlias of Expressions.alias
    |UndefinedAlias of Expressions.alias
    |InvalidDominantLeaf of Constants.constants
    |InvalidSelf
    |UnknownVariable of Variables.variables
    |InvalidLevel of Levels.levels * Levels.levels
    |NonlinearLeftMemberRule of Constants.constants
    |AmbiguousConstant of Constants.constants
    |RuleConflict of Constants.constants
    |InvalidInclusionPath of Expressions.path
    |CircularInclusion of Expressions.path
    |SyntaxError of Lexer.error_kinds
    |InvalidShadow

(* A type to represent information about an error. *)
type errors = {
    (* The kind of the error. *)
    kind: kinds;

    (* Information about the subexpression where the error appears. *)
    information: Information.information
}

(* Returns the error obtained from the lexer error err. The kind of the returned error is
 * SyntaxError. *)
let syntax_error_from_lexer err =
    let kind = SyntaxError (Lexer.error_to_error_kind err) in
    let info = Information.construct (Lexer.error_to_position err) in
    {kind = kind; information = info}

(* Returns a string representation of the error err. *)
let to_string err =
    Information.to_string err.information ^ ": " ^
    match err.kind with
        |InvalidVariable v -> Printf.sprintf "invalid variable %s" (Variables.to_string v)
        |InvalidConstant c -> Printf.sprintf "invalid constant %s" (Constants.to_string c)
        |InvalidAlias alias -> Printf.sprintf "invalid alias %s" alias
        |UndefinedAlias alias -> Printf.sprintf "undefined alias %s" alias
        |InvalidDominantLeaf c ->
            Printf.sprintf
                "invalid dominant leaf for rule of constant %s"
                (Constants.to_string c)
        |InvalidSelf -> "invalid self"
        |UnknownVariable v ->
            Printf.sprintf
                "unknown variable %s in right member of rule"
                (Variables.to_string v)
        |InvalidLevel (lvl, lvl') ->
            Printf.sprintf
                "level %d is expected instead of %d" (Levels.value lvl) (Levels.value lvl')
        |NonlinearLeftMemberRule c ->
            Printf.sprintf
                "nonlinear left member of rule of constant %s"
                (Constants.to_string c)
        |AmbiguousConstant c ->
            Printf.sprintf "constant %s already exists" (Constants.to_string c)
        |RuleConflict c ->
            Printf.sprintf "rule conflict for the constant %s" (Constants.to_string c)
        |InvalidInclusionPath path -> Printf.sprintf "path %s is invalid for inclusion" path
        |CircularInclusion path -> Printf.sprintf "circular inclusion involving %s" path
        |SyntaxError err -> Lexer.error_kind_to_string err
        |InvalidShadow -> "expression without shadow"

(* Returns the list of the inclusion errors in the expression e and recursively in the
 * expressions of the included Aclove files. *)
let inclusion_errors e =
    let rec aux paths e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ |Expressions.Alias _ -> []
            |Expressions.Constant (_, _, rules) ->
                rules |> Lists.map_pairs_operation (aux paths) (@) |> List.flatten
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux paths e1 @ aux paths e2
            |Expressions.Shift (_, _, e1) -> aux paths e1
            |Expressions.AliasDefinition (_, _, e1, e2) -> aux paths e1 @ aux paths e2
            |Expressions.Put (info, path) ->
                let path = Files.add_file_extension path |> Paths.simplify in
                if not (Sys.file_exists path) || not (Files.is_inclusion_path path) then
                    [{kind = InvalidInclusionPath path; information = info}]
                else
                    if List.mem path paths then
                        []
                    else
                        try
                            let e0 = Files.path_to_expression path in
                            let paths' = path :: paths in
                            if List.mem path (Files.included_paths e0) then
                                [{kind = CircularInclusion path; information = info}]
                            else
                                aux paths' e0
                        with
                            |Lexer.Error err -> [syntax_error_from_lexer err]
    in
    aux [] e |> List.sort_uniq compare

(* Returns the list of the invalid identifiers in the expression e. This expression has to
 * be inclusion free. *)
let invalid_identifier_errors e =
    assert (Properties.is_inclusion_free e);
    let rec aux e =
        match e with
            |Expressions.Variable (info, v) ->
                if Files.is_variable_name (Variables.name v) then
                    []
                else
                    [{kind = InvalidVariable v; information = info}]
            |Expressions.Constant (info, c, rules) ->
                let tmp = rules |> Lists.map_pairs_operation aux (@) |> List.flatten in
                if Files.is_constant_name (Constants.name c) then
                    tmp
                else
                    {kind = InvalidConstant c; information = info} :: tmp
            |Expressions.Self _ -> []
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |Expressions.Shift (_, _, e1) -> aux e1
            |Expressions.Alias (info, alias) ->
                if Files.is_alias alias then
                    []
                else
                    [{kind = InvalidAlias alias; information = info}]
            |Expressions.AliasDefinition (info, alias, e1, e2) ->
                let tmp = aux e1 @ aux e2 in
                if Files.is_alias alias then
                    tmp
                else
                    {kind = InvalidAlias alias; information = info} :: tmp
            |_ -> Expressions.ValueError (e, "Errors.invalid_identifier_errors") |> raise
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the undefined aliases in the expression e. This
 * expression has to be inclusion free. *)
let undefined_alias_errors e =
    assert (Properties.is_inclusion_free e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (_, _, rules) ->
                rules |> Lists.map_pairs_operation aux (@) |> List.flatten
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |Expressions.Shift (_, _, e1) -> aux e1
            |Expressions.Alias (info, alias) -> [(alias, info)]
            |Expressions.AliasDefinition (_, alias, e1, e2) ->
                aux e1 @ (aux e2 |> List.filter (fun (alias', _) -> alias' <> alias))
            |_ -> Expressions.ValueError (e, "Errors.undefined_alias_errors") |> raise
    in
    aux e
    |> List.map (fun (alias, info) -> {kind = UndefinedAlias alias; information = info})
    |> List.sort_uniq compare

(* Returns the list of the errors about the invalid dominant leaves in the expression e.
 * In all rules, the dominant leaves of the left members must be the expression self. The
 * expression e has to be simple. *)
let invalid_dominant_leaf_errors e =
    assert (Properties.is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (_, c, rules) ->
                let tmp_1 = rules |> Lists.map_pairs_operation aux (@) |> List.flatten in
                let tmp_2 =
                    rules
                    |> List.filter (fun (e1, _) ->
                        match Properties.dominant_leaf e1 with
                            |Expressions.Self _ -> false
                            |_ -> true)
                    |> List.map (fun (e1, _) ->
                        let info = Properties.root_information e1 in
                        {kind = InvalidDominantLeaf c; information = info})
                in
                tmp_1 @ tmp_2
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |_ -> Expressions.ValueError (e, "Errors.invalid_dominant_leaf_errors") |> raise
    in
    aux e

(* Returns the list of the errors about the invalid selfs in the expression e. Such an error
 * appears when a self expression is located outside a rule. The expression e has to be
 * simple. *)
let invalid_self_errors e =
    assert (Properties.is_simple e);
    let rec aux self_allowed e =
        match e with
            |Expressions.Variable _ -> []
            |Expressions.Constant (_, _, rules) ->
                rules |> Lists.map_pairs_operation (aux true) (@) |> List.flatten
            |Expressions.Self (info, _) ->
                if self_allowed then [] else [{kind = InvalidSelf; information = info}]
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2) ->
                aux self_allowed e1 @ aux self_allowed e2
            |Expressions.Shadow (_, e1, sh) ->
                aux self_allowed e1 @ aux false sh
            |_ -> Expressions.ValueError (e, "Errors.invalid_self_errors") |> raise
    in
    aux false e

(* Returns the list of the errors about the unknown variables of the right members or the
 * rules in the expression e. This expression has to be simple. *)
let unknown_variable_errors e =
    assert (Properties.is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (_, _, rules) ->
                let tmp_1 = rules |> Lists.map_pairs_operation aux (@) |> List.flatten in
                let tmp_2 =
                    rules
                    |> List.map (fun (e1, e2) ->
                        let left_vars = Properties.external_variables e1 in
                        Properties.external_variables e2
                            |> List.filter (fun v -> not (List.mem v left_vars))
                            |> List.map (fun v ->
                                let info = Properties.root_information e2 in
                                {kind = UnknownVariable v; information = info}))
                    |> List.flatten
                in
                tmp_1 @ tmp_2
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |_ -> Expressions.ValueError (e, "Errors.unknown_variable_errors") |> raise
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the invalid levels in the expression e. This occurs
 * when a subexpression has a different level from the one expected by its enclosing
 * expression, or when the left and right members of a rule have different levels, or when
 * the members of a rule have a different level from the constant they belong, or when the
 * two members of an application or of a map have different levels, or when the shadow
 * of a constant has not as level 1 plus the level of the constant. The expression e has to
 * be simple. *)
let invalid_level_errors e =
    assert (Properties.is_simple e);
    let error info lvl lvl' =
        if lvl <> lvl' then
            [{kind = InvalidLevel (lvl, lvl'); information = info}]
        else
            []
    in
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (_, c, rules) ->
                let lvl = Constants.level c in
                let tmp_1 = rules |> Lists.map_pairs_operation aux (@) |> List.flatten in
                let tmp_2 =
                    rules
                    |> List.map fst
                    |> List.map (fun e' ->
                        error (Properties.root_information e') (Properties.level e') lvl)
                    |> List.flatten
                in
                let tmp_3 =
                    rules
                    |> List.map snd
                    |> List.map (fun e' ->
                        error (Properties.root_information e') (Properties.level e') lvl)
                    |> List.flatten
                in
                tmp_1 @ tmp_2 @ tmp_3
            |Expressions.Application (info, e1, e2) |Expressions.Map (info, e1, e2) ->
                let tmp_1 = error info (Properties.level e1) (Properties.level e2) in
                let tmp_2 = aux e1 @ aux e2 in
                tmp_1 @ tmp_2
            |Expressions.Shadow (info, e1, sh) ->
                let lvl = Properties.level e1 |> Levels.shift 1 in
                let tmp_1 = error info lvl (Properties.level sh) in
                let tmp_2 = aux e1 @ aux sh in
                tmp_1 @ tmp_2
            |_ -> Expressions.ValueError (e, "Errors.invalid_level_errors") |> raise
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the left members of the rules of the expression e
 * which are nonlinear. This expression has to be simple. *)
let nonlinear_errors e =
    assert (Properties.is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (_, c, rules) ->
                let tmp_1 = rules |> Lists.map_pairs_operation aux (@) |> List.flatten in
                let tmp_2 =
                    rules
                    |> List.filter (fun (e', _) ->
                        let seq = Properties.external_variable_sequence e' in
                        List.length seq <> List.length (List.sort_uniq compare seq))
                    |> List.map (fun (e', _) ->
                        let info = Properties.root_information e' in
                        {kind = NonlinearLeftMemberRule c; information = info})
                in
                tmp_1 @ tmp_2
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |_ -> Expressions.ValueError (e, "Errors.nonlinear_errors") |> raise
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the ambiguous constants in expression e. Two
 * constants are ambiguous if they have the same name but different rules or types. The
 * expression e has to be simple and external self free. *)
let ambiguous_constant_errors e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (info, c, rules) ->
                (info, c, rules)
                :: (rules |> Lists.map_pairs_operation aux (@) |> List.flatten)
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |_ -> Expressions.ValueError (e, "Errors.conflict_errors") |> raise
    in
    Lists.triangle_product (aux e |> List.sort_uniq compare)
    |> List.filter (fun ((_, c, rules), (_, c', rules')) -> c = c' && rules <> rules')
    |> List.map (fun ((info, c, _), _) ->
        {kind = AmbiguousConstant c; information = info})
    |> List.sort_uniq compare

(* Returns the list of the errors about conflicts in the left members of the rules of the
 * expression e. Such a conflict appears when there is a constant of e which has two rules
 * having unifiable left members. The expression e has to be simple and external self
 * free. *)
let conflict_errors e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> []
            |Expressions.Constant (info, c, rules) ->
                let left_members =
                    rules |> List.map (fun (e', _) -> Evaluations.substitute_self e' e)
                in
                let tmp_1 =
                    if left_members |> Lists.triangle_product |> List.exists
                        (fun (e1, e2) -> Substitutions.unify e1 e2 |> Option.is_some)
                    then
                        [{kind = RuleConflict c; information = info}]
                    else
                        []
                in
                let tmp_2 = rules |> Lists.map_pairs_operation aux (@) |> List.flatten in
                tmp_1 @ tmp_2
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |_ -> Expressions.ValueError (e, "Errors.conflict_errors") |> raise
    in
    aux e |> List.sort_uniq compare

(* Returns the pair (errs, sh) where errs is list of the errors concerning the shadow of the
 * expression e, and sh is an option on the shadow of e if any and None otherwise. The
 * expression e has to be simple and external self free. *)
let shadow_errors_and_shadow e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    let sh = Shadows.compute e in
    if Option.is_none sh then
        ([{kind = InvalidShadow; information = Properties.root_information e}], None)
    else
        ([], sh)

