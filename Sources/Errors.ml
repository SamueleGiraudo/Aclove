(* Author: Samuele Giraudo
 * Creation: may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022,
 * apr. 2023, jun. 2023, jul. 2023, dec. 2023, jan. 2024
 *)

module C = Constants
module E = Expressions
module F = Files
module I = Information
module Ls = Lists
module Le = Lexer
module P = Properties
module Pa = Paths
module S = Substitutions
module St = Strings
module V = Variables
module Op = Operations
module Ou = Outputs

(* The error kinds an Aclove program can contain. *)
type kinds =
    |InvalidVariableName of V.variables
    |InvalidConstantName of C.constants
    |InvalidAliasName of E.alias
    |UnknownAlias of E.alias
    |InvalidVariablePosition of E.expressions
    |InvalidSelfPosition of E.expressions
    |InvalidMapPosition of E.expressions
    |UnknownVariable of V.variables
    |NonlinearLeftMemberRule of C.constants
    |AmbiguousConstant of C.constants
    |RuleConflict of C.constants
    |InvalidInclusionPath of E.path
    |CircularInclusion of E.path
    |NonUnifiableShadows of E.expressions * E.expressions
    |SyntaxError of Le.error_kinds

(* A type to represent information about an error. *)
type errors = {
    (* The kind of the error. *)
    kind: kinds;

    (* Information about the subexpression where the error appears. *)
    information: I.information
}

(* Returns the error obtained from the lexer error err. The kind of the returned error is
 * SyntaxError. *)
let syntax_error_from_lexer err =
    let kind = SyntaxError (Le.error_to_error_kind err) in
    let info = I.construct (Le.error_to_position err) in
    {kind = kind; information = info}

(* Returns a string representation of the error err. *)
let to_string err =
    I.to_string err.information ^ ":\n" ^
    match err.kind with
        |InvalidVariableName v ->
            "invalid variable name\n" ^ (V.to_string v |> St.indent 4)
        |InvalidConstantName c ->
            "invalid constant name\n" ^ (C.to_string c |> St.indent 4)
        |InvalidAliasName alias ->
            "invalid alias name\n" ^ (alias |> St.indent 4)
        |UnknownAlias alias ->
            "unknown alias\n" ^ (alias |> St.indent 4)
        |InvalidVariablePosition e ->
            "invalid variable position in\n"
            ^ (e |> Ou.to_buffered_string |> Buffer.contents |> St.indent 4)
        |InvalidSelfPosition e ->
            "invalid self position in\n"
            ^ (e |> Ou.to_buffered_string |> Buffer.contents |> St.indent 4)
        |InvalidMapPosition e ->
            "invalid map position in\n"
            ^ (e |> Ou.to_buffered_string |> Buffer.contents |> St.indent 4)
        |UnknownVariable v ->
            "unknown variable\n" ^ (V.to_string v |> St.indent 4)
        |NonlinearLeftMemberRule c ->
            "nonlinear left rule member of constant\n" ^ (C.to_string c |> St.indent 4)
        |AmbiguousConstant c ->
            "already existing constant\n" ^ (C.to_string c |> St.indent 4)
        |RuleConflict c ->
            "rule conflict for constant\n" ^ (C.to_string c |> St.indent 4)
        |InvalidInclusionPath path ->
            "invalid inclusion path\n" ^ (path |> St.indent 4)
        |CircularInclusion path ->
            "circular inclusion involving\n" ^ (path |> St.indent 4)
        |NonUnifiableShadows (sh1, sh2) ->
            "non unifiable shadows\n"
            ^ (sh1 |> Ou.to_buffered_string |> Buffer.contents |> St.indent 4)
            ^ "\nand\n"
            ^ (sh2 |> Ou.to_buffered_string |> Buffer.contents |> St.indent 4)
        |SyntaxError err -> Le.error_kind_to_string err

(* Returns the error reporting the non unifiable shadows sh1 and sh2 of the expression e. *)
let make_non_unifiable_shadows_error e sh1 sh2 =
    {kind = NonUnifiableShadows (sh1, sh2); information = P.root_information e}

(* Returns the list of the inclusion errors in the expression e and recursively in the
 * expressions of the included Aclove files. *)
let inclusion_errors e =
    let rec aux paths e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ -> []
            |E.Constant (_, _, rules) ->
                rules
                |> List.map
                    (fun (e_lst, e1) -> e1 :: e_lst |> List.map (aux paths) |> List.flatten)
                |> List.flatten
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2) ->
                aux paths e1 @ aux paths e2
            |E.AliasDefinition (_, _, e1, e2) -> aux paths e1 @ aux paths e2
            |E.Put (info, path) ->
                let path = Pa.add_extension F.extension path in
                if not (Sys.file_exists path) || not (F.is_inclusion_path path) then
                    [{kind = InvalidInclusionPath path; information = info}]
                else
                    let path' = Pa.canonicalize path in
                    if List.mem path' paths then
                        []
                    else
                        try
                            let e0 = F.path_to_expression path' in
                            let paths' = path' :: paths in
                            if List.mem path' (F.included_paths e0) then
                                [{kind = CircularInclusion path'; information = info}]
                            else
                                aux paths' e0
                        with
                            |Le.Error err -> [syntax_error_from_lexer err]
    in
    aux [] e |> List.sort_uniq compare

(* Returns the list of the invalid variable names, invalid constant names, and invalid alias
 * names in the expression e. *)
let invalid_name_errors e =
    let rec aux e =
        match e with
            |E.Variable (info, v) ->
                if F.is_variable_name (V.name v) then
                    []
                else
                    [{kind = InvalidVariableName v; information = info}]
            |E.Constant (info, c, rules) ->
                let tmp =
                    rules
                    |> List.map
                        (fun (e_lst, e1) -> e1 :: e_lst |> List.map aux |> List.flatten)
                    |> List.flatten
                in
                if F.is_constant_name (C.name c) then
                    tmp
                else
                    {kind = InvalidConstantName c; information = info} :: tmp
            |E.Self _ |E.Put _ -> []
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |E.Alias (info, alias) ->
                if F.is_alias alias then
                    []
                else
                    [{kind = InvalidAliasName alias; information = info}]
            |E.AliasDefinition (info, alias, e1, e2) ->
                let tmp = aux e1 @ aux e2 in
                if F.is_alias alias then
                    tmp
                else
                    {kind = InvalidAliasName alias; information = info} :: tmp
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the unknown aliases in the expression e. *)
let unknown_alias_errors e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Put _ -> []
            |E.Constant (_, _, rules) ->
                rules
                |> List.map
                    (fun (e_lst, e1) -> e1 :: e_lst |> List.map aux |> List.flatten)
                |> List.flatten
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |E.Alias (info, alias) -> [(alias, info)]
            |E.AliasDefinition (_, alias, e1, e2) ->
                aux e1 @ (aux e2 |> List.filter (fun (alias', _) -> alias' <> alias))
    in
    aux e
    |> List.map (fun (alias, info) -> {kind = UnknownAlias alias; information = info})
    |> List.sort_uniq compare

(* Returns the list of the errors about the invalid variables in the expression e. Such an
 * error appears when a variable is located outside a rule or a shadow. *)
let invalid_variable_position_errors e =
    let rec aux forbidden e =
        match e with
            |E.Variable (info, _) -> begin
                match forbidden with
                    |None -> []
                    |Some e -> [{kind = InvalidVariablePosition e; information = info}]
            end
            |E.Constant (_, _, rules) ->
                rules
                |> List.map (fun (e_lst, e1) -> e1 :: e_lst)
                |> List.flatten
                |> List.map (aux None)
                |> List.flatten
            |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux forbidden e1 @ aux forbidden e2
            |E.Shadow (_, e1, sh) -> aux forbidden e1 @ aux None sh
    in
    aux (Some e) e

(* Returns the list of the errors about the invalid selfs in the expression e. Such an error
 * appears when a self expression is located outside a rule or externally in an expression
 * which is in a left member of a rule. *)
let invalid_self_position_errors e =
    let rec aux forbidden e =
        match e with
            |E.Variable _ |E.Alias _ |E.Put _ -> []
            |E.Constant (_, _, rules) ->
                let tmp_1 =
                    rules
                    |> List.map fst
                    |> List.flatten
                    |> List.map (fun e -> aux (Some e) e)
                    |> List.flatten
                in
                let tmp_2 = rules |> List.map snd |> List.map (aux None) |> List.flatten in
                tmp_1 @ tmp_2
            |E.Self info -> begin
                match forbidden with
                    |None -> []
                    |Some e -> [{kind = InvalidSelfPosition e; information = info}]
            end
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux forbidden e1 @ aux forbidden e2
            |E.Shadow (_, e1, sh) -> aux forbidden e1 @ aux (Some sh) sh
    in
    aux (Some e) e

(* Returns the list of the errors about the invalid maps in the expression e. Such an error
 * appears when a map is located outside a shadow. *)
let invalid_map_position_errors e =
    let rec aux forbidden e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Constant (_, _, rules) ->
                rules
                |> List.map (fun (e_lst, e1) -> e1 :: e_lst)
                |> List.flatten
                |> List.map (fun e -> aux (Some e) e)
                |> List.flatten
            |E.Application (_, e1, e2) |E.AliasDefinition (_, _, e1, e2) ->
                aux (Some e1) e1 @ aux (Some e2) e2
            |E.Map (info, e1, e2) ->
                let tmp_1 =
                    match forbidden with
                        |None -> []
                        |Some e -> [{kind = InvalidMapPosition e; information = info}]
                in
                tmp_1 @ aux forbidden e1 @ aux forbidden e2
            |E.Shadow (_, e1, sh) -> aux (Some e1) e1 @ aux None sh
    in
    aux (Some e) e

(* Returns the list of the errors about the unknown variables of the right members of the
 * rules in the expression e. *)
let unknown_variable_errors e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Constant (_, _, rules) ->
                let tmp_1 =
                    rules
                    |> List.map
                        (fun (e_lst, e1) -> e1 :: e_lst |> List.map aux |> List.flatten)
                    |> List.flatten
                in
                let tmp_2 =
                    rules
                    |> List.map
                        (fun (e_lst, e1) ->
                            let left_vars =
                                e_lst
                                |> List.map P.external_variables
                                |> List.flatten
                            in
                            P.external_variables e1
                            |> List.filter (fun v -> not (List.mem v left_vars))
                            |> List.map
                                (fun v ->
                                    let info = P.root_information e1 in
                                    {kind = UnknownVariable v; information = info}))
                    |> List.flatten
                in
                tmp_1 @ tmp_2
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the left members of the rules of the expression e
 * which are nonlinear. *)
let nonlinear_errors e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Constant (_, c, rules) ->
                let tmp_1 =
                    rules
                    |> List.map
                        (fun (e_lst, e1) -> e1 :: e_lst |> List.map aux |> List.flatten)
                    |> List.flatten
                in
                let tmp_2 =
                    rules
                    |> List.filter
                        (fun (e_lst, _) ->
                            let seq =
                                e_lst
                                |> List.map P.external_variable_sequence
                                |> List.flatten
                            in
                            List.length seq <> List.length (List.sort_uniq compare seq))
                    |> List.map
                        (fun (_, e1) ->
                            let info = P.root_information e1 in
                            {kind = NonlinearLeftMemberRule c; information = info})
                in
                tmp_1 @ tmp_2
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
    in
    aux e |> List.sort_uniq compare

(* Returns the list of the errors about the ambiguous constants in expression e. Two
 * constants are ambiguous if they have the same name but different rules. The expression e
 * has to be inclusion free and alias free. *)
let ambiguous_constant_errors e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Constant (info, c, rules) ->
                let tmp =
                    rules
                    |> List.map
                        (fun (e_lst, e1) -> e1 :: e_lst |> List.map aux |> List.flatten)
                    |> List.flatten
                in
                (info, c, rules) :: tmp
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
    in
    e
    |> Op.remove_indices
    |> aux
    |> List.sort_uniq compare
    |> Ls.triangle_product
    |> List.filter (fun ((_, c, rules), (_, c', rules')) -> c = c' && rules <> rules')
    |> List.map (fun ((info, c, _), _) -> {kind = AmbiguousConstant c; information = info})
    |> List.sort_uniq compare

(* Returns the list of the errors about conflicts in the left members of the rules of the
 * external constants of the expression e. Such a conflict appears when there is a constant
 * of e which has two rules having unifiable left members. *)
let conflict_errors e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Constant (info, c, rules) ->
                let left_members =
                    rules
                    |> List.map (fun (e_lst, _) -> Op.left_member_rule_to_expression e e_lst)
                in
                let lst = left_members |> Ls.triangle_product in
                if lst |> List.exists (fun (e1, e2) -> S.unify e1 e2 |> Option.is_some) then
                    [{kind = RuleConflict c; information = info}]
                else
                    []
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2) ->
                aux e1 @ aux e2
            |E.AliasDefinition (_, _, e1, _) -> aux e1
    in
    aux e |> List.sort_uniq compare

