(* Author: Samuele Giraudo
 * Creation: (jan. 2021), jul. 2023
 * Modifications: jul. 2023, dec. 2023, jan. 2024
 *)

module C = Constants
module E = Expressions
module F = Files
module I = Information
module P = Properties
module Pa = Paths

(* Returns the expression obtained by putting the expression e as dominant expression
 * of the argument expressions of the list e_lst. *)
let left_member_rule_to_expression e e_lst =
    let ri = P.root_information e in
    e_lst |> List.fold_left (fun res e' -> E.Application (ri, res, e')) e

(* Returns the expression obtained by cleaning each constant name of the constants of e.
* This consists in replacing each constant name of e by its suffix. *)
let clean_constant_names e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Put _ |E.Alias _ -> e
            |E.Constant (info, c, rules) ->
                let c' = C.keep_suffix c in
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c', rules')
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns the expression obtained by replacing all free occurrences of the alias alias in
 * the expression e1 by the expression e2. *)
let substitute_free_aliases e1 alias e2 =
    let rec aux e1 =
        match e1 with
            |E.Variable _ |E.Self _ |E.Put _ -> e1
            |E.Constant (info, c, rules) ->
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c, rules')
            |E.Application (info, e1', e2') -> E.Application (info, aux e1', aux e2')
            |E.Map (info, e1', e2') -> E.Map (info, aux e1', aux e2')
            |E.Shadow (info, e1', sh) -> E.Shadow (info, aux e1', aux sh)
            |E.Alias (_, alias') -> if alias' = alias then e2 else e1
            |E.AliasDefinition (info, alias', e1', e2') ->
                let e2'' = if alias' = alias then e2' else aux e2' in
                E.AliasDefinition (info, alias', aux e1', e2'')
    in
    aux e1

(* Returns the expression obtained from the expression e by resolving its inclusions. The
 * exception Lexer.Error is raised when there are syntax errors in the included files in the
 * program. *)
let resolve_inclusions e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ -> e
            |E.Constant (info, c, rules) ->
                let rules' =
                    rules
                    |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c, rules')
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
            |E.Put (_, path) ->
                let path' = path |> Pa.add_extension F.extension |> Pa.canonicalize in
                aux (F.path_to_expression path')
    in
    aux e

(* Returns the expression obtained by replacing each alias admitting a definition in the
 * expression e by its definition. *)
let resolve_alias_definitions e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Alias _ |E.Self _ |E.Put _ -> e
            |E.Constant (info, c, rules) ->
                let rules' =
                    rules
                    |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c, rules')
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (_, alias, e1, e2) ->
                substitute_free_aliases (aux e2) alias (aux e1)
    in
    aux e

(* Returns the expression obtained from the expression e by removing its rules. *)
let remove_rules e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Alias _ |E.Self _ |E.Put _ -> e
            |E.Constant (info, c, _) -> E.Constant (info, c, [])
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns the expression obtained from the expression e by removing its shadows. *)
let remove_shadows e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Alias _ |E.Self _ |E.Put _ -> e
            |E.Constant (info, c, rules) ->
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c, rules')
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (_, e1, _) -> aux e1
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns the expression consisting in a shadow node, associating with the expression e the
 * shadow sh. *)
let add_shadow e sh =
    E.Shadow (P.root_information e, e, sh)

(* Returns the expressions obtained by removing the indices of the expression e. *)
let remove_indices e =
    let rec aux e =
        let info = P.root_information e |> I.remove_index in
        match e with
            |E.Variable (_, v) -> E.Variable (info, v)
            |E.Constant (_, c, rules) ->
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c, rules')
            |E.Self _ -> E.Self info
            |E.Application (_, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (_, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (_, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.Alias (_, alias) -> E.Alias (info, alias)
            |E.AliasDefinition (_, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
            |E.Put (_, path) ->
                E.Put (info, path)
    in
    aux e

