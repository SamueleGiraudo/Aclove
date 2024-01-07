(* Author: Samuele Giraudo
 * Creation: (may 2022), oct. 2022
 * Modifications: oct. 2022, nov. 2022, dec. 2022, jan. 2023, apr. 2023, jun. 2023,
 * jul. 2023, dec. 2023, jan. 2024
 *)

module E = Expressions
module I = Information
module S = Statistics

(* Tests if there are no inclusions in the expression e. *)
let is_inclusion_free e =
    e |> S.compute |> S.nb_inclusions = 0

(* Tests if there are no alias uses in the expression e. *)
let is_alias_free e =
    e |> S.compute |> S.nb_alias_definitions = 0

(* Tests if there are no shadows in the expression e. *)
let is_shadow_free e =
    e |> S.compute |> S.nb_shadows = 0

(* Tests if there are no maps in the expression e. *)
let is_map_free e =
    e |> S.compute |> S.nb_maps = 0

(* Tests if the expression e has no external selfs. *)
let is_external_self_free e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Constant _ |E.Alias _ |E.Put _ -> true
            |E.Self _ -> false
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 && aux e2
            |E.Shadow (_, e1, _) -> aux e1
    in
    aux e

(* Tests if the expression e has no external maps. *)
let is_external_map_free e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Constant _ |E.Self _ |E.Alias _ |E.Put _ -> true
            |E.Application (_, e1, e2) |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 && aux e2
            |E.Map _ -> false
            |E.Shadow (_, e1, _) -> aux e1
    in
    aux e

(* Returns the information data of the root of the expression e. *)
let root_information e =
    match e with
        |E.Variable (info, _) |E.Constant (info, _, _) |E.Self info
        |E.Application (info, _, _) |E.Map (info, _, _) |E.Shadow (info, _, _)
        |E.Alias (info, _) |E.AliasDefinition (info, _, _, _) |E.Put (info, _) ->
            info

(* Tests if the root of the expression e has an index. *)
let has_root_index e =
    root_information e |> I.index |> Option.is_some

(* Returns the index of the root of the expression e. This expression has to have a root
 * indexed. *)
let root_index e =
    assert (has_root_index e);
    e |> root_information |> I.index |> Option.get

(* Tests if the expression e is so that at each node, an index is defined. *)
let is_indexed e =
    let rec aux e =
        if has_root_index e then
            match e with
                |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> true
                |E.Constant (_, _, rules) ->
                    rules
                    |> List.for_all (fun (e_lst, e1) -> e1 :: e_lst |> List.for_all aux)
                |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2)
                |E.AliasDefinition (_, _, e1, e2) ->
                    aux e1 && aux e2
        else
            false
    in
    aux e

(* Returns the list of the external variables of the expression e. The order of the
 * variables in the list is the order, from left to right, of the variables in e. *)
let external_variable_sequence e =
    let rec aux e =
        match e with
            |E.Variable (_, v) -> [v]
            |E.Constant _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
            |E.Shadow (_, e1, _) -> aux e1
    in
    aux e

(* Returns the uniquely sorted list of the external variables of the expression e. *)
let external_variables e =
    e |> external_variable_sequence |> List.sort_uniq compare

(* Tests if the expression contains the external variable v. *)
let has_external_variable e v =
    let rec aux e =
        match e with
            |E.Variable (_, v') -> v' = v
            |E.Constant _ |E.Self _ |E.Alias _ |E.Put _ -> false
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 || aux e2
            |E.Shadow (_, e1, _) -> aux e1
    in
    aux e

