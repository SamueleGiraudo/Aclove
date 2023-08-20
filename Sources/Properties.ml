(* Author: Samuele Giraudo
 * Creation: (may 2022), oct. 2022
 * Modifications: oct. 2022, nov. 2022, dec. 2022, jan. 2023, apr. 2023, jun. 2023,
 * jul. 2023
 *)

(* Tests if there are no inclusions in the expression e. *)
let is_inclusion_free e =
    Statistics.nb_inclusions (Statistics.compute e) = 0

(* Tests if there are no alias uses in the expression e. This expression has to be inclusion
 * free. *)
let is_alias_free e =
    assert (is_inclusion_free e);
    let st = Statistics.compute e in
    Statistics.nb_aliases st = 0 && Statistics.nb_alias_definitions st = 0

(* Tests if there are no shifts in the expression e. This expression has to be inclusion
 * free and alias free. *)
let is_shift_free e =
    assert (is_inclusion_free e);
    assert (is_alias_free e);
    let st = Statistics.compute e in
    Statistics.nb_shifts st = 0

(* Tests if the expression e is simple. *)
let is_simple e =
    is_inclusion_free e && is_alias_free e && is_shift_free e

(* Tests if the expression e has no external selfs. This expression has to be simple. *)
let is_external_self_free e =
    assert (is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Constant _ -> true
            |Expressions.Self _ -> false
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2) ->
                aux e1 && aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ -> Expressions.ValueError (e, "Properties.is_external_self_free") |> raise
    in
    aux e

(* Tests if there are shadow in the expression e. This expression has to be inclusion
 * free. *)
let is_shadow_free e =
    assert (is_simple e);
    let st = Statistics.compute e in
    Statistics.nb_shadows st = 0

(* Returns the information data of the root of the expression e. *)
let root_information e =
    match e with
        |Expressions.Variable (info, _)
        |Expressions.Constant (info, _, _)
        |Expressions.Self (info, _)
        |Expressions.Application (info, _, _)
        |Expressions.Map (info, _, _)
        |Expressions.Shadow (info, _, _)
        |Expressions.Shift (info, _, _)
        |Expressions.Alias (info, _)
        |Expressions.AliasDefinition (info, _, _, _)
        |Expressions.Put (info, _) ->
            info

(* Tests if the root of the expression e has an index. *)
let has_root_index e =
    root_information e |> Information.index |> Option.is_some

(* Returns the index of the root of the expression e. This expression must be simple and
 * its root must be indexed. *)
let root_index e =
    assert (is_simple e);
    assert (has_root_index e);
    e |> root_information |> Information.index |> Option.get

(* Tests if the expression e is so that at each node, an index is defined. This expression
 * has to be simple. *)
let is_indexed e =
    assert (is_simple e);
    let rec aux e =
        if has_root_index e then
            match e with
                |Expressions.Variable _ |Expressions.Self _ -> true
                |Expressions.Constant (_, _, rules) ->
                    rules |> List.for_all (fun (e1, e2) -> aux e1 && aux e2)
                |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
                |Expressions.Shadow (_, e1, e2) ->
                    aux e1 && aux e2
                |_ -> Expressions.ValueError (e, "Properties.is_indexed") |> raise
        else
            false
    in
    aux e

(* Tests if the expression e is so that at each external variable node, an index is defined.
 * This expression has to be simple. *)
let are_external_variables_indexed e =
    assert (is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ -> has_root_index e
            |Expressions.Constant _ |Expressions.Self _ -> true
            |Expressions.Application (_, e1, e2)| Expressions.Map (_, e1, e2) ->
                aux e1 && aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ ->
                Expressions.ValueError (e, "Properties.are_external_variables_indexed")
                |> raise
    in
    aux e

(* Tests if the expression e is so that at each external self node, an index is defined.
 * This expression has to be simple. *)
let are_external_selfs_indexed e =
    assert (is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Constant _ -> true
            |Expressions.Self _ -> has_root_index e
            |Expressions.Application (_, e1, e2)| Expressions.Map (_, e1, e2) ->
                aux e1 && aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ ->
                Expressions.ValueError (e, "Properties.are_external_selfs_indexed")
                |> raise
    in
    aux e

(* Returns the dominant leaf of the expression e. The expression e has to be simple. *)
let dominant_leaf e =
    assert (is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Constant _ |Expressions.Self _ -> e
            |Expressions.Application (_, e', _) |Expressions.Map (_, _, e')
            |Expressions.Shadow (_, e', _) ->
                aux e'
            |_ -> Expressions.ValueError (e, "Properties.dominant_leaf") |> raise
    in
    aux e

(* Returns the level of the expression e. This expression has to have a dominant leaf which
 * is either a variable or a constant. *)
let level e =
    assert (is_simple e);
    match dominant_leaf e with
        |Expressions.Variable (_, v) -> Variables.level v
        |Expressions.Constant (_, c, _) -> Constants.level c
        |Expressions.Self (_, s) -> Selfs.level s
        |_ -> Expressions.ValueError (e, "Properties.level") |> raise

(* Returns the list of the external variables of the expression e in the order they appear.
 *The expression e has to  be simple. *)
let external_variable_sequence e =
    assert (is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable (_, v) -> [v]
            |Expressions.Constant _ |Expressions.Self _ -> []
            |Expressions.Application (_, e1, e2)| Expressions.Map (_, e1, e2) ->
                aux e1 @ aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ ->
                Expressions.ValueError (e, "Properties.external_variable_sequence") |> raise
    in
    aux e

(* Returns the uniquely sorted list of the external variables of the expression e. This
 * expression has to be simple. *)
let external_variables e =
    assert (is_simple e);
    e |> external_variable_sequence |> List.sort_uniq compare

(* Tests if the expression contains the external variable v. This expression has to be
 * simple. *)
let has_external_variable e v =
    assert (is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable (_, v') -> v' = v
            |Expressions.Constant _ -> false
            |Expressions.Self _ -> false
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2) ->
                aux e1 || aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ -> Expressions.ValueError (e, "Properties.has_external_variable") |> raise
    in
    aux e

(* Returns the expression obtained from the expression e by removing its rules. This
 * expression has to have no inclusion free, alias free, shift free, and shadow free. *)
let remove_rules e =
    assert (is_inclusion_free e);
    assert (is_alias_free e);
    assert (is_shift_free e);
    (*assert (is_shadow_free e);*)
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Alias _ |Expressions.Self _ -> e
            |Expressions.Constant (info, c, _) -> Expressions.Constant (info, c, [])
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, aux sh)
            |_ -> Expressions.ValueError (e, "Properties.remove_rules") |> raise
    in
    aux e

