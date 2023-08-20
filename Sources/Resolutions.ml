(* Author: Samuele Giraudo
 * Creation: (jan. 2021), jul. 2023
 * Modifications: jul. 2023
 *)

(* Returns the expression obtained by replacing all free occurrences of the alias alias in
 * the expression e1 by the expression e2. These two expressions have to be inclusion
 * free. *)
let substitute_free_aliases e1 alias e2 =
    assert (Properties.is_inclusion_free e1);
    assert (Properties.is_inclusion_free e2);
    let rec aux e1 =
        match e1 with
            |Expressions.Variable _ |Expressions.Self _ -> e1
            |Expressions.Constant (info, c, rules) ->
                Expressions.Constant (info, c, rules |> Lists.map_pairs aux)
            |Expressions.Application (info, e1', e2') ->
                Expressions.Application (info, aux e1', aux e2')
            |Expressions.Map (info, e1', e2') -> Expressions.Map (info, aux e1', aux e2')
            |Expressions.Shift (info, lvl, e1) -> Expressions.Shift (info, lvl, aux e1)
            |Expressions.Shadow (info, e1', sh) ->
                Expressions.Shadow (info, aux e1', aux sh)
            |Expressions.Alias (_, alias') -> if alias' = alias then e2 else e1
            |Expressions.AliasDefinition (info, alias', e1', e2') ->
                let e2'' = if alias' = alias then e2' else aux e2' in
                Expressions.AliasDefinition (info, alias', aux e1', e2'')
            |_ ->
                Expressions.ValueError (e1, "Resolutions.substitute_free_aliases") |> raise
    in
    aux e1

(* Returns the expression obtained from the expression e by resolving its inclusions. The
 * expression e has to have no inclusion errors. *)
let resolve_inclusions e =
    assert (Errors.inclusion_errors e = []);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ |Expressions.Alias _ -> e
            |Expressions.Constant (info, c, rules) ->
                Expressions.Constant (info, c, rules |> Lists.map_pairs aux)
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, aux sh)
            |Expressions.Shift (info, lvl, e1) -> Expressions.Shift (info, lvl, aux e1)
            |Expressions.AliasDefinition (info, alias, e1, e2) ->
                Expressions.AliasDefinition (info, alias, aux e1, aux e2)
            |Expressions.Put (_, path) ->
                let path' = Paths.simplify (Files.add_file_extension path) in
                aux (Files.path_to_expression path')
    in
    aux e

(* Returns the expression obtained by replacing all the aliases in the expression e by their
 * definitions. This expression has to be inclusion free. *)
let resolve_alias_definitions e =
    assert (Properties.is_inclusion_free e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Alias _ |Expressions.Self _ -> e
            |Expressions.Constant (info, c, rules) ->
                Expressions.Constant (info, c, rules |> Lists.map_pairs aux)
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, aux sh)
            |Expressions.Shift (info, lvl, e1) -> Expressions.Shift (info, lvl, aux e1)
            |Expressions.AliasDefinition (_, alias, e1, e2) ->
                substitute_free_aliases (aux e2) alias (aux e1)
            |_ ->
                Expressions.ValueError (e, "Resolutions.resolve_alias_definitions") |> raise
    in
    aux e

(* Returns the expression obtained from the expression e by resolving its shifts. The
 * expression e has to have no inclusion free and alias free. *)
let resolve_shifts e =
    assert (Properties.is_inclusion_free e);
    assert (Properties.is_alias_free e);
    let rec aux lvl e =
        match e with
            |Expressions.Variable (info, v) ->
                Expressions.Variable (info, Variables.shift_level lvl v)
            |Expressions.Constant (info, c, rules) ->
                let c' = Constants.shift_level lvl c in
                let rules' = rules |> Lists.map_pairs (aux lvl) in
                Expressions.Constant (info, c', rules')
            |Expressions.Self (info, s) -> Expressions.Self (info, Selfs.shift_level lvl s)
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux lvl e1, aux lvl e2)
            |Expressions.Map (info, e1, e2) ->
                Expressions.Map (info, aux lvl e1, aux lvl e2)
            |Expressions.Shadow (info, e1, sh) ->
                Expressions.Shadow (info, aux lvl e1, aux lvl sh)
            |Expressions.Shift (_, lvl', e1) -> aux (Levels.add lvl lvl') e1
            |_ -> Expressions.ValueError (e, "Resolutions.resolve_shifts") |> raise
    in
    aux (Levels.Level 0) e

