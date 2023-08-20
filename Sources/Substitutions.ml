(* Author: Samuele Giraudo
 * Creation: jan. 2021
 * Modifications: jan. 2021, feb. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022,
 * jun. 2022, aug. 2022, sep. 2022, oct. 2022, apr. 2023, jun. 2023, jul. 2023
 *)

(* A substitution is a map sending variables to options on expressions. *)
type substitutions =
    Substitution of (Variables.variables -> (Expressions.expressions option))

(* Returns the map of the substitution subs. *)
let map subs =
    let Substitution map = subs in
    map

(* Returns the empty substitution. This is the identity. *)
let empty =
    Substitution (Fun.const None)

(* Returns the singleton substitution sending the variable v to the expression e. *)
let singleton v e =
    Substitution (fun v' -> if v' = v then Some e else None)

(* Returns the substitution obtained by sending each variable of the list domain of
 * variables to the variables ranging from 1 to n, where n is the length of domain. *)
let packing domain =
    let lst =
        domain
        |> List.mapi (fun i v ->
            let v' = Variables.Variable (Variables.level v, string_of_int (i + 1)) in
            (v, Expressions.Variable (Information.empty, v')))
    in
    Substitution (fun v -> List.assoc_opt v lst)

(* Returns the expression obtained by applying the substitution subs on the expression e.
 * This replaces each external variable v of e by the expression being the image of v by
 * subs. If such variable has no image by subs, it remains unchanged in the resulting
 * expression. The expression e has to be simple. *)
let apply_on_expression subs e =
    assert (Properties.is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable (_, v) -> map subs v |> Options.value e
            |Expressions.Constant _ |Expressions.Self _ -> e
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, sh)
            |_ -> Expressions.ValueError (e, "Substitutions.apply_on_expression") |> raise
    in
    aux e

(* Returns the substitution defined as the disjoint union of the substitutions subs1 and
 * subs2. *)
let disjoint_union subs1 subs2 =
    let map v =
        match map subs1 v with
            |None -> map subs2 v
            |Some _ as res -> res
    in
    Substitution map

(* Returns an option on the substitution obtained by matching the expression e1 with a
 * prefix of the expression e2. None is returned when such a matching does not exist. The
 * expressions e1 and e2 have to be simple and external self free. *)
let matching_prefix e1 e2 =
    assert (Properties.is_simple e1);
    assert (Properties.is_simple e2);
    assert (Properties.is_external_self_free e1);
    assert (Properties.is_external_self_free e2);
    assert (Properties.is_shadow_free e1);
    assert (Properties.is_shadow_free e2);
    let rec aux e1 e2 =
        match e1, e2 with
            |Expressions.Variable (_, v1), _ -> Some (singleton v1 e2)
            |Expressions.Constant (_, c1, _), Expressions.Constant (_, c2, _) ->
                if c1 = c2 then Some empty else None
            |Expressions.Application (_, e1, e1'), Expressions.Application (_, e2, e2')
            |Expressions.Map (_, e1, e1'), Expressions.Map (_, e2, e2') ->
                Options.bind_2
                    (fun subs1 subs2 -> Some (disjoint_union subs1 subs2))
                    (aux e1 e2)
                    (aux e1' e2')
            (*
            |Expressions.Shadow (_, e1, _), e2 |e1, Expressions.Shadow (_, e2, _) ->
                aux e1 e2
            *)
            |_ -> None
    in
    aux e1 e2

(* Returns the substitution obtained by composing the substitutions subs1 and subs2. The
 * image of a variable v is hence the expression image of v by subst2 wherein its variables
 * are replaced by their images by subs1. *)
let compose subs1 subs2 =
    let map v =
        match map subs2 v with
            |None -> map subs1 v
            |Some e -> Some (apply_on_expression subs1 e)
    in
    Substitution map

(* Returns an option on the most general unifier of the expressions e1 and e2. None is
 * returned when these expressions are not unifiable. The expressions e1 and e2 have to be
 * simple and external self free. *)
let unify e1 e2 =
    assert (Properties.is_simple e1);
    assert (Properties.is_simple e2);
    assert (Properties.is_external_self_free e1);
    assert (Properties.is_external_self_free e2);
    assert (Properties.is_shadow_free e1);
    assert (Properties.is_shadow_free e2);
    let rec aux e1 e2 =
        match e1, e2 with
            |Expressions.Variable (_, v), Expressions.Variable (_, v') when v = v' ->
                Some empty
            |Expressions.Variable (_, v), e'' when Properties.has_external_variable e'' v ->
                None
            |e'', Expressions.Variable (_, v) when Properties.has_external_variable e'' v ->
                None
            |Expressions.Variable (_, v), e'' |e'', Expressions.Variable (_, v) ->
                Some (singleton v e'')
            |Expressions.Constant (_,  c, _), Expressions.Constant (_,  c', _) ->
                if c = c' then Some empty else None
            |Expressions.Application (_, e1', e1''), Expressions.Application (_, e2', e2'')
            |Expressions.Map (_, e1', e1''), Expressions.Map (_, e2', e2'') ->
                aux e1' e2'
                |> Options.bind (fun subs' ->
                    let se1 = apply_on_expression subs' e1''
                    and se2 = apply_on_expression subs' e2'' in
                    aux se1 se2
                    |> Options.bind (fun subs'' -> Some (compose subs'' subs')))
            (*
            |Expressions.Shadow (_, e1, _), e2 |e1, Expressions.Shadow (_, e2, _) ->
                aux e1 e2*)
            |_ -> None
    in
    aux e1 e2

