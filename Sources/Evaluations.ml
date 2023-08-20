(* Author: Samuele Giraudo
 * Creation: (oct. 2018), mar. 2020
 * Modifications: mar. 2020, apr. 2020, may 2020, jun. 2020, jan. 2021, feb. 2021,
 * nov. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022, sep. 2022, oct. 2022, dec. 2022,
 * jan. 2023, apr. 2023, jun. 2023, jul. 2023
 *)

(* Returns the expression obtained by replacing all external selfs of the expression e by
 * the expression e_self. These two expressions e and e_self must be simple. *)
let substitute_self e e_self =
    assert (Properties.is_simple e);
    assert (Properties.is_simple e_self);
    assert (Properties.is_shadow_free e);
    assert (Properties.is_shadow_free e_self);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Constant _ -> e
            |Expressions.Self _ -> e_self
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            (*|Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, sh)*)
            |_ -> Expressions.ValueError (e, "Evaluations.substitute_self") |> raise
    in
    aux e

(* Returns an option on the expression obtained by rewriting the expression e following the
 * rule rule such that its left member is a prefix of e. Returns None if this rewrite is not
 * possible. The expression e and the two expressions of rule have to be simple and external
 * self free. *)
let try_rewrite_prefix e rule =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    assert (Properties.is_simple (fst rule));
    assert (Properties.is_simple (snd rule));
    assert (Properties.is_external_self_free (fst rule));
    assert (Properties.is_external_self_free (snd rule));
    let (e1, e2) = rule in
    let m = Substitutions.matching_prefix e1 e in
    Option.map (fun m -> Substitutions.apply_on_expression m e2) m

(* Returns an option on the expression obtained by rewriting the expression e following the
 * rules of its dominant constant. The considered rule is the first one which is admissible.
 * Returns None if such a rewrite is not possible. The expression e has to be simple and
 * external self free. *)
let try_rewrite_prefix_dominant e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    match Properties.dominant_leaf e with
        |Expressions.Variable _ -> None
        |Expressions.Constant (_, _, rules) as e' ->
            rules
            |> Lists.map_pairs (fun r -> substitute_self r e')
            |> List.find_map (try_rewrite_prefix e)
        |_ -> Expressions.ValueError (e, "Evaluations.try_rewrite_prefix_dominant") |> raise

(* Returns the evaluation of the expression e. This expression has to be simple and
 * external self free. *)
let compute e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    assert (Properties.is_shadow_free e);
    let rec step e =
        match try_rewrite_prefix_dominant e with
            |Some _ as res -> res
            |None -> begin
                match e with
                    |Expressions.Application (info, e1, e2) -> begin
                        match step e1, step e2 with
                            |None, None -> None
                            |res1, res2 ->
                                let e1' = res1 |> Options.value e1 in
                                let e2' = res2 |> Options.value e2 in
                                Some (Expressions.Application (info, e1', e2'))
                    end
                    |Expressions.Map (info, e1, e2) -> begin
                        match step e1, step e2 with
                            |None, None -> None
                            |res1, res2 ->
                                let e1' = res1 |> Options.value e1 in
                                let e2' = res2 |> Options.value e2 in
                                Some (Expressions.Map (info, e1', e2'))
                    end
                    |_ -> None
            end
    in
    Options.iter step e

