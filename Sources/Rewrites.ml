(* Author: Samuele Giraudo
 * Creation: (oct. 2018), mar. 2020
 * Modifications: mar. 2020, apr. 2020, may 2020, jun. 2020, jan. 2021, feb. 2021,
 * nov. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022, sep. 2022, oct. 2022, dec. 2022,
 * jan. 2023, apr. 2023, jun. 2023, jul. 2023, dec. 2023, jan. 2024
 *)

module E = Expressions
module O = Options
module S = Substitutions

(* Returns the expression obtained by replacing all external selfs of the expression e by
 * the expression e_self. *)
let substitute_external_selfs e_self e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Constant _ |E.Alias _ |E.Put _ -> e
            |E.Self _ -> e_self
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns an option on the substitution defined as the disjoint union of the substitution
 * which is the prefix matching of each expression of the list e_lst_1 of expressions with
 * the expression at the same position of the list e_lst_2 of expressions. When these two
 * lists have different length or when such a matching does not exists, None is returned. *)
let matching_prefix_forests e_lst_1 e_lst_2 =
    let rec aux e_lst_1 e_lst_2 =
        match e_lst_1, e_lst_2 with
            |[], [] -> Some S.empty
            |[], _ |_, [] -> None
            |e1 :: e_lst_1', e2 :: e_lst_2' ->
                S.matching_prefix e1 e2
                |> Options.bind
                    (fun s -> aux e_lst_1' e_lst_2' |> Option.map (S.disjoint_union s))
    in
    aux e_lst_1 e_lst_2

(* Returns an option on the expression obtained by rewriting the expression e following the
 * rules of its dominant constant if it has one. The considered rule is the first one which
 * is admissible. Returns None if such a rewrite is not possible. *)
let try_rewrite_prefix e =
    let rec aux e arg_lst =
        match e with
            |E.Constant (_, _, rules) ->
                rules
                |> List.find_map
                    (fun (e_lst, e1) ->
                        matching_prefix_forests e_lst arg_lst
                        |> Option.map
                            (fun subs ->
                                let e' = substitute_external_selfs e e1 in
                                S.apply_on_expression subs e'))
            |E.Application (_, e1, e2) -> aux e1 (e2 :: arg_lst)
            |_ -> None
    in
    aux e []

(* Returns the normal form of the expression e. *)
let normal_form e =
    let rec step e =
        let res = try_rewrite_prefix e in
        if Option.is_some res then
            res
        else
            match e with
                |E.Application (info, e1, e2) -> begin
                    match step e1, step e2 with
                        |None, None -> None
                        |res1, res2 ->
                            let e1' = res1 |> O.value e1 and e2' = res2 |> O.value e2 in
                            Some (E.Application (info, e1', e2'))
                end
                |E.Map (info, e1, e2) -> begin
                    match step e1, step e2 with
                        |None, None -> None
                        |res1, res2 ->
                            let e1' = res1 |> O.value e1 and e2' = res2 |> O.value e2 in
                            Some (E.Map (info, e1', e2'))
                end
                |_ -> None
    in
    O.iter step e

(* Returns the expression obtained from the expression e by evaluating the left members of
 * its rules. *)
let resolve_left_members_rules e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ |E.Put _ -> e
            |E.Constant (info, c, rules) ->
                let rules' =
                    rules
                    |> List.map
                        (fun (e_lst, e1) ->
                            e_lst |> List.map aux |> List.map normal_form, aux e1)
                in
                E.Constant (info, c, rules')
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

