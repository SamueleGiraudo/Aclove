(* Author: Samuele Giraudo
 * Creation: (feb. 2022), may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, apr. 2023,
 * jun. 2023, jul. 2023, dec. 2023, jan. 2024
 *)

module D = DataErrors
module E = Expressions
module Er = Errors
module I = Information
module L = Lists
module O = Operations
module P = Properties
module S = Substitutions
module V = Variables

(* Returns the number of times this function has been called before during the running time
 * of the execution of the program. This is used to generate a new index. *)
let new_index =
    let index = ref 0 in
    fun _ -> incr index;
    !index

(* Returns the expression which is the variable specified by the nonnegative integer
 * index i. *)
let index_to_variable_expression i =
    assert (i >= 0);
    let v = V.Variable (string_of_int i) in
    E.Variable (I.empty, v)

(* Returns the expression which is the variable specified by the index of the root of the
 * expression e. This expression has to have a root indexed. *)
let root_index_expression e =
    assert (P.has_root_index e);
    index_to_variable_expression (P.root_index e)

(* Returns the expression obtained by indexing the expression e. *)
let set_indices e =
    let rec aux e =
        match e with
            |E.Variable (info, v) ->
                let info' = I.set_index info (new_index ()) in
                E.Variable (info', v)
            |E.Constant (info, c, rules) ->
                let info' = I.set_index info (new_index ()) in
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info', c, rules')
            |E.Self info ->
                let info' = I.set_index info (new_index ()) in
                E.Self info'
            |E.Application (info, e1, e2) ->
                let info' = I.set_index info (new_index ()) in
                E.Application (info', aux e1, aux e2)
            |E.Map (info, e1, e2) ->
                let info' = I.set_index info (new_index ()) in
                E.Map (info', aux e1, aux e2)
            |E.Shadow (info, e1, sh) ->
                let info' = I.set_index info (new_index ()) in
                E.Shadow (info', aux e1, aux sh)
            |E.Alias (info, alias) ->
                let info' = I.set_index info (new_index ()) in
                E.Alias (info', alias)
            |E.AliasDefinition (info, alias, e1, e2) ->
                let info' = I.set_index info (new_index ()) in
                E.AliasDefinition (info', alias, aux e1, aux e2)
            |E.Put (info, path) ->
                let info' = I.set_index info (new_index ()) in
                E.Put (info', path)
    in
    aux e

(* Returns the expression obtained from the expression e by changing the names of its
 * external variables by new fresh names. *)
let refresh_external_variable_names e =
    let lst =
        P.external_variables e
        |> List.map (fun v -> (v, index_to_variable_expression (new_index ())))
    in
    let subs = S.Substitution (fun v -> List.assoc_opt v lst) in
    S.apply_on_expression subs e

(* Returns the list of the pairs (v, index) where v is an external variable of the
 * expression e having index as index. This expression has to be indexed. *)
let external_variables_indices e =
    assert (P.is_indexed e);
    let rec aux e =
        match e with
            |E.Variable (_, v) -> [(v, P.root_index e)]
            |E.Constant _ |E.Self _ |E.Alias _ |E.Put _ -> []
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
            |E.Shadow (_, e1, _) -> aux e1
    in
    aux e

(* Returns the list of the indices of the external selfs of the expression e. This
 * expression has has to be indexed. *)
let external_selfs_indices e =
    assert (P.is_indexed e);
    let rec aux e =
        match e with
            |E.Variable _ |E.Constant _ |E.Alias _ |E.Put _ -> []
            |E.Self _ -> [P.root_index e]
            |E.Application (_, e1, e2) |E.Map (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) ->
                aux e1 @ aux e2
            |E.Shadow (_, e1, _) -> aux e1
    in
    aux e

(* Returns the list of the constraints formed by the pairs (i, sh) where sh is a shadow
 * expression and i is the index referring to a self in the expression e. The expression e
 * has to be indexed. *)
let constraints_selfs e sh =
    assert (P.is_indexed e);
    e
    |> external_selfs_indices
    |> List.map (fun i -> (index_to_variable_expression i, sh))

(* Returns the list of the constraints formed by the pairs of indices referring to the same
 * external variables of the expressions of the list of expressions e_lst. These
 * expressions have to be indexed. *)
let constraints_external_variables e_lst =
    assert (e_lst |> List.for_all P.is_indexed);
    let var_indices = e_lst |> List.map external_variables_indices |> List.flatten in
    var_indices
    |> List.map fst
    |> List.map
        (fun v ->
            match L.images_assoc v var_indices with
                |[] | [_] -> []
                |i :: lst' ->
                    let ive = index_to_variable_expression i in
                    lst' |> List.map (fun i' -> (ive, index_to_variable_expression i')))
    |> List.flatten

(* Returns the constraint between the shadow sh and the shadow expression obtained from the
 * indices of the expressions of the list e_lst of expressions which play the role of inputs
 * and the index of the expression e which play the role of output. The expressions of
 * e_lst and the expressions e and have to have a root index. *)
let constraint_map e_lst e sh =
    assert (e_lst |> List.for_all P.has_root_index);
    assert (P.has_root_index e);
    let t_map =
        List.fold_right
            (fun e res -> E.Map (I.empty, root_index_expression e, res))
            e_lst
            (root_index_expression e)
    in
    (sh, t_map)

(* Returns the list of the shadow constraints from the rule rule and the shadow sh. The
 * constraints take into account of the fact that the expression denoted by the left member
 * of rule and the right member of rule must be unifiable with sh, of the fact that the
 * common external variables of the expression denoted by the left member of rule and the
 * right member of rule must be unifiable, and of the fact that the external selfs of the
 * right member of rule must be unifiable with sh. The expressions of the left member of
 * rule and the expression of the right member of rule have to have a root index. *)
let constraints_rule rule sh =
    assert (fst rule |> List.for_all P.has_root_index);
    assert (P.has_root_index (snd rule));
    let (e_lst, e1) = rule in
    let cstrs_members = [constraint_map e_lst e1 sh] in
    let cstrs_external_variables = constraints_external_variables (e1 :: e_lst) in
    let cstrs_selfs = constraints_selfs e1 sh in
    cstrs_members @ cstrs_external_variables @ cstrs_selfs

(* Returns the list of the shadow constraints from the list of rules rules and the shadow
 * sh. The expressions forming the rules of rules have to have a root index. *)
let constraints_rules rules sh =
    assert (rules |> List.map fst |> List.flatten |> List.for_all P.has_root_index);
    assert (rules |> List.map snd |> List.for_all P.has_root_index);
    rules |> List.map (fun rule -> constraints_rule rule sh) |> List.flatten

(* Returns the expression obtained by renaming the external variables of the expression e in
 * such a way that the variables are more readable. *)
let pack_external_variables e =
    let subs = S.packing (P.external_variables e) in
    S.apply_on_expression subs e

(* Returns the data errors on the composition of the two possible substitutions of the
 * data errors se_1 and se_2. *)
let compose_substitutions se_1 se_2 =
    D.bind_data_2 (fun subs_1 subs_2 -> D.Data (S.compose subs_1 subs_2)) se_1 se_2

(* Returns the data errors on the substitution obtained by adding to the possible
 * substitution of the data errors se the constraint cstr, which is a pair of expressions
 * forming an equation. The expression e is the expression wherein a potential error
 * occurs. *)
let add_constraint e se cstr =
    match se with
        |D.Errors _ -> se
        |D.Data subs -> begin
            let e1 = S.apply_on_expression subs (fst cstr)
            and e2 = S.apply_on_expression subs (snd cstr) in
            match S.unify e1 e2 with
                |Some subs' -> D.Data (S.compose subs' subs)
                |None -> D.Errors [Er.make_non_unifiable_shadows_error e e1 e2]
        end

(* Returns the data errors on the substitution obtained by treating the constraints
 * specified by the possible expression of the data errors ee. This expression has to be
 * inclusion free, alias free, and indexed. The parameter evaluate is a function evaluating
 * data errors on expressions. This is used to compute the evaluation of the internal
 * shadows of the expression. *)
let infer_substitution evaluate ee =
    assert (ee |> D.test_data P.is_inclusion_free);
    assert (ee |> D.test_data P.is_alias_free);
    assert (ee |> D.test_data P.is_indexed);
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ -> D.Data S.empty
            |E.Constant (_, _, rules) ->
                let cstrs = constraints_rules rules (root_index_expression e) in
                let se =
                    rules
                    |> List.map (fun (e_lst, e1) -> e1 :: e_lst |> List.map aux)
                    |> List.flatten
                    |> List.fold_left compose_substitutions (D.Data S.empty)
                in
                cstrs |> List.fold_left (add_constraint e) se
            |E.Application (_, e1, e2) ->
                let cstrs =
                    constraint_map [e2] e (root_index_expression e1)
                    :: constraints_external_variables [e]
                in
                let se = compose_substitutions (aux e1) (aux e2) in
                cstrs |> List.fold_left (add_constraint e) se
            |E.Map (_, e1, e2) ->
                (*
                let e_map =
                    E.Map (I.empty, root_index_expression e1, root_index_expression e2)
                in
                let cstrs =
                    (root_index_expression e, e_map) :: constraints_external_variables [e]
                in
                let se = compose_substitutions (aux e1) (aux e2) in
                cstrs |> List.fold_left (add_constraint e) se
                *)
                compose_substitutions (aux e1) (aux e2)
            |E.Shadow (_, e1, sh) ->
                let se' =
                    D.Data sh
                    |> evaluate
                    |> D.map_data O.remove_shadows
                    |> D.map_data refresh_external_variable_names
                in
                if D.has_errors se' then
                    D.Errors (D.errors se')
                else
                    let sh' = se' |> D.data |> Option.get in
                    let rie = root_index_expression e in
                    let cstrs = [(rie, sh'); (rie, root_index_expression e1)] in
                    cstrs |> List.fold_left (add_constraint e) (aux e1)
            |_ -> E.error e "Shadows.infer_substitution"
    in
    ee |> D.bind_data aux

(* Returns the data errors on an expression which is the result of the shadow inference of
 * the possible expression of the data errors ee. This expression has to be inclusion free,
 * alias free, and indexed. The parameter evaluate is a function evaluating data errors on
 * expressions. This is used to compute the evaluation of the internal shadows of the
 * expression.*)
let infer_shadow evaluate ee =
    assert (ee |> D.test_data P.is_inclusion_free);
    assert (ee |> D.test_data P.is_alias_free);
    assert (ee |> D.test_data P.is_indexed);
    let se = infer_substitution evaluate ee in
    if D.has_errors se then
        D.Errors (D.errors se)
    else
        ee
        |> D.map_data root_index_expression
        |> D.map_data (S.apply_on_expression (D.data se |> Option.get))
        |> D.map_data pack_external_variables

