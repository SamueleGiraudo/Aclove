(* Author: Samuele Giraudo
 * Creation: (feb. 2022), may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, apr. 2023,
 * jun. 2023, jul. 2023
 *)

(* Returns the number of times this function has been called before during the running time
 * of the execution of the program. This is used to generate a new index. *)
let new_index =
    let index = ref 0 in
    fun _ -> incr index;
    !index

(* Returns the expression which is the variable specified by the nonnegative integer
 * index i. *)
let index_to_variable_expression lvl i =
    assert (i >= 0);
    let v = Variables.Variable (lvl, string_of_int i) in
    Expressions.Variable (Information.empty, v)

(* Returns the expression which is the variable specified by the index of the root of the
 * expression e. *)
let root_index_expression e =
    assert (Properties.is_simple e);
    assert (Properties.has_root_index e);
    let lvl = e |> Properties.level |> Levels.shift 1 in
    index_to_variable_expression lvl (Properties.root_index e)

(* Returns the expression obtained by indexing the expression e. This expression has to be
 * simple. *)
let set_indices e =
    assert (Properties.is_simple e);
    let rec aux e =
        match e with
            |Expressions.Variable (info, v) ->
                let info' = Information.set_index info (new_index ()) in
                Expressions.Variable (info', v)
            |Expressions.Constant (info, c, rules) ->
                let info' = Information.set_index info (new_index ()) in
                Expressions.Constant (info', c, rules |> Lists.map_pairs aux)
            |Expressions.Self (info, s) ->
                let info' = Information.set_index info (new_index ()) in
                Expressions.Self (info', s)
            |Expressions.Application (info, e1, e2) ->
                let info' = Information.set_index info (new_index ()) in
                Expressions.Application (info', aux e1, aux e2)
            |Expressions.Map (info, e1, e2) ->
                let info' = Information.set_index info (new_index ()) in
                Expressions.Map (info', aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) ->
                let info' = Information.set_index info (new_index ()) in
                Expressions.Shadow (info', aux e1, aux sh)
            |_ -> Expressions.ValueError (e, "Shadows.set_indices") |> raise
    in
    aux e

(* Returns the expression obtained from the expression e by changing the indices of its
 * subexpressions which are external variables with new indices. *)
let refresh_external_variable_indices e =
    assert (Properties.is_simple e);
    assert (Properties.are_external_variables_indexed e);
    let lst =
        Properties.external_variables e
        |> List.map (fun v ->
            (v, index_to_variable_expression (Variables.level v) (new_index ())))
    in
    let subs = Substitutions.Substitution (fun v -> List.assoc_opt v lst) in
    Substitutions.apply_on_expression subs e

(* Returns the list of the pairs (v, index) where v is an external variable of the
 * expression e having index as index. This expression has to be simple and such that
 * all external variables are indexed. *)
let external_variables_indices e =
    assert (Properties.is_simple e);
    assert (Properties.are_external_variables_indexed e);
    let rec aux e =
        match e with
            |Expressions.Variable (_, v) -> [(v, Properties.root_index e)]
            |Expressions.Constant _ |Expressions.Self _ -> []
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2) ->
                aux e1 @ aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ ->
                Expressions.ValueError (e, "Shadows.external_variables_indices")
                |> raise
    in
    aux e

(* Returns the list of the pairs (s, index) where s is an external self of the expression e
 * having index as index. This expression has to be simple and such that all external selfs
 * are indexed. *)
let external_selfs_indices e =
    assert (Properties.is_simple e);
    assert (Properties.are_external_selfs_indexed e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Constant _ -> []
            |Expressions.Self (_, s) -> [(s, Properties.root_index e)]
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2) ->
                aux e1 @ aux e2
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ -> Expressions.ValueError (e, "Shadows.external_selfs_indices") |> raise
    in
    aux e

(* Returns the list of the constraints formed by the pairs of indices referring to the same
 * external variables of the expressions e. This expression has to be simple and such that
 * all external variables are indexed. *)
let constraints_external_variables e =
    assert (Properties.is_simple e);
    assert (Properties.are_external_variables_indexed e);
    let var_indices = external_variables_indices e in
    Properties.external_variables e
    |> List.map (fun v ->
        let lvl = v |> Variables.level |> Levels.shift 1 in
        match Lists.images_assoc v var_indices with
            |[] | [_] -> []
            |i :: lst' ->
                let ive = index_to_variable_expression lvl i in
                lst' |> List.map (fun i' -> (ive, index_to_variable_expression lvl i')))
    |> List.flatten

(* Returns the list of the shadow constraints from the rule rule and the shadow sh. The
 * constraints take into account of the fact that the left and right members of rule must be
 * unifiable, of the fact that the common external variables of the left and right members
 * of rule must be unifiable, and of the fact that the external selfs of rule must be
 * unifiable with sh. The left and right members of rule have to be simple and indexed, and
 * sh must be simple and external self free. *)
let constraints_rule rule sh =
    assert (Properties.is_simple (fst rule));
    assert (Properties.is_indexed (fst rule));
    assert (Properties.is_indexed (snd rule));
    assert (Properties.is_simple (snd rule));
    assert (Properties.is_simple sh);
    assert (Properties.is_external_self_free sh);
    let (e1, e2) = rule in
    let e = Expressions.Map (Information.empty, e1, e2) in
    let cstr_members = [(root_index_expression e1, root_index_expression e2)] in
    let cstr_external_variables = constraints_external_variables e in
    let cstr_selfs =
        e
        |> external_selfs_indices
        |> List.map (fun (s, i) ->
            let lvl = s |> Selfs.level |> Levels.shift 1 in
            let ive = index_to_variable_expression lvl i in
            (ive, sh))
    in
    cstr_members @ cstr_external_variables @ cstr_selfs

(* Returns the list of the shadow constraints from the list of rules rules and the shadow
 * sh. The expressions forming the rules of rules have to be simple and indexed, and sh must
 * be simple and external self free. *)
let constraints_rules rules sh =
    assert (rules |> List.map fst |> List.for_all Properties.is_simple);
    assert (rules |> List.map snd |> List.for_all Properties.is_simple);
    assert (rules |> List.map fst |> List.for_all Properties.is_indexed);
    assert (rules |> List.map snd |> List.for_all Properties.is_indexed);
    assert (Properties.is_simple sh);
    assert (Properties.is_external_self_free sh);
    rules |> List.map (fun rule -> constraints_rule rule sh) |> List.flatten

(* Returns the shadow of the expression e in the form of an application. This expression has
 * to be simple and indexed. *)
let map_shadow e =
    assert (Properties.is_simple e);
    assert (Properties.is_indexed e);
    match e with
        |Expressions.Application (_, _, e2) ->
            let rie = root_index_expression e and rie2 = root_index_expression e2 in
            Expressions.Map (Information.empty, rie2, rie)
        |_ -> Expressions.ValueError (e, "Shadows.map_shadow") |> raise

(* Returns the expression obtained by renaming the external variables of the expression e in
 * such a way that the variables are more readable. The expression e has to be simple. *)
let pack_external_variables e =
    assert (Properties.is_simple e);
    let subs = Substitutions.packing (Properties.external_variables e) in
    Substitutions.apply_on_expression subs e

(* Returns an option on the substitution obtained by adding to the substitution subs the
 * constraints of the list cstr, which is a list of pairs of expressions forming equations.
 * None is returned when the constraints are inconsistent w.r.t. subs. *)
let add_constraints_to_substitution subs cstr =
    cstr
    |> List.fold_left (fun res (e1, e2) ->
        res
        |> Options.bind (fun subs ->
            let se1 = Substitutions.apply_on_expression subs e1
            and se2 = Substitutions.apply_on_expression subs e2 in
            Substitutions.unify se1 se2 |> Options.bind (fun subs' ->
                Some (Substitutions.compose subs' subs))))
        (Some subs)

(* Returns the expression obtained from the expression e by removing its shadows. This
 * expression has to have no inclusion free, alias free, and shift free. *)
let remove_shadows e =
    assert (Properties.is_inclusion_free e);
    assert (Properties.is_alias_free e);
    assert (Properties.is_shift_free e);
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Alias _ |Expressions.Self _ -> e
            |Expressions.Constant (info, c, rules) ->
                Expressions.Constant (info, c, rules |> Lists.map_pairs aux)
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (_, e1, _) -> aux e1
            |_ -> Expressions.ValueError (e, "Shadows.remove_shadows") |> raise
    in
    aux e

(* Returns an option to the substitution which unify the shadow constraints of the
 * expression e. None is returned when this expression is not typable. This expression has
 * to be simple, external self free, and indexed. *)
let infer_substitution e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    assert (Properties.is_indexed e);
    let compose subs_opt_1 subs_opt_2 =
        Options.bind_2
            (fun subs_1 subs_2 -> Some (Substitutions.compose subs_1 subs_2))
            subs_opt_1
            subs_opt_2
    in
    let update subs_opt cstr =
        subs_opt |> Options.bind (fun subs -> add_constraints_to_substitution subs cstr)
    in
    let rec aux e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ -> Some Substitutions.empty
            |Expressions.Constant (_, _, rules) ->
                let cstr =
                    constraints_rules rules (root_index_expression e)
                    |> Lists.map_pairs remove_shadows
                in
                let subs =
                    rules
                    |> Lists.map_pairs_operation aux compose
                    |> List.fold_left compose (Some Substitutions.empty)
                in
                update subs cstr
            |Expressions.Application (_, e1, e2) ->
                let cstr =
                    (root_index_expression e1, map_shadow e)
                    :: constraints_external_variables e
                    |> Lists.map_pairs remove_shadows
                in
                let subs = compose (aux e1) (aux e2) in
                update subs cstr
            |Expressions.Map (_, e1, e2) ->
                let cstr =
                    constraints_external_variables e |> Lists.map_pairs remove_shadows
                in
                let subs = compose (aux e1) (aux e2) in
                update subs cstr
            |Expressions.Shadow (_, e1, sh) ->
                let sh' = sh
                |> remove_shadows
                |> Evaluations.compute |> refresh_external_variable_indices in
                let rie = root_index_expression e in
                let cstr =
                    [(rie, sh'); (rie, root_index_expression e1)]
                    |> Lists.map_pairs remove_shadows
                in
                let subs = compose (aux e1) (aux sh) in
                update subs cstr
            |_ -> Expressions.ValueError (e, "Shadows.infer_substitution") |> raise
    in
    aux e

(* Returns an option on the shadow of the expression e. This expression has to be simple and
 * external self free. When e does not admit any shadow, None is returned. *)
let compute e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    let e' = set_indices e in
    e'
    |> infer_substitution
    |> Options.bind (fun subs ->
        Some (e'
            |> root_index_expression
            |> Substitutions.apply_on_expression subs
            |> pack_external_variables))

