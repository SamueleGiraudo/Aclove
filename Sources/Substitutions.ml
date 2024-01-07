(* Author: Samuele Giraudo
 * Creation: jan. 2021
 * Modifications: jan. 2021, feb. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022,
 * jun. 2022, aug. 2022, sep. 2022, oct. 2022, apr. 2023, jun. 2023, jul. 2023, dec. 2023,
 * jan. 2024
 *)

module E = Expressions
module I = Information
module O = Options
module P = Properties
module V = Variables

(* A substitution is a map sending variables to options on expressions. *)
type substitutions =
    Substitution of (V.variables -> (E.expressions option))

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
        |> List.mapi
            (fun i v ->
                let v' = V.Variable (string_of_int (i + 1)) in
                (v, E.Variable (I.empty, v')))
    in
    Substitution (fun v -> List.assoc_opt v lst)

(* Returns the expression obtained by applying the substitution subs on the expression e.
 * This replaces each external variable v of e by the expression being the image of v by
 * subs. If such variable has no image by subs, it remains unchanged in the resulting
 * expression. *)
let apply_on_expression subs e =
    let rec aux e =
        match e with
            |E.Variable (_, v) -> map subs v |> O.value e
            |E.Constant _ |E.Self _ |E.Alias _ |E.Put _ -> e
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
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

(* Returns an option on the substitution obtained by matching the expression e1 as a prefix
 * of the expression e2. None is returned when such a matching does not exist. *)
let matching_prefix e1 e2 =
    let rec aux e1 e2 =
        match e1, e2 with
            |E.Variable (_, v1), _ -> Some (singleton v1 e2)
            |E.Constant (_, c1, _), E.Constant (_, c2, _) when c1 = c2 -> Some empty
            |E.Self _, E.Self _ -> Some empty
            |E.Application (_, e1', e1''), E.Application (_, e2', e2'')
            |E.Map (_, e1', e1''), E.Map (_, e2', e2'')
            |E.Shadow (_, e1', e1''), E.Shadow (_, e2', e2'') ->
                O.bind_2
                    (fun subs1 subs2 -> Some (disjoint_union subs1 subs2))
                    (aux e1' e2')
                    (aux e1'' e2'')
            |E.Alias (_, alias_1), E.Alias (_, alias_2) when alias_1 = alias_2 ->
                Some empty
            |E.AliasDefinition (_, alias_1, e1', e1''),
                    E.AliasDefinition (_, alias_2, e2', e2'') when alias_1 = alias_2 ->
                O.bind_2
                    (fun subs1 subs2 -> Some (disjoint_union subs1 subs2))
                    (aux e1' e2')
                    (aux e1'' e2'')
            |E.Put (_, path_1), E.Put (_, path_2) when path_1 = path_2 -> Some empty
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
 * returned when these expressions are not unifiable. *)
let unify e1 e2 =
    let rec aux e1 e2 =
        match e1, e2 with
            |E.Variable (_, v1), E.Variable (_, v2) when v1 = v2 -> Some empty
            |E.Variable (_, v1), _ when P.has_external_variable e2 v1 -> None
            |_, E.Variable (_, v2) when P.has_external_variable e1 v2 -> None
            |E.Variable (_, v), e |e, E.Variable (_, v) -> Some (singleton v e)
            |E.Constant (_,  c1, _), E.Constant (_,  c2, _) when c1 = c2 -> Some empty
            |E.Self _, E.Self _ -> Some empty
            |E.Application (_, e1', e1''), E.Application (_, e2', e2'')
            |E.Map (_, e1', e1''), E.Map (_, e2', e2'')
            |E.Shadow (_, e1', e1''), E.Shadow (_, e2', e2'') ->
                aux e1' e2'
                |> O.bind
                    (fun subs' ->
                        let se1 = apply_on_expression subs' e1''
                        and se2 = apply_on_expression subs' e2'' in
                        aux se1 se2
                        |> O.bind (fun subs'' -> Some (compose subs'' subs')))
            |E.Alias (_, alias_1), E.Alias (_, alias_2) when alias_1 = alias_2 -> Some empty
            |E.AliasDefinition (_, alias_1, e1', e1''),
                    E.AliasDefinition (_, alias_2, e2', e2'') when alias_1 = alias_2 ->
                aux e1' e2'
                |> O.bind
                    (fun subs' ->
                        let se1 = apply_on_expression subs' e1''
                        and se2 = apply_on_expression subs' e2'' in
                        aux se1 se2
                        |> O.bind (fun subs'' -> Some (compose subs'' subs')))
            |E.Put (_, path_1), E.Put (_, path_2) when path_1 = path_2 -> Some empty
            |_ -> None
    in
    aux e1 e2

