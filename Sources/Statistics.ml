(* Author: Samuele Giraudo
 * Creation: may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022,
 * apr. 2023, jun. 2023, jul. 2023
 *)

(* A type to collect some statistics about an expression. *)
type statistics = {
    nb_variables: (Levels.levels * int) list;
    nb_constants: (Levels.levels * int) list;
    nb_selfs: (Levels.levels * int) list;
    nb_rules: int;
    nb_applications: int;
    nb_maps: int;
    nb_shadows: int;
    nb_shifts: int;
    nb_aliases: int;
    nb_alias_definitions: int;
    nb_inclusions: int
}

(* Returns a string representation of the statistics st. *)
let to_string st =
    let string_if condition str =
        if condition then str else ""
    in
    string_if (st.nb_variables <> [])
        (Printf.sprintf "Nb. variables:\n%s\n"
            (st.nb_variables
            |> List.map (fun (lvl, nb) ->
                Printf.sprintf "Level %d: %d" (Levels.value lvl) nb)
            |> String.concat "\n"
            |> Strings.indent 4))
    ^
    string_if (st.nb_constants <> [])
        (Printf.sprintf "Nb. constants:\n%s\n"
            (st.nb_constants
            |> List.map (fun (lvl, nb) ->
                Printf.sprintf "Level %d: %d" (Levels.value lvl) nb)
            |> String.concat "\n"
            |> Strings.indent 4))
    ^
    string_if (st.nb_selfs <> [])
        (Printf.sprintf "Nb. selfs:\n%s\n"
            (st.nb_selfs
            |> List.map (fun (lvl, nb) ->
                Printf.sprintf "Level %d: %d" (Levels.value lvl) nb)
            |> String.concat "\n"
            |> Strings.indent 4))
    ^
    string_if (st.nb_rules >= 1) (Printf.sprintf "Nb. rules: %d\n" st.nb_rules)
    ^
    string_if (st.nb_applications >= 1)
        (Printf.sprintf "Nb. applications: %d\n" st.nb_applications)
    ^
    string_if (st.nb_maps >= 1) (Printf.sprintf "Nb. maps: %d\n" st.nb_maps)
    ^
    string_if (st.nb_shadows >= 1) (Printf.sprintf "Nb. shadows: %d\n" st.nb_shadows)
    ^
    string_if (st.nb_shifts >= 1) (Printf.sprintf "Nb. shifts: %d\n" st.nb_shifts)
    ^
    string_if (st.nb_aliases >= 1) (Printf.sprintf "Nb. aliases: %d\n" st.nb_aliases)
    ^
    string_if (st.nb_alias_definitions >= 1)
        (Printf.sprintf "Nb. alias definitions: %d\n" st.nb_alias_definitions)
    ^
    string_if (st.nb_inclusions >= 1)
        (Printf.sprintf "Nb. inclusions: %d\n" st.nb_inclusions)

(* Returns the number of variables of level lvl surveyed by the statistics st. *)
let nb_variables st lvl =
    List.assoc_opt lvl st.nb_variables |> Options.value 0

(* Returns the number of constants of level lvl surveyed by the statistics st. *)
let nb_constants st lvl =
    List.assoc_opt lvl st.nb_constants |> Options.value 0

(* Returns the number of selfs of level lvl surveyed by the statistics st. *)
let nb_selfs st lvl =
    List.assoc_opt lvl st.nb_selfs |> Options.value 0

(* Returns the number of rules surveyed by the statistics st. *)
let nb_rules st =
    st.nb_rules

(* Returns the number of applications surveyed by the statistics st. *)
let nb_applications st =
    st.nb_applications

(* Returns the number of maps surveyed by the statistics st. *)
let nb_maps st =
    st.nb_maps

(* Returns the number of shadows surveyed by the statistics st. *)
let nb_shadows st =
    st.nb_shadows

(* Returns the number of shifts surveyed by the statistics st. *)
let nb_shifts st =
    st.nb_shifts

(* Returns the number of aliases surveyed by the statistics st. *)
let nb_aliases st =
    st.nb_aliases

(* Returns the number of alias definition surveyed by the statistics st. *)
let nb_alias_definitions st =
    st.nb_alias_definitions

(* Returns the number of inclusions surveyed by the statistics st. *)
let nb_inclusions st =
    st.nb_inclusions

(* Returns the empty statistics. *)
let empty =
    {nb_variables = [];
    nb_constants = [];
    nb_selfs = [];
    nb_rules = 0;
    nb_applications = 0;
    nb_maps = 0;
    nb_shadows = 0;
    nb_shifts = 0;
    nb_aliases = 0;
    nb_alias_definitions = 0;
    nb_inclusions = 0}

(* Returns the statistics obtained by merging the statistics st1 and st2. *)
let merge st1 st2 =
    {nb_variables = Lists.merge_assoc (+) st1.nb_variables st2.nb_variables;
    nb_constants = Lists.merge_assoc (+) st1.nb_constants st2.nb_constants;
    nb_selfs = Lists.merge_assoc (+) st1.nb_selfs st2.nb_selfs;
    nb_rules = st1.nb_rules + st2.nb_rules;
    nb_applications = st1.nb_applications + st2.nb_applications;
    nb_maps = st1.nb_maps + st2.nb_maps;
    nb_shadows = st1.nb_shadows + st2.nb_shadows;
    nb_shifts = st1.nb_shifts + st2.nb_shifts;
    nb_aliases = st1.nb_aliases + st2.nb_aliases;
    nb_alias_definitions = st1.nb_alias_definitions + st2.nb_alias_definitions;
    nb_inclusions = st1.nb_inclusions + st2.nb_inclusions}

(* Returns the statistics collected from the expression e. *)
let compute e =
    let rec aux e =
        match e with
            |Expressions.Variable (_, v) ->
                {empty with nb_variables = [(Variables.level v, 1)]}
            |Expressions.Constant (_, c, rules) ->
                let st =
                    rules
                    |> Lists.map_pairs_operation aux merge
                    |> List.fold_left merge empty
                in
                let nb_constants' =
                    Lists.merge_assoc (+) st.nb_constants [(Constants.level c, 1)]
                in
                let nb_rules' = List.length rules in
                {st with nb_constants = nb_constants'; nb_rules = st.nb_rules + nb_rules'}
            |Expressions.Self (_, s) -> {empty with nb_selfs = [(Selfs.level s, 1)]}
            |Expressions.Application (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_applications = 1 + st.nb_applications}
            |Expressions.Map (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_maps = 1 + st.nb_maps}
            |Expressions.Shadow (_, e1, sh) ->
                let st = merge (aux e1) (aux sh) in
                {st with nb_shadows = 1 + st.nb_shadows}
            |Expressions.Shift (_, _, e1) ->
                let st = aux e1 in
                {st with nb_shifts = 1 + st.nb_shifts}
            |Expressions.Alias _ -> {empty with nb_aliases = 1}
            |Expressions.AliasDefinition (_, _, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_alias_definitions = 1 + st.nb_alias_definitions}
            |Expressions.Put _ -> {empty with nb_inclusions = 1}
    in
    aux e

