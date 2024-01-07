(* Author: Samuele Giraudo
 * Creation: may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022,
 * apr. 2023, jun. 2023, jul. 2023, dec. 2023
 *)

module C = Constants
module E = Expressions
module Ls = Lists
module O = Options
module St = Strings
module V = Variables

(* A type to collect some statistics about an expression. *)
type statistics = {
    nb_variables: int;
    nb_constants: int;
    nb_selfs: int;
    nb_rules: int;
    nb_applications: int;
    nb_maps: int;
    nb_shadows: int;
    nb_aliases: int;
    nb_alias_definitions: int;
    nb_inclusions: int
}

(* Returns a string representation of the statistics st. *)
let to_string st =
    let string_if condition str =
        if condition then str else ""
    in
    string_if (st.nb_variables >= 1) (Printf.sprintf "Nb. variables: %d\n" st.nb_variables)
    ^
    string_if (st.nb_constants >= 1) (Printf.sprintf "Nb. constants: %d\n" st.nb_constants)
    ^
    string_if (st.nb_selfs >= 1) (Printf.sprintf "Nb. selfs: %d\n" st.nb_selfs)
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
    string_if (st.nb_aliases >= 1) (Printf.sprintf "Nb. aliases: %d\n" st.nb_aliases)
    ^
    string_if (st.nb_alias_definitions >= 1)
        (Printf.sprintf "Nb. alias definitions: %d\n" st.nb_alias_definitions)
    ^
    string_if (st.nb_inclusions >= 1)
        (Printf.sprintf "Nb. inclusions: %d\n" st.nb_inclusions)

(* Returns the number of variables surveyed by the statistics st. *)
let nb_variables st =
    st.nb_variables

(* Returns the number of constants surveyed by the statistics st. *)
let nb_constants st =
    st.nb_constants

(* Returns the number of selfs surveyed by the statistics st. *)
let nb_selfs st =
    st.nb_selfs

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
    {
        nb_variables = 0;
        nb_constants = 0;
        nb_selfs = 0;
        nb_rules = 0;
        nb_applications = 0;
        nb_maps = 0;
        nb_shadows = 0;
        nb_aliases = 0;
        nb_alias_definitions = 0;
        nb_inclusions = 0
    }

(* Returns the statistics obtained by merging the statistics st1 and st2. *)
let merge st1 st2 =
    {
        nb_variables = st1.nb_variables + st2.nb_variables;
        nb_constants = st1.nb_constants + st2.nb_constants;
        nb_selfs = st1.nb_selfs + st2.nb_selfs;
        nb_rules = st1.nb_rules + st2.nb_rules;
        nb_applications = st1.nb_applications + st2.nb_applications;
        nb_maps = st1.nb_maps + st2.nb_maps;
        nb_shadows = st1.nb_shadows + st2.nb_shadows;
        nb_aliases = st1.nb_aliases + st2.nb_aliases;
        nb_alias_definitions = st1.nb_alias_definitions + st2.nb_alias_definitions;
        nb_inclusions = st1.nb_inclusions + st2.nb_inclusions
    }

(* Returns the statistics collected from the expression e. *)
let compute e =
    let rec aux e =
        match e with
            |E.Variable _ -> {empty with nb_variables = 1}
            |E.Constant (_, _, rules) ->
                let st =
                    rules
                    |> List.map (fun (e_lst, e1) -> aux e1 :: (e_lst |> List.map aux))
                    |> List.flatten
                    |> List.fold_left merge empty
                in
                {st with
                    nb_constants = 1 + st.nb_constants;
                    nb_rules = List.length rules + st.nb_rules}
            |E.Self _ -> {empty with nb_selfs = 1}
            |E.Application (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_applications = 1 + st.nb_applications}
            |E.Map (_, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_maps = 1 + st.nb_maps}
            |E.Shadow (_, e1, sh) ->
                let st = merge (aux e1) (aux sh) in
                {st with nb_shadows = 1 + st.nb_shadows}
            |E.Alias _ -> {empty with nb_aliases = 1}
            |E.AliasDefinition (_, _, e1, e2) ->
                let st = merge (aux e1) (aux e2) in
                {st with nb_alias_definitions = 1 + st.nb_alias_definitions}
            |E.Put _ -> {empty with nb_inclusions = 1}
    in
    aux e

