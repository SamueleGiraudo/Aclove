(* Author: Samuele Giraudo
 * Creation: (jan. 2021), may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022,
 * apr. 2023, jun. 2023, jul. 2023, dec. 2023, jan. 2024
 *)

module C = Constants
module E = Expressions
module L = Lexer
module P = Paths
module Pa = Parser

(* The extension of Aclove files. *)
let extension = ".acl"

(* The maximal length for a variable. *)
let max_variable_length = 1024

(* The maximal length for a constant. *)
let max_constant_length = 1024

(* The maximal length for an alias (in definitions or in uses). *)
let max_alias_length = 1024

(* The maximal length for a path. *)
let max_path_length = 1024

(* Tests if the character c is an alphabetic character. *)
let is_alpha_character c =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

(* Tests if the character c is a numerical character. *)
let is_numerical_character c =
    ('0' <= c && c <= '9')

(* Tests if the character c is a special character. *)
let is_special_character c =
     c = '_' || c = '-' || c = '.' || c = '/'

(* Tests if the character c is a character allowed in identifiers. *)
let is_plain_character c =
     is_alpha_character c || is_numerical_character c || is_special_character c

(* Tests if the string str is plain, that is, if it has length 1 or more and it is made of
 * plain characters. *)
let is_plain_string str =
    String.length str >= 1 && str |> String.for_all is_plain_character

(* Tests if the string str can be a variable. *)
let is_variable_name str =
    let len = String.length str in
    1 <= len && len <= max_variable_length && is_plain_string str

(* Tests if the string str can be a constant. *)
let is_constant_name str =
    let len = String.length str in
    1 <= len && len <= max_constant_length && is_plain_string str

(* Tests if the string str can be an alias. *)
let is_alias str =
    let len = String.length str in
    1 <= len && len <= max_alias_length && is_plain_string str

(* Tests if the string str can be a path of an included file. *)
let is_inclusion_path str =
    let len = String.length str in
    (1 + String.length extension) <= len && len <= max_path_length
    && P.has_extension extension str && str |> String.for_all is_plain_character

(* Returns the expression e obtained by adding to all its inclusion paths the prefix
 * pref. *)
let complete_inclusion_paths pref e =
    let rec aux e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ -> e
            |E.Constant (info, c, rules) ->
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c, rules')
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
            |E.Put (info, path) -> E.Put (info, pref ^ path)
    in
    aux e

(* Returns the expression e obtained by adding to all its constant names the prefix pref. *)
let complete_constant_names pref e =
    let rec aux e =
        match e with
            |E.Variable _ -> e
            |E.Constant (info, c, rules) ->
                let c' = C.Constant (pref ^ C.name c) in
                let rules' =
                    rules |> List.map (fun (e_lst, e1) -> (e_lst |> List.map aux, aux e1))
                in
                E.Constant (info, c', rules')
            |E.Self _ |E.Alias _ |E.Put _ -> e
            |E.Application (info, e1, e2) -> E.Application (info, aux e1, aux e2)
            |E.Map (info, e1, e2) -> E.Map (info, aux e1, aux e2)
            |E.Shadow (info, e1, sh) -> E.Shadow (info, aux e1, aux sh)
            |E.AliasDefinition (info, alias, e1, e2) ->
                E.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* This cache is used to optimize the function path_to_expression. *)
let cache_path_to_expression = Hashtbl.create 1024

(* Returns the expression specified by the Aclove file at path path. This path must be
 * canonical. The exception Lexer.Error is raised when there are syntax errors in the
 * program. *)
let path_to_expression path =
    assert (Sys.file_exists path);
    assert (path = P.canonicalize path);
    match Hashtbl.find_opt cache_path_to_expression path with
        |Some e -> e
        |None ->
            let e =
                L.value_from_file_path path Pa.expression L.read
                |> complete_inclusion_paths (P.trim path)
                |> complete_constant_names (P.remove_extension path ^ "/")
            in
            Hashtbl.add cache_path_to_expression path e;
            e

(* Returns the list of the included paths in the expression e and recursively included by
 * the expressions of the included Aclove files. *)
let included_paths e =
    let rec aux paths e =
        match e with
            |E.Variable _ |E.Self _ |E.Alias _ -> []
            |E.Constant (_, _, rules) ->
                rules
                |> List.map
                    (fun (e_lst, e1) ->
                        aux paths e1 :: (e_lst |> List.map (aux paths)) |> List.flatten)
                |> List.flatten
            |E.Application (_, e1, e2) |E.Map (_, e1, e2) |E.Shadow (_, e1, e2)
            |E.AliasDefinition (_, _, e1, e2) -> aux paths e1 @ aux paths e2
            |E.Put (_, path) ->
                let path = P.add_extension extension path in
                if not (Sys.file_exists path) || not (is_inclusion_path path) then
                    []
                else
                    let path' = P.canonicalize path in
                    if List.mem path' paths then
                        [path']
                    else
                        try
                            let e0 = path_to_expression path' in
                            let paths' = path' :: paths in
                            path' :: aux paths' e0
                        with
                            |L.Error _ -> []
    in
    aux [] e

