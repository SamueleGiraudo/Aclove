(* Author: Samuele Giraudo
 * Creation: (jan. 2021), may 2022
 * Modifications: may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022,
 * apr. 2023, jun. 2023, jul. 2023
 *)

(* The extension of Aclove files. *)
let extension = ".acl"

(* The maximal length for a variable. *)
let max_variable_length = 256

(* The maximal length for a constant. *)
let max_constant_length = 256

(* The maximal length for an alias (in definitions or in uses). *)
let max_alias_length = 256

(* The maximal length for a path. *)
let max_path_length = 256

(* Returns the path obtained from the path path by adding the Aclove file extension. *)
let add_file_extension path =
    path ^ extension

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
    if len > max_path_length then
        false
    else
        let items = String.split_on_char '/' str in
        if List.mem "" items then
            false
        else
            let levels = items |> List.fold_left
                (fun res u ->
                    let v = if u = ".." then (List.hd res) - 1 else (List.hd res) + 1 in
                    v :: res)
                [0]
            in
            if levels |> List.exists (fun v -> v < 0) then
                false
            else
                len >= (1 + String.length extension)
                    && Paths.has_extension extension str
                    && str |> String.for_all is_plain_character

(* Returns the expression e obtained by adding to all its inclusion paths the prefix
 * pref. *)
let complete_inclusion_paths pref e =
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
            |Expressions.Put (info, path) -> Expressions.Put (info, pref ^ path)
    in
    aux e

(* Returns the expression e obtained by adding to all its variable and constant names the
 * prefix pref. *)
let complete_names pref e =
    let rec aux e =
        match e with
            |Expressions.Variable (info, v) ->
                let v' = Variables.Variable (Variables.level v, pref ^ Variables.name v) in
                Expressions.Variable (info, v')
            |Expressions.Constant (info, c, rules) ->
                let c' = Constants.Constant (Constants.level c, pref ^ Constants.name c) in
                Expressions.Constant (info, c', rules |> Lists.map_pairs aux)
            |Expressions.Self _ |Expressions.Alias _ |Expressions.Put _ -> e
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, aux sh)
            |Expressions.Shift (info, lvl, e1) -> Expressions.Shift (info, lvl, aux e1)
            |Expressions.AliasDefinition (info, alias, e1, e2) ->
                Expressions.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns the expression specified by the Aclove file at path path. The exception
 * Lexer.Error is raised when there are syntax errors in the program. *)
let path_to_expression path =
    let e = Lexer.value_from_file_path path Parser.expression Lexer.read in
    let path' = Paths.simplify path in
    e
    |> complete_inclusion_paths (Paths.trim path')
    |> complete_names (Paths.remove_extension path' ^ "/")

(* Returns the list of the included paths in the expression e and recursively included by
 * the expressions of the included Aclove files. *)
let included_paths e =
    let rec aux paths e =
        match e with
            |Expressions.Variable _ |Expressions.Self _ |Expressions.Alias _ -> []
            |Expressions.Constant (_, _, rules) ->
                rules |> Lists.map_pairs_operation (aux paths) (@) |> List.flatten
            |Expressions.Application (_, e1, e2) |Expressions.Map (_, e1, e2)
            |Expressions.Shadow (_, e1, e2)
            |Expressions.AliasDefinition (_, _, e1, e2) ->
                aux paths e1 @ aux paths e2
            |Expressions.Shift (_, _, e1) -> aux paths e1
            |Expressions.Put (_, path) ->
                let path = add_file_extension path |> Paths.simplify in
                if not (Sys.file_exists path) || not (is_inclusion_path path) then
                    []
                else
                    let path' = Paths.simplify path in
                    if List.mem path' paths then
                        [path']
                    else
                        try
                            let e0 = path_to_expression path' in
                            let paths' = path' :: paths in
                            path' :: aux paths' e0
                        with
                            |Lexer.Error _ -> []
    in
    aux [] e

