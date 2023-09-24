(* Author: Samuele Giraudo
 * Creation: (mar. 2019, mar. 2020), feb. 2021
 * Modifications: (mar. 2020, may 2020), feb. 2021, dec. 2021, feb. 2022, may 2022,
 * oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun. 2023, jul. 2023, aug. 2023, sep. 2023
 *)

(* A type to specifies how to print an expression. *)
type print_options = {
    verbose: bool;
    with_rules: bool;
    with_shadows: bool;
    with_full_names: bool
}

(* Returns the print option with the specified attributes. *)
let build_print_options verbose with_rules with_shadows with_full_names =
    {verbose = verbose;
    with_rules = with_rules;
    with_shadows = with_shadows;
    with_full_names = with_full_names}

(* Prints the string str as an error. *)
let print_error str =
    str |> Strings.csprintf Strings.Red |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_1 str =
    str |> Strings.csprintf Strings.Blue |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_2 str =
    str |> Strings.csprintf Strings.Magenta |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_3 str =
    str |> Strings.csprintf Strings.Yellow |> print_string;
    flush stdout

(* Prints the string str as a success. *)
let print_success str =
    str |> Strings.csprintf Strings.Green |> print_string;
    flush stdout

(* Returns the expression obtained by cleaning each constant name of the constants of e. *)
let clean_constant_names e =
    let rec aux e =
        match e with
            |Expressions.Variable (info, v) ->
                Expressions.Variable (info, Variables.keep_suffix v)
            |Expressions.Self _ |Expressions.Put _ |Expressions.Alias _ ->
                e
            |Expressions.Constant (info, c, rules) ->
                let c' = Constants.keep_suffix c in
                Expressions.Constant (info, c', rules |> Lists.map_pairs aux)
            |Expressions.Application (info, e1, e2) ->
                Expressions.Application (info, aux e1, aux e2)
            |Expressions.Map (info, e1, e2) -> Expressions.Map (info, aux e1, aux e2)
            |Expressions.Shadow (info, e1, sh) -> Expressions.Shadow (info, aux e1, aux sh)
            |Expressions.Shift (info, lvl, e1) -> Expressions.Shift (info, lvl, aux e1)
            |Expressions.AliasDefinition (info, alias, e1, e2) ->
                Expressions.AliasDefinition (info, alias, aux e1, aux e2)
    in
    aux e

(* Returns a buffer containing the string representation of the expression e. *)
let to_buffered_string e =
    let buffer = Buffer.create 1024 in
    let rec aux lvl_offset e =
        match e with
            |Expressions.Variable (_, v) ->
                let v' = Variables.shift_level (Levels.opposite lvl_offset) v in
                Variables.to_string v'
                |> Strings.csprintf Strings.Green
                |> Buffer.add_string buffer
            |Expressions.Constant (_, c, rules) ->
                let c' = Constants.shift_level (Levels.opposite lvl_offset) c in
                Constants.to_string c'
                |> Strings.csprintf Strings.Yellow
                |> Buffer.add_string buffer;
                if rules <> [] then begin
                    " [" |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                    let semicolon = "; " |> Strings.csprintf Strings.Blue in
                    rules |> List.iter (fun (e1, e2) ->
                        aux (Levels.add lvl_offset (Constants.level c')) e1;
                        " -> " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                        aux (Levels.add lvl_offset (Constants.level c')) e2;
                        semicolon |> Buffer.add_string buffer);
                    Buffer.truncate buffer (Buffer.length buffer - String.length semicolon);
                    "]" |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer
                end
            |Expressions.Self (_, s) ->
                let s' = Selfs.shift_level (Levels.opposite lvl_offset) s in
                Selfs.to_string s'
                |> Strings.csprintf Strings.Cyan |> Buffer.add_string buffer
            |Expressions.Application (_, e1, e2) ->
                "(" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer;
                aux lvl_offset e1;
                " " |> Buffer.add_string buffer;
                aux lvl_offset e2;
                ")" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer
            |Expressions.Map (_, e1, e2) ->
                "(" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer;
                aux lvl_offset e1;
                " ^ " |> Strings.csprintf Strings.Red |> Buffer.add_string buffer;
                aux lvl_offset e2;
                ")" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer
            |Expressions.Shadow (_, e1, sh) ->
                "(" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer;
                aux lvl_offset e1;
                ")" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer;
                " : " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                "(" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer;
                aux lvl_offset sh;
                ")" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer
            |Expressions.Shift (_, lvl, e1) ->
                Levels.to_string lvl |> Buffer.add_string buffer;
                "(" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer;
                aux (Levels.add lvl_offset (Levels.opposite lvl)) e1;
                ")" |> Strings.csprintf Strings.Magenta |> Buffer.add_string buffer
            |Expressions.Alias (_, alias) -> alias |> Buffer.add_string buffer
            |Expressions.AliasDefinition (_, alias, e1, e2) ->
                "let " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                alias |> Buffer.add_string buffer;
                " = " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                aux lvl_offset e1;
                "in " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                aux lvl_offset e2;
            |Expressions.Put (_, path) ->
                "put " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                path |> Buffer.add_string buffer
    in
    aux (Levels.Level 0) e;
    buffer

(* Prints the expression e following the print options po. *)
let print po e =
    let e1 = if po.with_rules then e else Properties.remove_rules e in
    let e2 = if po.with_shadows then e1 else Shadows.remove_shadows e1 in
    let e3 = if po.with_full_names then e2 else clean_constant_names e2 in
    if po.verbose then begin
        "## Characteristics:\n" |> Strings.indent 4 |> print_information_2;
        Statistics.compute e
        |> Statistics.to_string
        |> Strings.indent 8
        |> print_string;
        "## Expression:\n" |> Strings.indent 4 |> print_information_2
    end;
    "" |> Strings.indent 8 |> print_string;
    Buffer.output_buffer stdout (to_buffered_string e3);
    print_newline ()













(******************************************************************************************)


(* Returns a buffer containing the compact string representation of the expression e. This
 * expression has to be simple and external self free. *)
let to_buffered_compact_string e =
    assert (Properties.is_simple e);
    assert (Properties.is_external_self_free e);
    let buffer = Buffer.create 16 in
    let rec aux e =
        match e with
            |Expressions.Variable (_, v) ->
                Variables.to_string v |> Buffer.add_string buffer
            |Expressions.Constant (_, c, _) ->
                Constants.to_string c |> Buffer.add_string buffer
            |Expressions.Application (_, e1, e2) ->
                aux e1;
                if Buffer.nth buffer (Buffer.length buffer - 1) = ')' then begin
                    Buffer.truncate buffer (Buffer.length buffer - 1);
                    Buffer.add_string buffer ", ";
                    aux e2;
                    Buffer.add_string buffer ")"
                end
                else begin
                    Buffer.add_string buffer "(";
                    aux e2;
                    Buffer.add_string buffer ")"
                end
            |Expressions.Map (_, e1, e2) -> begin
                let j = Buffer.length buffer in
                aux e2;
                let str = Buffer.contents buffer in
                match String.index_from_opt str j '[' with
                    |Some i ->
                        Buffer.truncate buffer (i + 1);
                        aux e1;
                        Buffer.add_string buffer ", ";
                        String.sub str (i + 1) (String.length str - i - 1)
                        |> Buffer.add_string buffer
                    |None ->
                        Buffer.add_string buffer "[";
                        aux e1;
                        Buffer.add_string buffer "]"
            end
            |Expressions.Shadow (_, e1, sh) ->
                aux e1;
                " : " |> Strings.csprintf Strings.Blue |> Buffer.add_string buffer;
                aux sh
            |_ -> Expressions.ValueError (e, "Outputs.to_buffered_compact_string") |> raise
    in
    aux e;
    buffer

let is_bool e =
    assert (Properties.is_simple e);
    match e with
        |Expressions.Constant (_, Constants.Constant (_, "false"), _)
        |Expressions.Constant (_, Constants.Constant (_, "true"), _) ->
            true
        |_ -> false

let rec is_nat e =
    assert (Properties.is_simple e);
    match e with
        |Expressions.Constant (_, Constants.Constant (_, "zero"), _) -> true
        |Expressions.Application (
            _,
            Expressions.Constant (_, Constants.Constant (_, "succ"), _),
            e') ->
            is_nat e'
        |_ -> false
(*
let is_int_term t =
    match t with
        |Term.Constant ("int.pos", [t']) |Term.Constant ("int.neg", [t']) -> is_bits_term t'
        |_ -> false
*)
let is_pair e =
    assert (Properties.is_simple e);
    match e with
        |Expressions.Application (
            _,
            Expressions.Application (
                _, Expressions.Constant (_, Constants.Constant (_, "pair"), _), _),
            _) ->
                true
        |_ -> false

let rec is_list e =
    assert (Properties.is_simple e);
    match e with
        |Expressions.Constant (_, Constants.Constant (_, "empty"), _) -> true
        |Expressions.Application (
            _, Expressions.Application (
                _, Expressions.Constant (_, Constants.Constant (_, "cell"), _), _), e') ->
            is_list e'
        |_ -> false

let is_bit e =
    assert (Properties.is_simple e);
    match e with
        |Expressions.Constant (_, Constants.Constant (_, "bit_0"), _)
        |Expressions.Constant (_, Constants.Constant (_, "bit_1"), _) -> true
        |_ -> false
(*
let rec is_binary_tree_term t =
    match t with
        |Term.Constant ("binary_tree.leaf", []) -> true
        |Term.Constant ("binary_tree.node", [_; t; t']) ->
            is_binary_tree_term t && is_binary_tree_term t'
        |_ -> false
*)

let rec to_pretty_string e =
    assert (Properties.is_simple e);
    let e = clean_constant_names e in
    if is_bool e then
        string_of_bool e
    (*
    else if is_int_term t then
        string_of_int_term t
    *)
    else if is_nat e then
        string_of_nat e
    else if is_pair e then
        string_of_pair e
    else if is_list e then
        string_of_list e
    else if is_bit e then
        string_of_bit e
    (*
    else if is_binary_tree_term t then
        string_of_binary_tree_term t
    *)
    else
        e |> to_buffered_string |> Buffer.contents

and string_of_bool e =
    assert (is_bool e);
    match e with
        |Expressions.Constant (_, Constants.Constant (_, "true"), _) -> "T"
        |Expressions.Constant (_, Constants.Constant (_, "false"), _) -> "F"
        |_ -> Expressions.ValueError (e, "Outputs.string_of_bool") |> raise
and string_of_nat e =
    assert (is_nat e);
    let rec aux e =
        match e with
            |Expressions.Constant (_, Constants.Constant (_, "zero"), _) -> 0
            |Expressions.Application (
                _,
                Expressions.Constant (_, Constants.Constant (_, "succ"), _), e') ->
                1 + aux e'
            |_ -> Expressions.ValueError (e, "Outputs.string_of_nat") |> raise
    in
    string_of_int (aux e)
(*
and string_of_int_term t =
    assert (is_int_term t);
    let rec abs_value t =
        match t with
            |Term.Constant ("bits.empty", []) -> Z.zero
            |Term.Constant ("bits.bit_0", [t']) -> Z.mul (abs_value t') (Z.of_int 2)
            |Term.Constant ("bits.bit_1", [t']) ->
                Z.succ (Z.mul (abs_value t') (Z.of_int 2))
            |_ -> Z.zero
    in
    match t with
        |Term.Constant ("int.pos", [t']) -> Z.to_string (abs_value t')
        |Term.Constant ("int.neg", [t']) -> Z.to_string (Z.sub Z.zero (abs_value t'))
        |_ -> Z.to_string Z.zero
*)
and string_of_pair e =
    assert (is_pair e);
    match e with
        |Expressions.Application (
            _, Expressions.Application (
                _, Expressions.Constant (_, Constants.Constant (_, "pair"), _), e1), e2) ->
            Printf.sprintf "<%s | %s>" (to_pretty_string e1) (to_pretty_string e2)
        |_ -> Expressions.ValueError (e, "Outputs.string_of_pair") |> raise
and string_of_list e =
    assert (is_list e);
    let rec aux e =
        match e with
            |Expressions.Constant (_, Constants.Constant (_, "empty"), _) -> ""
            |Expressions.Application (
                _,
                Expressions.Application (
                    _,
                    Expressions.Constant (_, Constants.Constant (_, "cell"), _), e1),
                e2) ->
                let str_e1 = to_pretty_string e1 and str_e2 = aux e2 in
                if str_e2 = "" then str_e1 else str_e1 ^ ", " ^ str_e2
            |_ -> Expressions.ValueError (e, "Outputs.string_of_list") |> raise
    in
    "(" ^ (aux e) ^ ")"
and string_of_bit e =
    assert (is_bit e);
    match e with
        |Expressions.Constant (_, Constants.Constant (_, "bit_0"), _) -> "0"
        |Expressions.Constant (_, Constants.Constant (_, "bit_1"), _) -> "1"
        |_ -> Expressions.ValueError (e, "Outputs.string_of_bit") |> raise
(*
and string_of_binary_tree_term t =
    assert (is_binary_tree_term t);
    let rec aux t =
        match t with
            |Term.Constant ("binary_tree.leaf", []) -> "*"
            |Term.Constant ("binary_tree.node", [e; t; t']) ->
                Printf.sprintf "%s(%s, %s)" (to_string e) (aux t) (aux t')
            |_ -> ""
    in
    aux t
*)

