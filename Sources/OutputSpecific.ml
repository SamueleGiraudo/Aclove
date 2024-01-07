(******************************************************************************************)
(* THIS IS NOT PART OF ACLOVE SOURCES *)

(* Conversion to strings of particular expressions. To use in a system interpretor. *)


(* Returns a buffer containing the compact string representation of the expression e. This
 * expression has to be simple and external self free. *)
let to_buffered_compact_string e =
    assert (P.is_simple e);
    assert (P.is_external_self_free e);
    let buffer = Buffer.create 16 in
    let rec aux e =
        match e with
            |E.Variable (_, v) ->
                V.to_string v |> Buffer.add_string buffer
            |E.Constant (_, c, _) ->
                C.to_string c |> Buffer.add_string buffer
            |E.Application (_, e1, e2) ->
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
            |E.Map (_, e1, e2) -> begin
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
            |E.Shadow (_, e1, sh) ->
                aux e1;
                " : " |> St.csprintf St.Blue |> Buffer.add_string buffer;
                aux sh
            |_ -> E.ValueError (e, "Outputs.to_buffered_compact_string") |> raise
    in
    aux e;
    buffer

let is_bool e =
    assert (P.is_simple e);
    match e with
        |E.Constant (_, C.Constant (_, "false"), _)
        |E.Constant (_, C.Constant (_, "true"), _) ->
            true
        |_ -> false

let rec is_nat e =
    assert (P.is_simple e);
    match e with
        |E.Constant (_, C.Constant (_, "zero"), _) -> true
        |E.Application (
            _,
            E.Constant (_, C.Constant (_, "succ"), _),
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
    assert (P.is_simple e);
    match e with
        |E.Application (
            _,
            E.Application (
                _, E.Constant (_, C.Constant (_, "pair"), _), _),
            _) ->
                true
        |_ -> false

let rec is_list e =
    assert (P.is_simple e);
    match e with
        |E.Constant (_, C.Constant (_, "empty"), _) -> true
        |E.Application (
            _, E.Application (
                _, E.Constant (_, C.Constant (_, "cell"), _), _), e') ->
            is_list e'
        |_ -> false

let is_bit e =
    assert (P.is_simple e);
    match e with
        |E.Constant (_, C.Constant (_, "bit_0"), _)
        |E.Constant (_, C.Constant (_, "bit_1"), _) -> true
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
    assert (P.is_simple e);
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
        |E.Constant (_, C.Constant (_, "true"), _) -> "T"
        |E.Constant (_, C.Constant (_, "false"), _) -> "F"
        |_ -> E.ValueError (e, "Outputs.string_of_bool") |> raise
and string_of_nat e =
    assert (is_nat e);
    let rec aux e =
        match e with
            |E.Constant (_, C.Constant (_, "zero"), _) -> 0
            |E.Application (
                _,
                E.Constant (_, C.Constant (_, "succ"), _), e') ->
                1 + aux e'
            |_ -> E.ValueError (e, "Outputs.string_of_nat") |> raise
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
        |E.Application (
            _, E.Application (
                _, E.Constant (_, C.Constant (_, "pair"), _), e1), e2) ->
            Printf.sprintf "<%s | %s>" (to_pretty_string e1) (to_pretty_string e2)
        |_ -> E.ValueError (e, "Outputs.string_of_pair") |> raise
and string_of_list e =
    assert (is_list e);
    let rec aux e =
        match e with
            |E.Constant (_, C.Constant (_, "empty"), _) -> ""
            |E.Application (
                _,
                E.Application (
                    _,
                    E.Constant (_, C.Constant (_, "cell"), _), e1),
                e2) ->
                let str_e1 = to_pretty_string e1 and str_e2 = aux e2 in
                if str_e2 = "" then str_e1 else str_e1 ^ ", " ^ str_e2
            |_ -> E.ValueError (e, "Outputs.string_of_list") |> raise
    in
    "(" ^ (aux e) ^ ")"
and string_of_bit e =
    assert (is_bit e);
    match e with
        |E.Constant (_, C.Constant (_, "bit_0"), _) -> "0"
        |E.Constant (_, C.Constant (_, "bit_1"), _) -> "1"
        |_ -> E.ValueError (e, "Outputs.string_of_bit") |> raise
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

