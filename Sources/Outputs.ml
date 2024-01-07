(* Author: Samuele Giraudo
 * Creation: (mar. 2019, mar. 2020), feb. 2021
 * Modifications: (mar. 2020, may 2020), feb. 2021, dec. 2021, feb. 2022, may 2022,
 * oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun. 2023, jul. 2023, aug. 2023, sep. 2023,
 * dec. 2023, jan. 2024
 *)

module C = Constants
module E = Expressions
module St = Strings
module V = Variables

(* Prints the string str as an error. *)
let print_error str =
    "?? " ^ str |> St.csprintf St.Red |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_1 str =
    ">> " ^ str |> St.csprintf St.Blue |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_2 str =
    ">> " ^ str |> St.csprintf St.Magenta |> print_string;
    flush stdout

(* Prints the string str as an information. *)
let print_information_3 str =
    ">> " ^ str |> St.csprintf St.Yellow |> print_string;
    flush stdout

(* Prints the string str as a success. *)
let print_success str =
    "!! " ^ str |> St.csprintf St.Green |> print_string;
    flush stdout

(* Tests of the expression e is atomic. This is the case when e is a variable, a constant,
 * a self, an alias, or a put expression. *)
let is_atomic e =
    match e with
        |E.Variable _ |E.Constant _ |E.Self _ |E.Alias _ |E.Put _ -> true
        |E.Application _ |E.Map _ |E.Shadow _ |E.AliasDefinition _ -> false

(* Returns a buffer containing the string representation of the expression e. *)
let to_buffered_string e =
    let buffer = Buffer.create 1024 in
    let space () = " " |> Buffer.add_string buffer in
    let l_brack () = " [" |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let r_brack () = "]" |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let l_par () = "(" |> St.csprintf St.Magenta |> Buffer.add_string buffer in
    let r_par () = ")" |> St.csprintf St.Magenta |> Buffer.add_string buffer in
    let pipe () = " | " |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let sharp () = "# " |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let at () = "@" |> St.csprintf St.Cyan |> Buffer.add_string buffer in
    let circ () = " ^ " |> St.csprintf St.Red |> Buffer.add_string buffer in
    let colon () = " : " |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let equal () = " = " |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let dot () = " . " |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let bang  ()= "!" |> St.csprintf St.Blue |> Buffer.add_string buffer in
    let rec aux e =
        match e with
            |E.Variable (_, v) ->
                V.to_string v |> St.csprintf St.Green |> Buffer.add_string buffer
            |E.Constant (_, c, rules) ->
                C.to_string c |> St.csprintf St.Yellow |> Buffer.add_string buffer;
                if rules <> [] then begin
                    l_brack ();
                    rules
                    |> List.iter
                        (fun (e_lst, e1) ->
                            pipe ();
                            e_lst
                            |> List.iter
                                (fun e ->
                                    if not (is_atomic e) then l_par ();
                                    aux e;
                                    if not (is_atomic e) then r_par ();
                                    space ());
                            sharp ();
                            aux e1);
                    r_brack ()
                end
            |E.Self _ -> at ()
            |E.Application (_, e1, e2) ->
                aux e1;
                space ();
                if not (is_atomic e2) then l_par ();
                aux e2;
                if not (is_atomic e2) then r_par ()
            |E.Map (_, e1, e2) ->
                if not (is_atomic e1) then l_par ();
                aux e1;
                if not (is_atomic e1) then r_par ();
                circ ();
                aux e2
            |E.Shadow (_, e1, sh) ->
                if not (is_atomic e1) then l_par ();
                aux e1;
                if not (is_atomic e1) then r_par ();
                colon ();
                aux sh
            |E.Alias (_, alias) -> alias |> Buffer.add_string buffer
            |E.AliasDefinition (_, alias, e1, e2) ->
                alias |> Buffer.add_string buffer;
                equal ();
                aux e1;
                dot ();
                aux e2
            |E.Put (_, path) ->
                bang ();
                path |> Buffer.add_string buffer
    in
    aux e;
    buffer

