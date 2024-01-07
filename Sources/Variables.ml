(* Author: Samuele Giraudo
 * Creation: jun. 2023
 * Modifications: jun. 2023, jul. 2023, sep. 2023, dec. 2023
 *)

(* A type for variables. A variable is specified by its level and its name. *)
type variables = Variable of string

(* Returns the name of the variable v. *)
let name v =
    let Variable name = v in
    name

(* Returns the variable obtained from the variable v by keeping only the suffix of its
 * name. *)
let keep_suffix v =
    let str = String.split_on_char '/' (name v) |> List.rev |> List.hd in
    Variable str

(* Returns a string representation of the variable v. *)
let to_string v =
    Printf.sprintf "%%%s" (name v)

