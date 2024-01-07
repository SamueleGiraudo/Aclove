(* Author: Samuele Giraudo

 * Creation: jun. 2023
 * Modifications: jun. 2023, jul. 2023, dec. 2023, jan. 2024
 *)

(* A type for constants. A constant is specified by its level and its name. *)
type constants = Constant of string

(* Returns the name of the constant c. *)
let name c =
    let Constant name = c in
    name

(* Returns the constant obtained from the constant c by keeping only the suffix of its
 * name. *)
let keep_suffix c =
    let str = String.split_on_char '/' (name c) |> List.rev |> List.hd in
    Constant str

(* Returns a string representation of the constant c. *)
let to_string c =
    Printf.sprintf "'%s" (name c)

