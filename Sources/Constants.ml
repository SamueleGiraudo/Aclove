(* Author: Samuele Giraudo
 * Creation: jun. 2023
 * Modifications: jun. 2023, jul. 2023
 *)

(* A type for constants. A constant is specified by its level and its name. *)
type constants = Constant of (Levels.levels * string)

(* Returns the level of the constant c. *)
let level c =
    let Constant (level, _) = c in
    level

(* Returns the name of the constant c. *)
let name c =
    let Constant (_, name) = c in
    name

(* Returns the constant obtained by shifting the constant c by the level lvl. *)
let shift_level lvl c =
    Constant (Levels.add lvl (level c), name c)

(* Returns the constant obtained from the constant c by keeping only the suffix of its
 * name. *)
let keep_suffix c =
    let str = String.split_on_char '/' (name c) |> List.rev |> List.hd in
    Constant (level c, str)

(* Returns a string representation of the constant c. *)
let to_string c =
    Levels.to_string (level c) ^ Printf.sprintf "'%s" (name c)

