(* Author: Samuele Giraudo
 * Creation: jun. 2023
 * Modifications: jun. 2023, jul. 2023
 *)

(* A type for variables. A variable is specified by its level and its name. *)
type variables = Variable of (Levels.levels * string)

(* Returns the level of the variable v. *)
let level v =
    let Variable (level, _) = v in
    level

(* Returns the name of the variable v. *)
let name v =
    let Variable (_, name) = v in
    name

(* Returns the variable obtained by shifting the variable v by the level lvl. *)
let shift_level lvl v =
    Variable (Levels.add lvl (level v), name v)

(* Returns a string representation of the variable v. *)
let to_string v =
    Levels.to_string (level v) ^ Printf.sprintf "%%%s" (name v)

