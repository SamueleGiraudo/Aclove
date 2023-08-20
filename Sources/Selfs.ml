(* Author: Samuele Giraudo
 * Creation: jun. 2023
 * Modifications: jun. 2023, jul. 2023
 *)

(* A type for selfs. A self is specified by its level. *)
type selfs = Self of Levels.levels

(* Returns the level of the self s. *)
let level s =
    let Self level = s in
    level

(* Returns the self obtained by shifting the self s by the level lvl. *)
let shift_level lvl s =
    Self (Levels.add lvl (level s))

(* Returns a string representation of the self s. *)
let to_string s =
    Levels.to_string (level s) ^ "@"

