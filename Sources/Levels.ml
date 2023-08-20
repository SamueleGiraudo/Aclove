(* Author: Samuele Giraudo
 * Creation: jun. 2023
 * Modifications: jun. 2023, jul. 2023
 *)

(* A type for levels. *)
type levels = Level of int

(* Returns the value of the level lvl. *)
let value lvl =
    let Level value = lvl in
    value

(* Returns the level obtained by shifting by k the value of the level lvl. *)
let shift k lvl =
    Level (k + value lvl)

(* Returns the level obtained by adding the levels lvl and lvl'. *)
let add lvl lvl' =
    Level (value lvl + value lvl')

(* Returns the level admitting as value the opposite of the one of the level lvl. *)
let opposite lvl =
    Level (- (value lvl))

(* Returns a string representation of the level lvl. *)
let to_string lvl =
    let k = value lvl in
    if k >= 0 then String.make k '+' else String.make (-k) '-'

