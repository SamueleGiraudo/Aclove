(* Author: Samuele Giraudo
 * Creation: (may 2020), jul. 2023
 * Modifications: jul. 2023
 *)

(* Returns def if the optional value opt is None. Otherwise, returns the value carried by
 * opt. *)
let value def opt =
    match opt with
        |Some x -> x
        |None -> def

(* Returns the image by the map f of the value of the optional value of opt if any. None
 * is returned otherwise. *)
let bind f opt =
    match opt with
        |None -> None
        |Some x -> f x

(* Returns the image by the map f of the value of the optional value of opt_1 and opt_2 if
* any. None is returned otherwise. *)
let bind_2 f opt_1 opt_2 =
    match opt_1, opt_2 with
        |None, _ |_, None -> None
        |Some x1, Some x2 -> f x1 x2

(* Returns the value obtained by applying iteratively the function f on x. This function f
 * returns an option on a value. When this value is None, x is returned. Otherwise, the
 * image by option_iter and f on the value of f x is returned. *)
let rec iter f x =
    match f x with
        |None -> x
        |Some x' -> iter f x'

