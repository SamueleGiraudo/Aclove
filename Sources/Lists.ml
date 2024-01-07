(* Author: Samuele Giraudo
 * Creation: (may 2020), jul. 2023
 * Modifications: jul. 2023, dec. 2023, jan. 2024
 *)

(* Returns the list of the pairs (a, b) such that a appears before b in the list lst. *)
let rec triangle_product lst =
    match lst with
        |[] -> []
        |x :: lst' -> (lst' |> List.map (fun y -> (x, y))) @ (triangle_product lst')

(* Given the association list lst such that an element can have several images, returns the
 * list of the images of x. *)
let images_assoc x lst =
    lst |> List.filter (fun (x', _) -> x' = x) |> List.map snd

