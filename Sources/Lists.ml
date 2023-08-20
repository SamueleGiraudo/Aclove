(* Author: Samuele Giraudo
 * Creation: (may 2020), jul. 2023
 * Modifications: jul. 2023
 *)

(* Returns the list obtained by considering each pair (x, x') element of the list lst and
 * sending it to op (f x) (f x') where op is a binary map and f is a unary map. *)
let map_pairs_operation f op lst =
    lst |> List.map (fun (x, x') -> op (f x) (f x'))

(* Returns the list of the pairs obtained by mapping each component of the pairs of the list
 * lst by the map f. *)
let map_pairs f lst =
    map_pairs_operation f (fun x x' -> (x, x')) lst

(* Returns the prefix of length n of the list lst. *)
let rec prefix lst n =
    match lst, n with
        |_, n when n <= 0 -> []
        |[], _ -> []
        |x :: lst', n -> x :: (prefix lst' (n - 1))

(* Returns the list of the pairs (a, b) such that a appears before b in the list lst. *)
let rec triangle_product lst =
    match lst with
        |[] -> []
        |x :: lst' -> (lst' |> List.map (fun y -> (x, y))) @ (triangle_product lst')

(* Given the association list lst such that an element can have several images, returns the
 * list of the images of x. *)
let images_assoc x lst =
    lst |> List.filter (fun (x', _) -> x' = x) |> List.map snd

(* Returns the association list obtained by merging the two association lists al1 and al2
 * by the binary operation op. These association lists must be sorted w.r.t. their keys. *)
let rec merge_assoc op al1 al2 =
    match al1, al2 with
        |[], al |al, [] -> al
        |((x1, y1) as c1) :: al1', ((x2, y2) as c2) :: al2' ->
            if x1 < x2 then
                c1 :: merge_assoc op al1' al2
            else if x1 > x2 then
                c2 :: merge_assoc op al1 al2'
            else
                (x1, op y1 y2) :: merge_assoc op al1' al2'

