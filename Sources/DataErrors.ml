(* Author: Samuele Giraudo
 * Creation: jan. 2024
 * Modifications: jan. 2024
 *)

module E = Errors

(* A type to encapsulate some data or to handle a list of errors. This is not forced by the
 * type definition but the encapsulated data will be a substitution or an expression. *)
type 'data data_errors =
    |Data of 'data
    |Errors of (E.errors list)

(* Tests if the data errors de has errors. *)
let has_errors de =
    match de with
        |Data _ -> false
        |Errors _ -> true

(* Returns an option to the data of the data errors de. *)
let data de =
    match de with
        |Data de -> Some de
        |Errors _ -> None

(* Returns the list of errors of the data errors de. *)
let errors de =
    match de with
        |Data _ -> []
        |Errors errs -> errs

(* Tests if the predicate f is satisfied for the data of the data errors de. When de has
 * errors, this returns true. *)
let test_data f de =
    match de with
        |Data de -> f de
        |Errors _ -> true

(* Returns the data errors which is the data obtained by considering the image by f of the
 * data of the data errors de. When de has errors, this returns de. *)
let map_data f de =
    match de with
        |Data d -> Data (f d)
        |Errors _ -> de

(* Returns the data errors which is the image by f of the data of the data errors de. When
 * de has errors, this returns de. *)
let bind_data f de =
    match de with
        |Data d -> f d
        |Errors errs -> Errors errs

(* Returns the data errors which is the errors returned by the map f applied on the data of
 * the data errors de if these errors does not form an empty list. Otherwise, or when de has
 * errors, de is returned. *)
let check_errors f de =
    match de with
        |Data d ->
            let errs = f d in
            if errs = [] then de else Errors errs
        |Errors _ -> de

(* Returns the data errors which is the image by f of the data of the data errors de_1 and
* de_2. When exactly one among de_1 or de_2 has errors, this returns this data error having
* an error. When both de_1 and de_2 have errors, this returns the errors having as error
* list the concatenation of the list of errors of de_1 and de_2. *)
let bind_data_2 f de_1 de_2 =
    match de_1, de_2 with
        |Data d1, Data d2 -> f d1 d2
        |Errors errs, Data _ |Data _, Errors errs -> Errors errs
        |Errors errs_1, Errors errs_2 -> Errors (errs_1 @ errs_2)

