(* Author: Samuele Giraudo
 * Creation: may. 2022
 * Modifications: may. 2022, aug. 2022, oct. 2022, apr. 2022, jul. 2023
 *)

(* The type to add information for each expression (and its subexpressions). *)
type information = {
    (* The position with respect to the file where the subexpression appears. *)
    file_position: FilePositions.file_positions option;

    (* The index is a (unique) integer attributed to each subexpression. It is used in
    * type checking. *)
    index: int option
}

(* Returns the information specified by the file position file_position. The index of the
 * returned information is None. *)
let construct file_position =
    {file_position = Some file_position; index = None}

(* Returns the empty information. *)
let empty =
    {file_position = None; index = None}

(* Returns a string representation of the information info. *)
let to_string info =
    (match info.index with Some ind -> Printf.sprintf "I%d " ind |None -> "")
    ^
    (match info.file_position with Some fp -> FilePositions.to_string fp |None -> "")

(* Returns the index of the information info. This is an optional value. *)
let index info =
    info.index

(* Returns the information obtained by setting as index the index field of the information
 * info. *)
let set_index info index =
    {info with index = Some index}

