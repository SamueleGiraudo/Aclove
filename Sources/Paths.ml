(* Author: Samuele Giraudo
 * Creation: (may 2020), jul. 2023
 * Modifications: jul. 2023, dec. 2023, jan. 2024
 *)

(* Returns the extension of the file at path path. *)
let extension path =
    let i = String.rindex path '.' in
    String.sub path i (String.length path - i)

(* Tests if the file at path path has the extension ext (with the point). *)
let has_extension ext path =
    if String.contains path '.' then extension path = ext else false

(* Returns the path obtained from the path path by adding the extension extension. *)
let add_extension extension path =
    path ^ extension

(* Returns the string obtained from the path path by removing its file extension, including
 * the '.'. *)
let remove_extension path =
    assert (String.contains path '.');
    let i = String.rindex path '.' in
    String.sub path 0 i

(* Returns the path obtained by suppressing the last part of the path path, by keeping the
 * `/`. For instance, if path is "aa/ab/abc", then "aa/ab/" is returned. If path has no
 * occurrence of '/', the empty path is returned. *)
let trim path =
    try
        let i = String.rindex path '/' in
        String.sub path 0 (i + 1)
    with
        |Not_found -> ""

(* This cache is used to optimize the function canonicalize. *)
let cache_canonical_paths = Hashtbl.create 1024

(* Returns the absolute path specified by the possibly relative path path containing
 * possibly symbolic links. This path must be the path of an existing file. *)
let canonicalize path =
    assert (Sys.file_exists path);
    match Hashtbl.find_opt cache_canonical_paths path with
        |Some path' -> path'
        |None ->
            let ch = Unix.open_process_in ("readlink -e " ^ Filename.quote path) in
            let path' = input_line ch in
            Unix.close_process_in ch |> ignore;
            Hashtbl.add cache_canonical_paths path path';
            path'

