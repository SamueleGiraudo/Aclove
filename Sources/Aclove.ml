(* Author: Samuele Giraudo
 * Creation: (may 2018), mar. 2020
 * Modifications: mar. 2020, apr. 2020, may 2020, jun. 2020, dec. 2020, jan. 2021,
 * feb. 2021, nov. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022, jun. 2022, aug. 2022,
 * sep. 2022, oct. 2022, nov. 2022, dec. 2022, jan. 2023, apr. 2023, jun. 2023, jul. 2023,
 * aug. 2023, sep. 2023
 *)

(* Aclove - A programming language based on a mix of combinatory logic and general term
 * rewrite systems. *)

(* TODO
 *
 * - Check if the management of paths is good. It seems that there are some problem related
 *   to the current paths and its simplifications, mainly when an included path starts with
 *   "..".
 *
 * - Consolidate names of variables and constant (possible collisions by using / in names.
 *
 * - Problems about the management of included files using symbolic links. Indeed, the
 *   prefix added to the constants is the traversed path. This can provoke shadow errors
 *   since some shadow constants which should be the same are considered as different when
 *   a same file is used several times through different paths using symbolic links. This
 *   problem should be resolved by transforming a path using links as a concrete path.
 *
 * - Optimize the printing of expressions by not printing some parenthesis. For instance,
 *   "'a 'b 'c" must be printed instead of "(('a 'b) 'c)".
 *
 * - Implement statistics about evaluation.
 *
 * - Implement statistics about typing.
 *
 * - Add all possible assertions everywhere. Remove the test for "simple" expressions.
 *   Instead, write assertions for atomic properties.
 *
 * - Consolidate the data structures (by adding names like type x = X of y instead of
 *   having simply type x = y).
 *
 * - IMPORTANT: Implement dependent types.
 *
 * - Clean the code and add modules.
 *
 * - Implement the environment model (instead of the substitution model).
 *
 * - Write an efficient script for compiling this project, not based on ocamlbuild.
 *
 * - Maybe write a lexer/parser by hand without using menhir and others.
 *
 * - Improve error messages (in particular for shadow errors.
 *
 *)

let name = "Aclove"

let logo = "/\\<<|_"

let description =
    "A programming language mixing combinatory logic and general term rewrite systems."

(*let version = "0.001"*)
(*let version = "0.010"*)
(*let version = "0.011" and version_date = "2020-06"*)
(*let version = "0.100" and version_date = "2021-02-21"*)
(*let version = "0.101" and version_date = "2021-12-01"*)
(*let version = "0.110" and version_date = "2022-02-26"*)
(*let version = "0.111" and version_date = "2022-05-30"*)
(*let version = "1.000" and version_date = "2022-09-05"*)
(*let version = "1.001" and version_date = "2022-10-06"*)
(*let version = "1.010" and version_date = "2022-10-12"*)
(*let version = "1.011" and version_date = "2022-12-25"*)
(*let version = "1.100" and version_date = "2023-01-01"*)
(*let version = "1.101" and version_date = "2023-04-19"*)
(*let version = "1.110" and version_date = "2023-07-02"*)
let version = "1.1110" and version_date = "2023-07-17"

let author = "Samuele Giraudo"

(*let email = "samuele.giraudo@univ-eiffel.fr"*)
let email = "giraudo.samuele@uqam.fr"

(* Returns a string of information about the Aclove program. *)
let information =
    Printf.sprintf "%s\n%s\n%s\nCopyright (C) 2018--2023 %s\nWritten by %s [%s]\n\
        Version: %s (%s)\n"
        logo name description author author email version version_date

(* Returns the help string about the arguments of the program. *)
let help_string =
      "Usage:\n    ./aclove [--help] [--version] --file PATH [--verbose] [--input OPT] \
       [--output OPT]\nwhere:\n"
    ^ "    + `--help` prints the short help (the present text).\n"
    ^ "    + `--version` prints the version and other information.\n"
    ^ "    + `--file PATH` sets PATH as the path to the Aclove program to consider, \
             contained in a " ^ Files.extension ^ " file.\n"
    ^ "    + `--verbose` enables the verbose mode.\n"
    ^ "    + The options OPT control the display mode of the input and output \
             expressions:\n"
    ^ "        + `no-rules` drops the rules of the expression.\n"
    ^ "        + `no-shadows` drops the shadows of the expression.\n"
    ^ "        + `short-names` keeps only the last part of the variable and constant names \
                  of the expression.\n"

;;

(* Main expression. *)

(* Version. *)
if Arguments.exists "--version" then begin
    Outputs.print_success information;
    exit 0
end;

(* Help. *)
if Arguments.exists "--help" then begin
    Outputs.print_information_1 help_string;
    exit 0
end;

(* Test if there is a single file path. *)
let arg_lst = Arguments.option_values "--file" in
if List.length arg_lst <> 1 then begin
    "Error: one path must follow the --file argument.\n" |> Outputs.print_error;
    exit 1
end;

(* The path of the file containing the program. *)
let path = List.hd arg_lst in

(* Checks the existence of the file at path path. *)
if Sys.file_exists path |> not then begin
    Printf.sprintf "Error: there is no file %s.\n" path |> Outputs.print_error;
    exit 1
end;

(* Checks if the file has the right extension. *)
if not (Paths.has_extension Files.extension path) then begin
    Printf.sprintf "Error: the file %s has not %s as extension.\n" path Files.extension
    |> Outputs.print_error;
    exit 1
end;

(* Detection of the verbose mode. *)
let verbose = Arguments.exists "--verbose" in

(* Reads and processes the given Aclove program. *)
if verbose then Outputs.print_information_1 "# Computing... ";
let clock_start = Unix.gettimeofday () in
let pr = Processings.process_path path in
let clock_end = Unix.gettimeofday () in
if verbose then begin
    Outputs.print_success "Done ";
    Printf.sprintf "[duration: %.2f s].\n" (clock_end -. clock_start)
    |> Outputs.print_information_3
end;

(* If the expression is incorrect, its errors are printed. *)
if Processings.has_errors pr then begin
    Printf.sprintf
        "There are errors in the program:\n%s"
        (Processings.errors pr
        |> List.map Errors.to_string
        |> String.concat "\n"
        |> Strings.indent 4)
    |> Outputs.print_error;
    if verbose then
        "\nThe expression has not been evaluated." |> Outputs.print_information_1;
    print_newline ();
    exit 1
end;

(* Prints the input expression. *)
if Arguments.exists "--input" then begin
    let po =
        Outputs.build_print_options
            verbose
            (Arguments.option_has_value "--input" "no-rules" |> not)
            (Arguments.option_has_value "--input" "no-shadows" |> not)
            (Arguments.option_has_value "--input" "short-names" |> not)
    in
    if verbose then "# Input expression:\n" |> Outputs.print_information_1;
    Outputs.print po (Processings.input_with_shadow pr)
end;

(* Prints the output expression. *)
if Arguments.exists "--output" then begin
    let po =
        Outputs.build_print_options
            verbose
            (Arguments.option_has_value "--output" "no-rules" |> not)
            (Arguments.option_has_value "--output" "no-shadows" |> not)
            (Arguments.option_has_value "--output" "short-names" |> not)
    in
    if verbose then "# Output expression:\n" |> Outputs.print_information_1;
    Outputs.print po (Processings.output_with_shadow pr)
end;

exit 0

