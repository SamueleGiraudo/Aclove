(* Author: Samuele Giraudo
 * Creation: (may 2018), mar. 2020
 * Modifications: mar. 2020, apr. 2020, may 2020, jun. 2020, dec. 2020, jan. 2021,
 * feb. 2021, nov. 2021, dec. 2021, feb. 2022, mar. 2022, may 2022, jun. 2022, aug. 2022,
 * sep. 2022, oct. 2022, nov. 2022, dec. 2022, jan. 2023, apr. 2023, jun. 2023, jul. 2023,
 * aug. 2023, sep. 2023, oct. 2023, nov. 2023, dec. 2023, jan. 2024
 *)

(* Aclove - A programming language based on a mix of combinatory logic and general term
 * rewrite systems. *)


module A = Arguments
module D = DataErrors
module E = Errors
module Ev = Evaluations
module F = Files
module O = Outputs
module Op = Options
module P = Paths
module Ss = Statistics
module St = Strings

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
(*let version = "1.1110" and version_date = "2023-07-17"*)
let version = "1.1111" and version_date = "2024-01-07"

let author = "Samuele Giraudo"

(*let email = "samuele.giraudo@univ-eiffel.fr"*)
let email = "giraudo.samuele@uqam.fr"

(* Returns a string of information about the Aclove program. *)
let information =
    Printf.sprintf "%s\n%s\n%s\nCopyright (C) 2018--2024 %s\nWritten by %s [%s]\n\
        Version: %s (%s)\n"
        logo name description author author email version version_date

(* Returns the help string about the arguments of the program. *)
let help_string =
      "Usage:\n    ./aclove [--help] [--version] --file PATH [--verbose LVL] \
       [--no-rules] [--no-shadows] [--short-names]\nwhere:\n"
    ^ "    + `--help` prints the short help (the present text).\n"
    ^ "    + `--version` prints the version and other information.\n"
    ^ "    + `--file PATH` sets PATH as the path to the Aclove program to consider, \
             contained in a " ^ F.extension ^ " file.\n"
    ^ "    + `--verbose LVL` enables the verbose mode at level `LVL`, from 0 (nothing) to \
             2 (full). By default, the level is 1.\n"
    ^ "    + `--no-rules` drops the rules of the result expression.\n"
    ^ "    + `--no-shadows` drops the shadows of the result expression.\n"
    ^ "    + `--short-names` keeps only the last part of the constant names of the result \
             expression.\n"

(* Prints the error message msg followed by a line break, and halts the execution. *)
let error msg =
    "Error: " ^ msg ^ "\n" |> O.print_error;
    exit 1

(* Prints the errors of the data errors ee for the specified verbosity level verbosity. *)
let print_errors verbosity ee =
    if D.has_errors ee then begin
        Printf.sprintf
            "There are errors in the program:\n%s"
            (D.errors ee |> List.map E.to_string |> String.concat "\n" |> St.indent 4)
        |> O.print_error;
        if verbosity >= 2 then begin
            print_newline ();
            "The expression has not been evaluated." |> O.print_information_1
        end;
        print_newline ();
        exit 1
    end

(* Returns the data errors on an expression obtained by processing the possible expression
 * of the data errors ee by the processing process_f. The verbosity level is specified by
 * verbosity and name is the name of the process. *)
let process verbosity name process_f ee =
    if verbosity >= 2 then Printf.sprintf "%s...\n" name |> O.print_information_1;
    let clock_start = Unix.gettimeofday () in
    let ee = process_f ee in
    let clock_end = Unix.gettimeofday () in
    if verbosity >= 2 then begin
        O.print_success "Done.\n";
        Printf.sprintf "Duration: %.2f s.\n" (clock_end -. clock_start)
        |> O.print_information_3
    end;
    ee

;;

(* Main expression. *)

(* Version. *)
if A.exists "--version" then begin
    O.print_success information;
    exit 0
end;

(* Help. *)
if A.exists "--help" then begin
    O.print_information_1 help_string;
    exit 0
end;

(* Reads the Aclove program file path. *)
let path =
    match A.option_value "--file" with
        |None -> error "the option --file must be given and followed by one path."
        |Some path -> path
in

(* Checks the existence of the file at path path. *)
if Sys.file_exists path |> not then error (Printf.sprintf "there is no file %s." path);

(* Checks if the file has the right extension. *)
if not (P.has_extension F.extension path) then
    error (Printf.sprintf "the file %s has not %s as extension." path F.extension);

(* Reads the verbosity level. *)
let verbosity =
    if A.exists "--verbose" |> not then
        1
    else
        match A.bounded_integer_option_value 0 2 "--verbose" with
            |None -> error "one integer between 0 and 2 must follow the --verbose argument."
            |Some lvl -> lvl
in

(* Detects the options. *)
let with_rules = A.exists "--no-rules" |> not in
let with_shadows = A.exists "--no-shadows" |> not in
let with_full_names = A.exists "--short-names" |> not in

(* Reads the main Aclove program. *)
let ee = Ev.from_path path in

(* Pre-processes the given Aclove program. *)
let ee = process verbosity "Pre-processing" Ev.pre_process ee in

(* If the expression is incorrect, its errors are printed. *)
print_errors verbosity ee;

(* Evaluates the expression. *)
let ee = process verbosity "Evaluating" Ev.process ee in

(* If the expression is incorrect, its errors are printed. *)
print_errors verbosity ee;

(* Post-processes the expression. *)
let ee =
    process
        verbosity
        "Post-processing"
        (Ev.post_process with_rules with_shadows with_full_names)
        ee
in

(* Prints statistics on the expression. *)
if verbosity >= 2 then begin
    "Output expression:\n" |> O.print_information_1;
    "Characteristics:\n" |> O.print_information_2;
    D.data ee |> Option.get |> Ss.compute |> Ss.to_string |> St.indent 4 |> print_string;
    "Expression:\n" |> O.print_information_2
end;

(* Prints the expression. *)
if verbosity >= 1 then begin
    Buffer.output_buffer stdout (O.to_buffered_string (D.data ee |> Option.get));
    print_newline ()
end;

exit 0

