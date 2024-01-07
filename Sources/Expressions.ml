(* Author: Samuele Giraudo
 * Creation: jan. 2021
 * Modifications: jan. 2021, feb. 2021, nov. 2021, dec. 2021, feb. 2022, mar. 2022,
 * may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun 2023,
 * dec. 2023, jan. 2023
 *)

module C = Constants
module I = Information
module V = Variables

(* A type for aliases. *)
type alias = string

(* A type for paths. *)
type path = string

(* A type for the expressions of the Aclove language. *)
type expressions =

    (* The nodes of the term expressions. *)
    |Variable of (I.information * V.variables)
    |Constant of (I.information * C.constants * (rules list))
    |Self of I.information
    |Application of (I.information * expressions * expressions)
    |Map of (I.information * expressions * expressions)

    (* The quasi meta-node of shadow annotations. *)
    |Shadow of (I.information * expressions * shadows)

    (* The meta-nodes of the expressions for the management of aliases. *)
    |Alias of (I.information * alias)
    |AliasDefinition of (I.information * alias * expressions * expressions)

    (* The meta-node of the expressions for the management of inclusions. *)
    |Put of (I.information * path)

(* A shadow is an expression (this is defined for clarity). *)
and shadows = expressions

(* A rule is a pair of expressions. *)
and rules = (expressions list) * expressions

(* Here are some definitions of notions related to expressions and used in other files.
 *
 *     - Given an expression e, a variable, a constant, or a self of e is EXTERNAL if it
 *       does not belong to any constant shadow nor to any rule.
 *
 *     - An expression e has a DOMINANT BRANCH if there is a path consisting in application
 *       nodes and left branches connecting the root of e and a variable or a constant.
 *
 *     - The DOMINANT CONSTANT of an expression e is the constant at the end of the dominant
 *       branch of e.
 *
 *     - An expression e1 is a PREFIX of an expression e2 if it is possible to superimpose
 *       e1 into e2 such that all nodes coincide, except possibly variables of e1 which can
 *       be put onto any node of e2.
 *
 *     - An expression is LINEAR if it does not admit any repetition of the same variable in
 *       a left member of any of its rules.
 *
 *     - A SHADOW is an expression which is considered as a type.
 *)

(* An exception to handle cases where an expression has a inappropriate form. *)
exception ValueError of (expressions * string)

(* Raise the exception ValueError with the expression e and the string msg as
 * information. *)
let error e msg =
    ValueError (e, msg) |> raise

