(* Author: Samuele Giraudo
 * Creation: jan. 2021
 * Modifications: jan. 2021, feb. 2021, nov. 2021, dec. 2021, feb. 2022, mar. 2022,
 * may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun 2023
 *)

(* A type for aliases. *)
type alias = string

(* A type for paths. *)
type path = string

(* A type for the expressions of the Aclove language. *)
type expressions =

    (* The nodes of the term expressions. *)
    |Variable of (Information.information * Variables.variables)
    |Constant of (Information.information * Constants.constants * (rules list))
    |Self of (Information.information * Selfs.selfs)
    |Application of (Information.information * expressions * expressions)
    |Map of (Information.information * expressions * expressions)

    (* The quasi meta-node of shadow annotations. *)
    |Shadow of (Information.information * expressions * shadows)

    (* The meta-nodes of the expressions for the management of level shifts. *)
    |Shift of (Information.information * Levels.levels * expressions)

    (* The meta-nodes of the expressions for the management of aliases. *)
    |Alias of (Information.information * alias)
    |AliasDefinition of (Information.information * alias * expressions * expressions)

    (* The meta-node of the expressions for the management of inclusions. *)
    |Put of (Information.information * path)

(* A shadow is an expression (this is defined for clarity). *)
and shadows = expressions

(* A rule is a pair of expressions. *)
and rules = expressions * expressions

(* Here are some definitions of notions related to expressions and used in other files.
 *
 *     - Given an expression e, a variable, a constant, or a self of e is EXTERNAL if it
 *       does not belong to any constant shadow and any rule.
 *
 *     - Given an expression e, a variable, a constant, or a self of e is NESTED if it
 *       belongs to the shadow or the rules of a constant of e.
 *
 *     - The DOMINANT LEAF of an expression e is the variable, the constant, or the self
 *       which is located as left children of the application nodes forming a maximal left
 *       branch from the root,
 *
 *     - The ARGUMENTS of an expression e are the subexpressions of e located as right
 *       children of the application nodes forming a maximal left branch from the root. The
 *       FIRST ARGUMENT is the deepest one.
 *
 *     - The expression e is SIMPLE if e has no aliases (no Alias and no AliasDefinition
 *       nodes) and no inclusion nodes (no Put nodes).
 *
 *     - A simple expression e' is a PREFIX of a simple expression e if its possible to
 *      superimpose e' onto e, by sharing their roots and by possibly superimpose variables
 *      with any other nodes.
 *
 *     - A simple expression is LINEAR if it does not admit any repetition of the same
 *       variable in a left member of any of its rules.
 *
 *     - A SHADOW is an expression which is considered as a type. If e is an expression of
 *       level lvl, a shadow of e has level lvl + 1.
 *
 *     - Levels can be negative. In this way, an expression considered as a value is in fact
 *       also a shadow.
 *
 *)

(* An exception to handle cases where an expression has a inappropriate form. *)
exception ValueError of (expressions * string)

