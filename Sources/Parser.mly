(* Author: Samuele Giraudo
 * Creation: jun. 2020
 * Modifications: jun. 2020, jan. 2021, feb. 2021, dec. 2021, feb. 2022, mar. 2022,
 * may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun. 2023,
 * jul. 2023
 *)

%token L_PAR R_PAR
%token L_BRACK R_BRACK
%token LT GT
%token PERCENT
%token PRIME
%token PLUS
%token MINUS
%token AT
%token COLON
%token ARROW
%token SEMICOLON
%token CIRC
%token LET
%token EQUAL
%token IN
%token PUT
%token BANG
%token <string> CHAR_STRING
%token EOF

%start <Expressions.expressions> expression

%%


expression:
    |exp=expression_1 EOF {exp}


expression_1:
    |e=expression_2 {e}

   (* let ALIAS = EXP_1 in EXP_2 *)
    |LET alias=CHAR_STRING EQUAL e1=expression_1 IN e2=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.AliasDefinition (info, alias, e1, e2)
    }


expression_2:
    |e=expression_3 {e}

    (* EXP_1 <EXP_2> EXP_3 *)
    |e1=expression_2 LT e2=expression_1 GT e3=expression_3 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, Expressions.Application (info, e2, e1), e3)
    }


expression_3:
    |e=expression_4 {e}

    (* EXP : SH *)
    |e1=expression_3 COLON sh=expression_4 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Shadow (info, e1, sh)
    }


expression_4:
    |e=expression_5 {e}

    (* EXP_1 ^ EXP_2 *)
    |e1=expression_5 CIRC e2=expression_4 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Map (info, e1, e2)
    }


expression_5:
    |e=expression_6 {e}

    (* EXP_1 EXP_2 *)
    |e1=expression_5 e2=expression_6 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, e1, e2)
    }


expression_6:
    |e=expression_7 {e}

    (* + EXP *)
    |PLUS e=expression_6 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Shift (info, Levels.Level 1, e)
    }

    (* - EXP *)
    |MINUS e=expression_6 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Shift (info, Levels.Level (-1), e)
    }


expression_7:
    (* (EXP) *)
    |L_PAR e=expression_1 R_PAR {e}

    (* %VAR *)
    |PERCENT name=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let v = Variables.Variable (Levels.Level 0, name) in
        Expressions.Variable (info, v)
    }

    (* 'CONST RULES *)
    |PRIME name=CHAR_STRING rules=rules {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let c = Constants.Constant (Levels.Level 0, name) in
        Expressions.Constant (info, c, rules)
    }

    (* 'CONST *)
    |PRIME name=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let c = Constants.Constant (Levels.Level 0, name) in
        Expressions.Constant (info, c, [])
    }

    (* @ *)
    |AT {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let s = Selfs.Self (Levels.Level 0) in
        Expressions.Self (info, s)
    }

    (* ALIAS *)
    |alias=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Alias (info, alias)
    }

    (* put PATH *)
    |PUT path=CHAR_STRING
    (* ! PATH *)
    |BANG path=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Put (info, path)
    }


rules:
    (* [ RULE_1 ; ... ; RULE_k ] *)
    |L_BRACK rules=separated_list(SEMICOLON, rule) R_BRACK {rules}


rule:
    (* EXP_1 -> EXP_2 *)
    |e1=expression_2 ARROW e2=expression_1 {(e1, e2)}

