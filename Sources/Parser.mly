(* Author: Samuele Giraudo
 * Creation: jun. 2020
 * Modifications: jun. 2020, jan. 2021, feb. 2021, dec. 2021, feb. 2022, mar. 2022,
 * may 2022, aug. 2022, sep. 2022, oct. 2022, nov. 2022, dec. 2022, apr. 2023, jun. 2023,
 * jul. 2023, dec. 2023
 *)

%token L_PAR R_PAR
%token L_BRACK R_BRACK
%token LT GT
%token LTLT GTGT
%token PERCENT
%token PRIME
%token AT
%token COLON
%token SHARP
%token PIPE
%token CIRC
%token EQUAL
%token DOT
%token BANG
%token <string> CHAR_STRING
%token EOF

%start <Expressions.expressions> expression

%%


expression:
    |exp=expression_1 EOF {exp}


expression_1:
    |e=expression_2 {e}

   (* ALIAS = EXP_1 . EXP_2 *)
    |alias=CHAR_STRING EQUAL e1=expression_1 DOT e2=expression_1 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.AliasDefinition (info, alias, e1, e2)
    }


expression_2:
    |e=expression_3 {e}

    (* EXP_1 >EXP_2> EXP_3 *)
    |e1=expression_2 GT e2=expression_3 GT e3=expression_3 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, Expressions.Application (info, e2, e1), e3)
    }


expression_3:
    |e=expression_4 {e}

    (* EXP_1 <EXP_2< EXP_3 *)
    |e1=expression_4 LT e2=expression_4 LT e3=expression_3 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, Expressions.Application (info, e2, e1), e3)
    }


expression_4:
    |e=expression_5 {e}

    (* EXP : SH *)
    |e1=expression_5 COLON sh=expression_4 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Shadow (info, e1, sh)
    }


expression_5:
    |e=expression_6 {e}

    (* EXP_1 << EXP_2 *)
    |e1=expression_6 LTLT e2=expression_5 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, e1, e2)
    }

expression_6:
    |e=expression_7 {e}

    (* EXP_1 >> EXP_2 *)
    |e1=expression_6 GTGT e2=expression_7 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, e1, e2)
    }


expression_7:
    |e=expression_8 {e}

    (* EXP_1 ^ EXP_2 *)
    |e1=expression_8 CIRC e2=expression_7 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Map (info, e1, e2)
    }


expression_8:
    |e=expression_9 {e}

    (* EXP_1 EXP_2 *)
    |e1=expression_8 e2=expression_9 {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Application (info, e1, e2)
    }


expression_9:
    (* (EXP) *)
    |L_PAR e=expression_1 R_PAR {e}

    (* %VAR *)
    |PERCENT name=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let v = Variables.Variable name in
        Expressions.Variable (info, v)
    }

    (* 'CONST *)
    |PRIME name=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let c = Constants.Constant name in
        Expressions.Constant (info, c, [])
    }

    (* 'CONST RULES *)
    |PRIME name=CHAR_STRING rules=rules {
        let info = Information.construct (FilePositions.from_position $startpos) in
        let c = Constants.Constant name in
        Expressions.Constant (info, c, rules)
    }

    (* @ *)
    |AT {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Self info
    }

    (* ALIAS *)
    |alias=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Alias (info, alias)
    }

    (* ! PATH *)
    |BANG path=CHAR_STRING {
        let info = Information.construct (FilePositions.from_position $startpos) in
        Expressions.Put (info, path)
    }


rules:
    (* [ RULE_1 ... RULE_k ] *)
    |L_BRACK rules=list(rule) R_BRACK {rules}


rule:
    (* | EXP_1 ... EXP_k # EXP *)
    |PIPE e_lst=list(expression_9) SHARP e1=expression_1 {(e_lst, e1)}

