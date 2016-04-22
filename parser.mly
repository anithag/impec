%{
  open Ast
  open Printf
  open Lexing
%}

%token <int> INTEGER
%token <int> LOC
%token <string> VAR
%token <char> CHANNEL
%token <int> ENCLAVE
%token PLUS MODULO UNDERSCORE LPAREN RPAREN LCURLY RCURLY COMMA SEQ COLON DOT NEQUALS EQUALS TRUE FALSE CALL
       IF THEN ELSE ENDIF LAMBDA EOF DEREF UPDATE SET ISUNSET  OUTPUT ASSIGN SKIP WHILE DO END
       INT BOOL COND FUNC REF LOW HIGH ERASE CHANNEL DECLASSIFY TOP  NORMAL  MUTABLE IMMUTABLE CARET

%type <Ast.program> program
%type <Ast.encstmt> stmt
%type <Ast.encexp> exp
%type <Ast.cndset> Uset

%start program
 
%%

Uset    : LCURLY RCURLY 		{VarSet.empty}
	 | LCURLY unsetcnd RCURLY	{VarSet.union $2 VarSet.empty }

unsetcnd : VAR 				{VarSet.add $1 VarSet.empty}
	 | VAR COMMA unsetcnd		{VarSet.add $1 $3}	
 

policy  : LOW						{Low}
        | HIGH						{High}
	| TOP						{Top}
        | policy ERASE policy COMMA VAR 		{Erase($1, $5, $3)}

mode 	: NORMAL					{ Normal}
        | ENCLAVE 					{Enclave($1)} 

reftype : MUTABLE					{Mutable}
	| IMMUTABLE					{Immutable}
	
basetype  : INT				   		{EBtInt}
          | BOOL			   		{EBtBool}
          | COND UNDERSCORE mode 	   		{EBtCond($3)}
          | FUNC LPAREN mode COMMA LCURLY vardecllist RCURLY COMMA eidlist COMMA policy COMMA Uset COMMA LCURLY vardecllist RCURLY COMMA eidlist RPAREN {EBtFunc($3, $6, $9, $11, $13, $16, $19)}
	  | labeltype CARET mode REF CARET reftype			{EBtRef($3, $6, $1)}

labeltype : basetype UNDERSCORE policy  { ($1, $3) }

eidlist   : LCURLY RCURLY	   { [] }
	  | LCURLY eidnonemptylist RCURLY { $2 }

eidnonemptylist   : INTEGER COMMA eidnonemptylist  { [$1] @ $3}
	  	  | INTEGER		   	   { [$1] }

vardecl   : LOC COLON labeltype   { VarLocMap.add (Mem $1) $3 VarLocMap.empty } 
          | VAR COLON labeltype	    { VarLocMap.add (Reg $1) $3 VarLocMap.empty }
	 
vardecllist: vardecl                { $1 }
	 |vardecl SEQ vardecllist   { VarLocMap.merge (fun key left right -> match left, right with
							| Some x, Some y -> None (* Error *)
							| None, right -> right
							| left, None  -> left
						      ) $1 $3 }
program: vardecllist stmt           {($1, $2)} 

stmt : IF bexp THEN stmt ELSE stmt ENDIF  	{ EIf($2, $4, $6) }
     | SKIP			   		{ ESkip }
     | VAR ASSIGN exp		    		{ EAssign($1, $3)  }
     | VAR ASSIGN DECLASSIFY LPAREN exp RPAREN  { EDeclassify($1, $5)  }
     | stmt SEQ stmt		    		{ ESeq($1, $3)  }
     | WHILE bexp DO stmt   END     		{ EWhile($2, $4) }
     | exp UPDATE exp 		    		{ EUpdate($1, $3) }
     | OUTPUT LPAREN CHANNEL COMMA aexp RPAREN  { EOutput($3, $5) }
     | CALL LPAREN exp RPAREN 	    		{ ECall($3) }
     | SET LPAREN VAR RPAREN        		{ ESet($3) }


exp : bexp 				{ $1 }
    | aexp 				{ $1 }
    | lexp                              { $1 }

lexp : LPAREN LAMBDA LPAREN mode COMMA LCURLY vardecllist RCURLY COMMA eidlist COMMA policy COMMA Uset COMMA LCURLY vardecllist RCURLY COMMA eidlist RPAREN DOT stmt RPAREN UNDERSCORE policy 	{ ELam($4,$7,$10,$12,$14,$17, $20, $26, $23) }

bexp: TRUE			  			 { ETrue  }
    | FALSE                        			 { EFalse }
    | aexp EQUALS aexp 					 { EEq($1, $3) }
    | aexp NEQUALS aexp 				 { ENeq($1, $3) }
    | ISUNSET LPAREN VAR RPAREN   			 { EIsunset($3) }

aexp: VAR                          { EVar $1}
    | INTEGER                      { EConstant($1) }
    | LOC			   { ELoc $1 }
    | aexp PLUS aexp 		   { EPlus($1, $3) }
    | aexp MODULO aexp 		   { EModulo($1, $3) }
    | LPAREN DEREF exp	RPAREN	   { EDeref($3) }
