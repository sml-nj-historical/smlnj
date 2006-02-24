structure S = LexSpec

structure RE = RegExp
structure SIS = RE.SymSet

val symTable : RE.re AtomMap.map ref = ref AtomMap.empty 

fun charToSym c = Word32.fromInt (Char.ord c)
fun oneChar c = SIS.singleton (charToSym c)
fun oneChar' s = oneChar (String.sub (s, 0))

%%

%name MLLex

%term 
    EOF
  | DECLS of string
  | LT		(* < *)
  | GT		(* > *)
  | LP		(* ( *)
  | RP		(* ) *)
  | LB		(* [ *)
  | RB		(* ] *)
  | RBD		(* -] *)
  | LCB		(* { *)
  | RCB		(* } *)
  | CHARS of RegExp.sym_set 
  | QMARK    
  | STAR     
  | PLUS     
  | BAR
  | CARAT 
  | DOLLAR 
  | SLASH 
  | DASH
  | CHAR of string
  | EQ
  | REPS of int
  | ID of string 
  | ARROW
  | ACT of string
  | SEMI 
  | LEXMARK 
  | COMMA
  | STATES
  | LEXSTATE of string
  | COUNT
  | REJECTTOK
  | FULLCHARSET
  | STRUCT
  | HEADER
  | ARG
  | POSARG

%nonterm
    Start of S.spec
  | Decls of string
  | Defs of S.config
  | StartStates of AtomSet.set
  | Rules of S.rule list
  | Rule of S.rule
  | RuleStates of AtomSet.set
  | OrExp of RE.re
  | CatExp of RE.re
  | Exp of RE.re
  | InExp of RE.re
  | CharClass of SIS.set
  | CharClass' of SIS.set
  | CharRng of SIS.set
  | AChar of Char.char

%left BAR
%nonassoc QMARK
%left PLUS
%nonassoc STAR

%pos int
%eop EOF
%noshift EOF
%start Start
%verbose

%%

Start
	: Decls LEXMARK Defs LEXMARK Rules
		(S.Spec {decls = Decls, 
		         conf = Defs, 
			 rules = Rules})

Decls
	: DECLS	
		(DECLS)
	| (* empty *)
		("")

Defs
	: (* empty *)
		(S.mkConfig())
	| Defs STATES StartStates SEMI
		(S.updStartStates (Defs, StartStates))
	| Defs HEADER ACT
		(S.updHeader (Defs, 
		   String.substring (ACT, 1, String.size ACT - 2)))
	| Defs STRUCT ID
		(S.updStructName (Defs, ID))
	| Defs ARG ACT
		(S.updArg (Defs, ACT))
	| Defs COUNT
		(Defs)
	| Defs REJECTTOK
		(Defs)
	| Defs ID EQ OrExp SEMI
		(symTable := AtomMap.insert 
		  	       (!symTable, Atom.atom ID, OrExp);
		 Defs)

StartStates
	: LEXSTATE
		(AtomSet.singleton (Atom.atom LEXSTATE))
	| LEXSTATE StartStates
		(AtomSet.add (StartStates, Atom.atom LEXSTATE))

Rules
	: (* empty *)
		([])
	| Rule Rules
		(Rule :: Rules)

Rule	
	: OrExp ARROW ACT
		((NONE, OrExp), ACT)
	| LT RuleStates GT OrExp ARROW ACT
		((SOME RuleStates, OrExp), ACT)

RuleStates
	: LEXSTATE
		(AtomSet.singleton (Atom.atom LEXSTATE))
	| RuleStates COMMA LEXSTATE
		(AtomSet.add (RuleStates, Atom.atom LEXSTATE))

OrExp
	: OrExp BAR CatExp
		(RE.mkOr (OrExp, CatExp))
	| CatExp
		(CatExp)

CatExp
	: CatExp Exp
		(RE.mkConcat (CatExp, Exp))
	| Exp
		(Exp)

Exp
	: Exp QMARK
		(RE.mkOpt Exp)
	| Exp STAR
		(RE.mkClosure Exp)
	| Exp PLUS
		(RE.mkAtLeast (Exp, 1))
	| Exp REPS RCB
		(RE.mkRep (Exp, REPS, REPS))
	| Exp REPS COMMA REPS RCB
		(RE.mkRep (Exp, REPS1, REPS2))
	| InExp
		(InExp)

InExp
	: CHARS
		(RE.mkSymSet CHARS) 
	| ID RCB
		(case AtomMap.find (!symTable, Atom.atom ID)
		  of SOME re => re
		   | NONE => raise Fail ("'" ^ ID ^ "' not defined"))
	| LP OrExp RP
		(OrExp)
	| LB CARAT CharClass 
		(RE.mkSymSet (SIS.complement CharClass))
	| LB CharClass 
		(RE.mkSymSet CharClass)

CharClass
	: CharClass' RB
		(CharClass')
	| DASH CharClass' RB
		(SIS.add (CharClass', charToSym #"-"))
	| CharClass' RBD
		(SIS.add (CharClass', charToSym #"-"))

CharClass'
	: CHAR
		(oneChar (String.sub (CHAR, 0)))
	| CHAR DASH AChar
		(SIS.interval (charToSym (String.sub (CHAR, 0)), 
				charToSym AChar))
	| CharClass' CharRng
		(SIS.union (CharRng, CharClass'))

CharRng
	: AChar DASH AChar
		(SIS.interval (charToSym AChar1, charToSym AChar2))
	| AChar
		(oneChar AChar)

AChar	
	: CARAT
		(#"^")
	| CHAR
		(String.sub (CHAR, 0))