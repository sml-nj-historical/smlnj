(* Copyright (c) 1998 by Lucent Technologies *)

(***************************************************************************

  TOKEN.SML: hash table for token recognition

 ***************************************************************************)


signature TOKENTABLE =
sig
	structure Tokens : C_TOKENS
	val checkToken : (string * int) -> (Tokens.svalue,int)Tokens.token
end

functor TokenTable(structure Tokens : C_TOKENS): TOKENTABLE = 
struct
  
  structure Tokens = Tokens
  structure ParseControl = Config.ParseControl
  type item = (int * int) -> (Tokens.svalue, int) Tokens.token
  exception Keyword
  exception LexError
  val keywords : item AtomTable.hash_table = AtomTable.mkTable(64, Keyword)
    
  val _ = let
    val insert = AtomTable.insert keywords
    fun ins (s, item) = insert (Atom.atom s, item)
  in
    app ins ([
	      ("break", Tokens.BREAK),
	      ("case", Tokens.CASE),
	      ("continue", Tokens.CONTINUE),
	      ("default", Tokens.DEFAULT),
	      ("do", Tokens.DO),
	      ("else", Tokens.ELSE),
	      ("auto", Tokens.AUTO),
	      ("extern", Tokens.EXTERN),
	      ("register", Tokens.REGISTER),
	      ("static", Tokens.STATIC),
	      ("for", Tokens.FOR),
	      ("goto", Tokens.GOTO),
	      ("if", Tokens.IF),
	      ("char", Tokens.CHAR),
	      ("double", Tokens.DOUBLE),
	      ("enum", Tokens.ENUM),
	      ("float", Tokens.FLOAT),
	      ("int", Tokens.INT),
	      ("long", Tokens.LONG),
	      ("short", Tokens.SHORT),
	      ("struct", Tokens.STRUCT),
	      ("union", Tokens.UNION),
	      ("unsigned", Tokens.UNSIGNED),
	      ("signed", Tokens.SIGNED),
	      ("const", fn p => if ParseControl.constAllowed 
				  then (Tokens.CONST p)
				else (ParseControl.violation "the keyword 'const' not allowed";
				      raise LexError)),
	      ("volatile", fn p => if ParseControl.volatileAllowed 
				     then (Tokens.VOLATILE p)
				   else (ParseControl.violation "the keyword 'volatile' not allowed";
					 raise LexError)),
	      ("void", Tokens.VOID),
	      ("sizeof", Tokens.SIZEOF),
	      ("typedef", Tokens.TYPEDEF),
	      ("return", Tokens.RETURN),
	      ("switch", Tokens.SWITCH),
	      ("while", Tokens.WHILE)
	      ] @
              (if ParseControl.Dkeywords then
		(* tokens for D *)
		[
		 ]
	      else nil))

  end

  fun checkToken (s, pos) = let
    val endPos = pos + size s
    val name = Atom.atom s
  in
    case (AtomTable.find keywords name)
      of (SOME tokFn) => tokFn(pos, endPos)
       | NONE => 
	   (if TypeDefs.checkTdef(s) = true then 
	      Tokens.TYPE_NAME(s,pos,endPos)
	    else Tokens.ID(s,pos,endPos))
  (* end case *)
  end

end
