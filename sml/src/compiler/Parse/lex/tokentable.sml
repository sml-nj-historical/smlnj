(* tokentable.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(***************************************************************************

  TOKEN.SML: hash table for token recognition

 ***************************************************************************)

functor TokenTable (Tokens:ML_TOKENS) : sig

    val checkId : (string * int) -> (Tokens.svalue,int) Tokens.token
    val checkSymId : (string * int) -> (Tokens.svalue,int) Tokens.token
    val checkTyvar : (string * int) -> (Tokens.svalue,int) Tokens.token

end = struct

    exception NotToken

    structure Tbl = IntStrMap

    val hashStr = HashString.hashString

    fun mkTable (sz, l) = let
	val t = Tbl.new (128, NotToken)
	fun ins (str, tok) =
	    let val strsz = size str
	    in
		Tbl.add t (hashStr str, str,
			   fn yypos => tok (yypos, yypos + strsz))
	    end
    in
	List.app ins l;
	t
    end

    val symIdTbl = mkTable (16, [
	    ("*"	, Tokens.ASTERISK),
	    ("|"	, Tokens.BAR),
	    (":"	, Tokens.COLON),
	    (":>"	, Tokens.COLONGT),
	    ("="	, Tokens.EQUALOP),
	    ("#"	, Tokens.HASH),
	    ("->"	, Tokens.ARROW),
	    ("=>"	, Tokens.DARROW)
	  ])

    val idTbl = mkTable (64, [
	    ("and"	, Tokens.AND),
	    ("abstype"	, Tokens.ABSTYPE),
	    ("as"	, Tokens.AS),
	    ("case"	, Tokens.CASE),
	    ("datatype"	, Tokens.DATATYPE),
	    ("else"	, Tokens.ELSE),
	    ("end"	, Tokens.END),
	    ("eqtype"	, Tokens.EQTYPE),
	    ("exception", Tokens.EXCEPTION),
	    ("do"	, Tokens.DO),
	    ("fn"	, Tokens.FN),
	    ("fun"	, Tokens.FUN),
	    ("functor"	, Tokens.FUNCTOR),
	    ("funsig"	, Tokens.FUNSIG),
	    ("handle"	, Tokens.HANDLE),
	    ("if"	, Tokens.IF),
	    ("in"	, Tokens.IN),
	    ("include"	, Tokens.INCLUDE),
	    ("infix"	, Tokens.INFIX),
	    ("infixr"	, Tokens.INFIXR),
	    ("lazy"	, Tokens.LAZY),
	    ("let"	, Tokens.LET),
	    ("local"	, Tokens.LOCAL),
	    ("nonfix"	, Tokens.NONFIX),
	    ("of"	, Tokens.OF),
	    ("op"	, Tokens.OP),
	    ("open"	, Tokens.OPEN),
	    ("overload"	, Tokens.OVERLOAD),
	    ("raise"	, Tokens.RAISE),
	    ("rec"	, Tokens.REC),
	    ("sharing"	, Tokens.SHARING),
	    ("sig"	, Tokens.SIG),
	    ("signature", Tokens.SIGNATURE),
	    ("struct"	, Tokens.STRUCT),
	    ("structure", Tokens.STRUCTURE),
	    ("then"	, Tokens.THEN),
	    ("type"	, Tokens.TYPE),
	    ("val"	, Tokens.VAL),
	    ("where"	, Tokens.WHERE),
	    ("while"	, Tokens.WHILE),
	    ("with"	, Tokens.WITH),
	    ("withtype"	, Tokens.WITHTYPE),
	    ("orelse"	, Tokens.ORELSE),
	    ("andalso"	, Tokens.ANDALSO),
	    ("link_plugin", Tokens.LINK_PLUGIN)
	  ])

    val overloadHash = hashStr "overload"
    val lazyHash = hashStr "lazy"
    val linkPluginHash = hashStr "link_plugin"

  (* look-up an identifier.  If the symbol is found, the corresponding token is
   * generated with the position of its begining. Otherwise it is a regular
   * identifier. *)
    fun checkId (str, yypos) = let
	val hash = hashStr str
	fun mkId () =
	    Tokens.ID(FastSymbol.rawSymbol(hash,str), yypos, yypos+size(str))
    in
	let val tokFn = Tbl.map idTbl (hash, str)
	in
	    if not (!ParserControl.overloadKW) andalso
	              hash = overloadHash andalso str = "overload"
	       orelse not (!ParserControl.lazysml) andalso
		      hash = lazyHash andalso str = "lazy"
	       orelse not (!ParserControl.linkPluginKW) andalso
		      hash = linkPluginHash andalso str = "link_plugin"
	    then mkId ()
	    else tokFn yypos
	end handle NotToken => mkId ()
    end

    fun checkSymId (str, yypos) = let
	val hash = hashStr str
    in
	Tbl.map symIdTbl (hash, str) yypos
	handle NotToken =>
	       Tokens.ID(FastSymbol.rawSymbol(hash,str),
			 yypos, yypos+size(str))
    end

    fun checkTyvar (str, yypos) = let
	val hash = hashStr str
    in
	Tokens.TYVAR (FastSymbol.rawSymbol(hash,str),yypos,yypos+size (str))
    end
end
