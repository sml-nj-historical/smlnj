(* json-stream-parser.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure JSONStreamParser : sig

  (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx
      }

    val parser : 'ctx callbacks -> (TextIO.instream * 'ctx) -> unit

  end = struct

    structure Lex = JSONLexer
    structure T = JSONTokens

  (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx
      }

    fun parser cb (inStrm, ctx) = let
	  val lexer = Lex.lex srcMap
	  fun parseValue (strm, ctx) = let
		val (tok, pos, strm) = lexer strm
		in
		  case tok
		   of T.LB => parseArray strm
		    | T.LCB => parseObject strm
		    | T.KW_null => (strm, #null cb ctx)
		    | T.KW_true => (strm, #boolean cb (ctx, true))
		    | T.KW_false => (strm, #boolean cb (ctx, false))
		    | T.INT n => (strm, #integer cb (ctx, n))
		    | T.FLOAT f => (strm, #float cb (ctx, f))
		    | T.STRING s => (strm, #string cb (ctx, s))
		    | _ => (* error *)
		  (* end case *)
		end
	  and parseArray (strm, ctx) = let
		fun loop (strm, ctx) = let
		      val (strm, ctx) = parseValue (strm, ctx)
		    (* expect either a "," or a "]" *)
		      val (tok, pos, strm) = lexer strm
		      in
			case tok
			 of T.RB => (strm, ctx)
			  | T.COMMA => loop (strm, ctx)
			  | _ => (* error *)
			(* end case *)
		      end
		val (strm, ctx) = loop (strm, #startArray cb ctx)
		in
		  (strm, #endArray ctx)
		end
	  and parseObject (strm, ctx) = let
		fun loop (strm, ctx) = let
(* expect STRING COLON value ("," or "}") *)
		      val (tok, pos, strm) = lexer strm
		      in
			case tok
			 of T.EOF =>
			  | T.LB =>
			  | T.RB =>
			  | T.LCB =>
			  | T.RCB =>
			  | T.COMMA =>
			  | T.COLON =>
			  | T.KW_null =>
			  | T.KW_true =>
			  | T.KW_false =>
			  | T.INT n =>
			  | T.FLOAT f =>
			  | T.STRING s =>
			(* end case *)
		      end
		val (strm, ctx) = loop (strm, #startObject cb ctx)
		in
		  (strm, #endObject ctx)
		end
	  in
	    parseValue (Lex.streamifyInstream inStrm, ctx)
	  end

  end
