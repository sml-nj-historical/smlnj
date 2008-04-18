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

    fun parser _ _ = raise Fail "unimplemented"

  end
