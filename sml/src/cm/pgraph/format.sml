(* format.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Format the list-of-edges dependency graph so it becomes a valid
 * ML program.
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure FormatPortable : sig
    val output : PortableGraph.graph * TextIO.outstream -> unit
end = struct
    structure P = PortableGraph

    fun output (P.GRAPH { imports, defs, export }, outs) = let
	val context = "C"
	fun out l = app (fn x => TextIO.output (outs, x)) l

	fun varlist [] = "[]"
	  | varlist [x] = concat ["[", x, "]"]
	  | varlist (h :: t) =
	    concat ("[" :: h :: foldr (fn (x, a) => ", " :: x :: a) ["]"] t)

	fun cfc (front, args) =
	    (out [front];
	     app (fn x => out [" ", x]) (context :: args))

	fun tos s = concat ["\"", String.toString s, "\""]

	fun rhs (P.SYM (ns, n)) = cfc ("SYM", [tos ns, tos n])
	  | rhs (P.SYMS syms) = cfc ("SYMS", [varlist syms])
	  | rhs (P.IMPORT { lib, syms }) = cfc ("IMPORT", [lib, syms])
	  | rhs (P.COMPILE { src, env, syms, native }) =
	    cfc (if native then "NCOMPILE" else "COMPILE",
		 [tos src, env, syms])
	  | rhs (P.FILTER { env, syms }) = cfc ("FILTER", [env, syms])
	  | rhs (P.MERGE l) = cfc ("MERGE", [varlist l])

	fun dodef (P.DEF d) =
	    (out ["       val (C, ", #lhs d, ") = "]; rhs (#rhs d); out ["\n"])
    in
	out ["val thelibrary = fn ", context, " => (\n"];
	out ["fn ", varlist imports, " => let\n"];
	app dodef defs;
	out ["   in\n       EXPORT ", context, " ", export,
	     "\n   end\n\
	     \ | _ => raise Fail \"wrong number of input libraries\")\n"]
    end
end
