(* Externally visible aspects of the lexer and parser 
 *
 * $Log$
 * Revision 1.1.1.7  1999/04/17 16:22:25  monnier
 * version 110.11
 *
 * Revision 1.1.1.1  1998/04/08 18:40:15  george
 * Version 110.5
 *
 * Revision 1.1.1.1  1997/01/14 01:38:03  george
 *   Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:39  george
 * Version 109
 * 
 *)

signature INTERFACE =
sig

type pos
val line : pos ref
val init_line : unit -> unit
val next_line : unit -> unit
val error : string * pos * pos -> unit

type arg
val nothing : arg

end  (* signature INTERFACE *)

functor Interface () : INTERFACE =
struct

type pos = int
val line = ref 0
fun init_line () = (line := 1)
fun next_line () = (line := !line + 1)
fun error (errmsg,line:pos,_) =
  output(std_out,"Line " ^ (makestring line) ^ ": " ^ errmsg ^ "\n")

type arg = unit

val nothing = ()

end  (* functor INTERFACE *)
