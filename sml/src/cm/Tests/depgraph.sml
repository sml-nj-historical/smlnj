type lib
type env
type sym
type context
type src

type symbols = sym list

type libtemplate

val SRC : context * string -> src

val STR : string -> sym
val SIG : string -> sym
val FCT : string -> sym

val IMPORT : lib * symbols -> env
val COMPILE : src * env * symbols -> env
val LAYER : env list -> env
val FILTER : env * symbols -> env

val LIB : (context -> lib list -> env) -> libtemplate

(* *** *)

(* example *)
val thelibrary = LIB (fn ctxt => (
fn [ lib_basis, lib_mylib ] => let
       val s1 = SRC (ctxt, "baz.sml")
       val str_Int = STR "Int"
       val str_Foo = STR "Foo"
       val sig_BAR = SIG "BAR"
       val fun_Baz = FCT "Baz"
       val str_FooBar = STR "FooBar"
       val ss1 = [str_Int]
       val ss2 = [str_Foo, sig_BAR]
       val ss3 = [f_Baz, S_FooBar]
       val ss4 = [f_Baz]
       val e1 = IMPORT (lib_basis, ss1)
       val e2 = IMPORT (lib_mylib, ss2) 
       val e3 = LAYER [e1, e2]
       val e4 = COMPILE (s1, e3, ss3)
       val e5 = FILTER (e4, ss4)
   in
       e5
   end
 | _ => raise Fail "need libraries \"basis\" and \"mylib\""))
