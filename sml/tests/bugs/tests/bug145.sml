(* bug145.sml *)

val cl = ref([]:int SMLofNJ.Cont.cont list);
SMLofNJ.Cont.callcc (fn k=>(cl:=[k]; 42));
val u :string = SMLofNJ.Cont.throw (hd (!cl)) 65; (* value 65 with string type! *)
u+1;     (* u as integer *)
u^"str"; (* u as string *)
u:bool;  (* u as boolean (cannot print) *)
u:real;  (* u as real (core dump) *)
