(* test34.sml *)
(* keywords: functor, equality *)

signature S = 
sig
     type aa
     datatype bb = BB of aa
     val eq_bb:bb->bb->bool
end;

structure s:S =
struct
    datatype aa =  AA
    datatype bb = BB of aa
    fun eq_bb c d = (c = d)
end;

signature T = 
sig
    type aa
    val eq_aa:aa->aa->bool
end;

structure t:T =
struct
    datatype aa = AA
    fun eq_aa x y = (x=y)
end;


