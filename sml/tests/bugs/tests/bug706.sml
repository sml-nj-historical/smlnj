(* bug706.sml *)
(* non-terminating structure compilation in 0.75 *)

structure Queue1 =
  struct
    
    type 'a T = 'a list;
    exception E;
    
    fun hd(x::q) = x
      | hd [] = raise E;
  end

signature QUEUE = 
  sig
    type 'a T
    exception E
    val hd : 'a T -> 'b T
  end;

structure Q1: QUEUE = Queue1;
