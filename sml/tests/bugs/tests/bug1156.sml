(* bug1156.sml *)

structure I :
  sig
    type 'a intmap
    val new : unit -> bool intmap
    val add : '2a intmap -> int * '2a -> unit
    val map : 'a intmap -> int -> 'a
  end =
struct
  open Array List
  infix 9 sub
  datatype 'a bucket = NIL | B of (int * 'a * 'a bucket)
  datatype 'a intmap =
    H of {table: 'a bucket array ref,exn: exn}
  val new = fn () => H{table=ref(array(16,NIL)),exn=Overflow}
  fun index () = 
    let val k = 16 (* Array.length a *)
	val v = Word.fromInt(k - 1)
	val w = Word.andb(0wx2, v) 
	val z = Word.toInt w
     in z
    end
  fun map (H{table,exn}) =
      (fn i => let val ref a = table
		   val nn = index () 
		   val _ = print ((Int.toString nn) ^ " nnn \n")
		   fun find NIL = raise exn
		     | find(B(i',j,r)) = if i=i' then j else find r
	       in find(a sub nn) 
	       end)

  fun add _ _ = ()
end;

val x = I.map (I.new())  2;
