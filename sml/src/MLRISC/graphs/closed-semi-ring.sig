signature CLOSED_SEMI_RING =
sig

  type elem

  val zero : elem
  val one  : elem
  val +    : elem * elem -> elem
  val *    : elem * elem -> elem
  val star : elem -> elem

end

(*
 * $Log: closed-semi-ring.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:35  george
 *   Version 110.10
 *
 *)
