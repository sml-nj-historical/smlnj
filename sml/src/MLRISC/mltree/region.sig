signature REGION = sig
  type region
  val stack : region
  val memory : region
  val toString : region -> string
end

(*
 * $Log: region.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
