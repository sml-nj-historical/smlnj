signature TRANSACTION =
sig

   exception Abort

   val transaction : 'a -> (unit -> 'a) -> 'a

end

(*
 * $Log: transaction.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:55  george
 *   Version 110.10
 *
 *)
