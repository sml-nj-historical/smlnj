functor TransactionFn(Log : TRANSACTION_LOG) : TRANSACTION =
struct

   exception Abort

   fun transaction default func =
   let
       val _ = Log.begin()
       val x = func()
       val _ = Log.commit()
   in
       x
   end
   handle Abort => (Log.abort(); default)
	| e     => (Log.abort(); raise e)

end

(*
 * $Log: transaction.sml,v $
 * Revision 1.1.1.1  1998/11/16 21:48:55  george
 *   Version 110.10
 *
 *)
