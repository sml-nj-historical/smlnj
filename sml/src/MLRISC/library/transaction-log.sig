signature TRANSACTION_LOG =
sig

   exception TransactionLog

   type version = int
   val version      : version ref
   val add_object   : { rollback : version -> unit, 
			commit   : version -> unit } -> unit   
   val begin        : unit -> unit
   val commit       : unit -> unit
   val abort        : unit -> unit
   val init         : unit -> unit
end

(*
 * $Log: transaction-log.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:55  george
 *   Version 110.10
 *
 *)
