(* bug729.sml *)
(* include causes sharing violation *)

signature ELEMENT =
sig
  type t
  val put : unit  (* necessary *)
end;

signature TERM =
sig
  structure S : sig 
	          include ELEMENT
		end
  type subst
  sharing type subst = S.t
end;

functor Term () : sig include TERM end = (* ": TERM" works ok *)
struct
  structure S =
  struct
    datatype t = C   (* must be datatype -- "type t = unit" works ok *)
    val put = ()
  end

  type subst = S.t
end;

structure Terms : TERM = Term();  (* signature ": TERM" necessary *)
