(* test53.sml *)
(* keywords: functor, sharing, origin *)

(* causes a compiler bug in version 0.70 *)
(* This code shows that we'll need to augment structure instantiation
   arrays during functor abstraction to include structures which are
   not in the signature, but which have views that are in the
   signature.  D. Tarditi
*)

signature S0 =
sig
  type u
end;

signature S1 =
sig
  type t
  val v : t
end;

(* define a structure A, but export only views of A *)

functor F1() : sig
	         structure B : S0
	         structure C : S1
	       end =
struct
  structure A =
    struct
      datatype u = U
      datatype t = T
      val v = T
    end
  structure B : S0 = A
  structure C : S1 = A
end;

structure D = F1();

(* the definitional sharing constraint implies that C.t = D.C.t,
   but we won't know this unless we keep the origin of D.B around.*)

functor F2(A : sig 
	        structure C : S1
	        sharing D.B = C
	       end) : sig
	 	         val v : A.C.t
	              end =
struct
  val v = D.C.v
end;


