(* 234.sml *)
(* tests open in functor *)

(* 1) creation of a structure coming from a functor with an open *)

signature S = 
sig
  eqtype t
  val x:t 
  structure s:sig type u val y:u end
end

structure a = 
struct 
  datatype t=c
  val x=c
  structure s = struct datatype u = d val y=d end
end

functor OP(X:S) = 
struct
  open X
  val w= X.x = x
end;

functor OP2(X:S):S = 
struct
  open X
  val w= X.x = x
end;

structure b=OP(a);
structure b2 = OP2(a);

(* 2) then you play around with a *)

structure c:S=b;
structure d=OP(c);

(a.x = b.x) andalso (a.x = c.x) andalso (a.x = d.x);

(a.s.y = b.s.y) andalso (a.s.y = c.s.y) andalso (a.s.y = d.s.y);

(* 3) playing after coercion *)

structure c2=b2;
structure d2=OP2(c2);

(a.x = b2.x) andalso (a.x = c2.x) andalso (a.x = d2.x);
