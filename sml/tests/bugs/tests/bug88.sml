(* bug88.sml -- subscript exception while printing *)

signature A = sig type t end;

functor F1(a:A) = struct
  datatype t2 = f of a.t
  end;

functor F2(a:A) = struct
  structure S = F1(a);
  open S
  end;

structure SA = struct type t = int end;

structure F2SA = F2(SA);
