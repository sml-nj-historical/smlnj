(* Functor abstraction may need to abstract an EMBEDDED structure without
   ever abstracting the TOP structure which also contains the instantiation
   arrays for the EMBEDDED structure.*)

functor F() =
struct
  local
    structure A : sig structure B : sig type t end end =
    struct
      structure B = struct type t = int end
    end
  in
    structure C = A.B
  end
end;

structure S1 = F();

structure S2 = F();

(* verify that S1.C.t and S2.C.t are equivalent *)
val h(f: S1.C.t->unit, x: S2.C.t) = f x;

