(* test equality properties: S.t will have an equality property of IND.
   When signature S is instantiated, d should also have an equality property
   of IND.*)

structure S :> sig type t end = struct type t=int end

signature S =
  sig
     datatype d = C of S.t
  end
