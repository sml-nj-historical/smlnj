(* bug29.sml *)
(* keywords: functor, typedef *)

(* tarditi *)

signature FF =
sig
  type t
end;

functor F(type s) : FF =
struct
  type a = s list
  type t = a list
end;
