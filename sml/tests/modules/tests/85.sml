(* test85.sml *)

structure A =
   struct
      val x = Compiler.Control.internals
   end

(* check abstraction of functor bodies containing mutually-recursive
   datatypes *)

functor F() : sig type d type e end =
   struct
      datatype d = D of e
      and      e = E of d
   end

structure S = F()

