(* 286.sml *)

functor F() =
struct
  structure A =
   struct
     type u = unit
     type t = int
     val x: t = 3
   end

  functor G() =
  struct
    type s = A.t
    fun f(x: A.t) = ()
  end

  structure A =
   struct
     type t = string
     val x: t = "abc"
   end
end;

structure S = F();
    

