(* check printing of constructors whose datatype def'n has been hidden.
   We shouldn't print constructors whose datatype def'n is visible, since
   we'll print them when the def'n is printed.  We should print constructors
   whose datatype def'n isn't visible.*)

functor F() =
   struct
      datatype d = D of e
      and      e = E of d
      datatype d = F
   end

structure S = F()
