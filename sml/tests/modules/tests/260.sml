signature SIG =
sig
  type t 
  val x : t
end

functor F() : SIG = struct 
  type t = int ref
  val x = ref 0
end 

functor G () : SIG = struct
  structure E : SIG = F()
  open E
end 
