signature SIG = sig end

functor F() :> SIG = struct end

functor G () = 
struct
  structure A = struct end
  structure CMSA = F (structure A = A)
end 

structure B = G()


