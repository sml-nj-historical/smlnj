signature U = sig type t end

functor T(type s) =
  struct local structure A :> U = struct type t = s end
         in type t = A.t
         end
  end;

functor F(type s) = 
  struct structure Y = T(type s = s)
         datatype s = EE of Y.t
         val y = EE
  end;

structure A = F(type s = int)

