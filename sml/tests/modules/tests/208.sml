structure S = struct type t = int val x : t = 3 end
signature U = sig type t val x : t end

functor T() =
  struct local structure U :> U = S
         in
         val x = U.x
         end
  end;

functor F() = 
  struct local structure Y = T()
         in val y = Y.x
         end
  end;

structure A = F()

