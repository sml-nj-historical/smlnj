signature INT = sig type t=int end
structure ONE :> INT = struct type t=int end
structure TWO :> INT = struct type t=int end
functor F(structure A:INT   structure B:INT  sharing A=B) = struct end
structure THREE=F(structure A=ONE structure B=TWO)
