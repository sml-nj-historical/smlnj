functor F() =
struct
  datatype d = D
  structure A : sig val x : d end
   = struct val x = D end
end

