signature SIG = sig type u val y : u end

functor F(type t val x : t) =
struct 
  structure U : SIG = 
    struct
      datatype q = K of t 
      type u = q list
      val y = [K x]
    end
    val z = hd U.y
end

structure A = F(type t = int val x = 3);
