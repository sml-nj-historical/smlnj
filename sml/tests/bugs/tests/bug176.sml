(* bug.176 *)

signature INCLUDE_1 = sig  type Ty  val x: Ty  end
signature INCLUDE_2 = sig  type Ty  val y: Ty  end

signature BOTH =
  sig
    type T
    include INCLUDE_1 sharing type Ty = T
    include INCLUDE_2 sharing type Ty = T
  end

functor F(Both: BOTH) =
  struct
    val _ = [Both.x, Both.y]
  end;
