functor F() =
  struct
    functor G(type t val y : t) = struct (* val x = y *) end
  end

functor H(type t) =
  struct
    functor I(val y:t) = struct (* val x = y *) end
  end

functor J(type t val y:t) =
  struct
    functor K(val z:t) = struct end
  end