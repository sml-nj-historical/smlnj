signature Index =
  sig
    type A
  end

signature Square =
  sig
    structure Source : Index
    structure Target : Index
    val top : Source.A -> Target.A
  end

signature FunctorSig =
  sig type 'a C
      val map : ('a -> 'b) -> 'a C -> 'b C
  end

functor MapIndex (structure F:FunctorSig) (structure In:Index) : Index =
  struct
    type A = In.A F.C
  end

functor MapSquare (structure G:FunctorSig) (structure Sq:Square) : Square =
  struct
    structure Source = MapIndex (structure F = G) (structure In = Sq.Source)
    structure Target = MapIndex (structure F = G) (structure In = Sq.Target)
    val top = G.map Sq.top
  end
