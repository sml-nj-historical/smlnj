(* bug922.sml *)
(* 922. failure of functor currying *)

signature Index =
	sig
		type A
		type I
		val index : A -> I
	end

signature Square =
	sig
		structure Source : Index
		structure Target : Index
		val top : Source.A -> Target.A
		val bottom : Source.I -> Target.I
	end

signature FunctorSig =
	sig
		type 'a C
		val map : ('a -> 'b) -> 'a C -> 'b C
	end

functor MapIndex (structure F:FunctorSig) (structure In:Index) : Index =
	struct
		type A = In.A F.C
		type I = In.I F.C
		val index = F.map In.index
	end

functor MapSquare (structure G:FunctorSig) (structure Sq:Square) : Square =
	struct
		structure Source = MapIndex (structure F = G) (structure In = Sq.Source)
		structure Target = MapIndex (structure F = G) (structure In = Sq.Target)
		val top = G.map Sq.top
		val bottom = G.map Sq.bottom
	end
