structure VectorUtil =
  struct
    local
      open Vector
    in
    fun listOfVector v = let
	  fun f (0, l) = l
	    | f (i, l) = f (i-1, Vector.sub(v, i-1) :: l)
	  in
	    f (Vector.length v, [])
	  end
    fun mapv f = let
	  fun mapv' #[a] = #[f a]
	    | mapv' #[a, b] = #[f a, f b]
	    | mapv' #[a, b, c] = #[f a, f b, f c]
	    | mapv' #[a, b, c, d] = #[f a, f b, f c, f d]
	    | mapv' v = vector (map f (listOfVector v))
	  in
	    mapv'
	  end
    end
 end;
