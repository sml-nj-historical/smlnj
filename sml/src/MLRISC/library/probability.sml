(* probability.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * A representation of probabilities for branch prediction.
 *)

signature PROBABILITY =
  sig

    type prob

    exception BadProb

    val never : prob	(* 0% probability *)
    val always : prob	(* 100% probability *)

    val prob : (int * int) -> prob
    val fromFreq : int list -> prob list

    val + : (prob * prob) -> prob
    val - : (prob * prob) -> prob
    val * : (prob * prob) -> prob
    val / : (prob * int) -> prob

    val percent : int -> prob

    val toReal : prob -> real
    val toString : prob -> string

  end

structure Probability :> PROBABILITY =
  struct

  (* Probabilities are represented as positive rationals.  Zero is
   * represented as PROB(0w0, 0w0) and one is represented as
   * PROB(0w1, 0w1).  There are several invariants about PROB(n, d):
   *	1) n <= d
   *	2) if n = 0w0, then d = 0w0 (uniqueness of zero)
   *	3) if d = 0w1, then n = 0w1 (uniqueness of one)
   *)
    datatype prob = PROB of (word * word)

    exception BadProb

    val never = PROB(0w0, 0w0)
    val always = PROB(0w1, 0w1)

  (* Fast GCD on words.  This algorithm is based on the following
   * observations:
   *	- If u and v are both even, then gcd(u, v) = 2*gcd(u/2, v/2)
   *	- If u is even and v is odd, then gcd(u, v) = gcd(u/2, v)
   *	- If both are odd, then gcd(u, v) = gcd(abs(u-v), v)
   *)
    fun gcd (u : word, v : word) = let
	  fun isEven x = (Word.andb(x, 0w1) = 0w0)
	  fun divBy2 x = Word.>>(x, 0w1)
	  fun lp1 (g, u, v) =
		if isEven(Word.orb(u, v))
		  then lp1 (Word.<<(g, 0w1), divBy2 u, divBy2 v)
		  else lp2 (g, u, v)
	  and lp2 (g, 0w0, v) = g*v
	    | lp2 (g, u, v) =
		if (isEven u) then lp2 (g, divBy2 u, v)
		else if (isEven v) then lp2 (g, u, divBy2 v)
		else if (u < v) then lp2 (g, u, divBy2(v-u))
		else lp2 (g, divBy2(u-v), v)
	  in
	    lp1 (0w1, u, v)
	  end

    fun normalize (0w0, _) = never
      | normalize (n, d) = (case Word.compare(n, d)
	   of LESS => (case gcd(n, d)
		 of 0w1 => PROB(n, d)
		  | g => PROB(Word.div(n, g), g)
		(* end case *))
	    | EQUAL => always
	    | GREATER => raise BadProb
	  (* end case *))
	    
    fun prob (n, d) =
	  if (n > d) orelse (n < 0) orelse (d <= 0)
	    then raise Domain
	    else normalize(Word.fromInt n, Word.fromInt d)

    fun add (PROB(n1, d1), PROB(n2, d2)) = normalize(d2*n1 + d1*n2, d1*d2)

    fun sub (PROB(n1, d1), PROB(n2, d2)) = let
	  val n1' = d2*n1
	  val n2' = d1*n2
	  in
	    if (n1' < n2') then raise BadProb else normalize(n1'-n2', d1*d2)
	  end

    fun mul (PROB(n1, d1), PROB(n2, d2)) = normalize (n1*n2, d1*d2)

    fun divide (PROB(n, d), m) = if (m <= 0)
	  then raise BadProb
	  else if (n = 0w0) then never
	  else normalize(d, n * Word.fromInt m)

    fun percent n =
	  if (n < 0) then raise BadProb
	  else normalize(Word.fromInt n, 0w100)

    fun fromFreq l = let
	  fun sum ([], tot) = tot
	    | sum (w::r, tot) = if (w < 0)
		then raise BadProb
		else sum(r, Word.fromInt w + tot)
	  val tot = sum (l, 0w0)
	  in
	    List.map (fn w => normalize(Word.fromInt w, tot)) l
	  end

    fun toReal (PROB(0w0, _)) = 0.0
      | toReal (PROB(_, 0w1)) = 1.0
      | toReal (PROB(n, d)) = let
	  fun toReal n = Real.fromLargeInt(Word.toLargeIntX n)
	  in
	    toReal n / toReal d
	  end

    fun toString (PROB(0w0, _)) = "0"
      | toString (PROB(_, 0w1)) = "1"
      | toString (PROB(n, d)) = let
	  val toStr = Word.fmt StringCvt.DEC
	  in
	    concat [toStr n, "/", toStr d]
	  end

    val op + = add
    val op - = sub
    val op * = mul
    val op / = divide

  end

