(* format-combinators.sml
 *
 *   Well-typed "printf" for SML, aka "Unparsing Combinators".
 *     This code was written by Matthias Blume (2002).  Inspiration
 *     obtained from Olivier Danvy's "Functional Unparsing" work.
 *
 * (C) 2002, Lucent Technologies, Bell Labs
 *
 *
 * Description:
 *
 * The idea is to use combinators for constructing something akin to
 * the format string of C's printf function.  The difference is, however,
 * that our formats aren't strings.  Instead, format( fragment)s have
 * meaningful types, and passing them to function "format" results
 * in a curried function whose arguments have precisely the types that
 * correspond to argument-consuming parts of the format.  (Such
 * argument-consuming parts are similar to the %-specifications of printf.)
 *
 * Here is how the typing works: There is an underlying notion of
 * "abstract formats" of type 'a format.  However, the user operates
 * at the level of "format fragments" which have type ('a, 'b)
 * fragment and are typically polymorphic in 'a (where 'b is
 * instantiated to some type containing 'a).  Fragments are
 * functions from formats to formats and can be composed freely using
 * the function composition operator 'o'.  This form of format
 * composition translates to a corresponding concatenation of the
 * resulting output.
 *
 * Fragments are composed from two kids of primitve fragments called
 * "elements" and "glue", respectively.  An "element" is a fragment that
 * consumes some argument (which thanks to the typing magic appears as a
 * curried argument when the format gets executed).  As "glue" we refer
 * to fragments that do not consume arguments but merely insert fixed
 * text (fixed at format construction time) into the output.
 *
 * There are also adjustment operations that pad, trim, or fit the output
 * of entire fragments (primitive or not) to a given size.
 *
 * A number of elements and some glue has been predefined.  Here are
 * examples on how to use this facility:
 *
 *  open FormatCombinators
 *
 *  format nothing                      ==> ""
 *
 *  format int                          ==> fn: int -> string
 *  format int 1234                     ==> "1234"
 *
 *  format (t"The square of " o int o t" is " o int o t".")
 *                                      ==> fn: int -> int -> string
 *  format (t"The square of " o int o t" is " o int o t".") 2 4
 *                                      ==> "The square of 2 is 4."
 *
 *  format (int o bool o char)          ==> fn : int -> bool -> char -> string
 *  format (int o bool o char) 1 true #"x"
 *                                      ==> "1truex"
 *
 *  format (glue string "glue vs. " o string o glue int 42 o sp 5 o int)
 *         "ordinary text " 17
 *                                      ==> "glue vs. ordinary text 42     17" 
 *
 * Fragments can be padded, trimmed, or fitted to generate text pieces of 
 * specified sizes.  Padding/trimming/fitting may be nested.
 * The operations are parameterized by a place (left, center, right) and
 * a width. Padding never shrinks strings, trimming never extends
 * strings, and fitting is done as necessary by either padding or trimming.
 * Examples:
 *
 *  format (pad left 6 int) 1234        ==> "  1234"
 *  format (pad center 6 int) 1234      ==> " 1234 "
 *  format (pad right 6 int) 1234       ==> "1234  "
 *  format (trim left 2 int) 1234       ==> "34"
 *  format (trim center 2 int) 1234     ==> "23"
 *  format (trim right 2 int) 1234      ==> "12"
 *  format (fit left 3 int) 12          ==> " 12"
 *  format (fit left 3 int) 123         ==> "123"
 *  format (fit left 3 int) 1234        ==> "234"
 *
 * Nesting:
 *
 *  format (pad right 20 (int o pad left 10 real) o t"x") 12 22.3
 *                                      ==> "12      22.3        x"
 *)
structure FormatCombinators :> FORMAT_COMBINATORS = struct

    type 'a format         = string list -> 'a
    type ('a, 'b) fragment = 'a format -> 'b format
    type 'a glue           = ('a, 'a) fragment
    type ('a, 't) element  = ('a, 't -> 'a) fragment

    type place = int * int -> int
    fun left (a, i)   = a - i
    fun center (a, i) = Int.quot (a - i, 2)
    fun right (a, i)  = 0

    local
	(* Generic padding, trimming, and fitting.  Nestability
	 * is achieved by remembering the current state s, passing
	 * a new empty one to the fragment, adjusting the output
	 * from that, and fitting the result back into the remembered
	 * state. ("States" are string lists and correspond to
	 * output coming from fragments to the left of the current point.) *)
	fun ptf adj pl n fr fm s = let
	    fun work s' = let
		val x' = concat (rev s')
		val sz = size x'
	    in
		adj (x', sz, n, pl (sz, n)) :: s
	    end
	in
	    (fr (fm o work)) []
	end

	fun pad0 (s, sz, n, off) =
	    StringCvt.padRight #" " n (StringCvt.padLeft #" " (sz - off) s)
	fun trim0 (s, _, n, off) = String.substring (s, off, n)
	fun pad1 (arg as (s, sz, n, _)) = if n < sz then s else pad0 arg
	fun trim1 (arg as (s, sz, n, _)) = if n > sz then s else trim0 arg
	fun fit1 (arg as (_, sz, n, _)) = (if n < sz then trim0 else pad0) arg
    in
	fun format' rcv fr   = fr (rcv o rev) []
        fun format fr        = format' concat fr

	fun using cvt fm x a = fm (cvt a :: x)

        fun int fm           = using Int.toString fm
	fun real fm          = using Real.toString fm
	fun bool fm          = using Bool.toString fm
	fun string fm        = using (fn x => x) fm
	fun string' fm       = using String.toString fm
	fun char fm          = using String.str fm
	fun char' fm         = using Char.toString fm

	fun int' rdx fm      = using (Int.fmt rdx) fm
	fun real' rfmt fm    = using (Real.fmt rfmt) fm

	fun pad place        = ptf pad1 place
	fun trim place       = ptf trim1 place
	fun fit place        = ptf fit1 place
    end

    fun glue e a fm x = e fm x a

    fun nothing fm    = fm
    fun t s           = glue string s
    fun sp n          = pad left n nothing
    fun nl fm         = t "\n" fm
    fun tab fm        = t "\t" fm
end
