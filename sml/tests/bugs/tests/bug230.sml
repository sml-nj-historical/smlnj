(* bug230.sml *)

(* loops when printing value of p *)

signature FIELD =
sig
   type elem
   val zero: elem
   val one: elem
   val eq : elem * elem -> bool
   val + : elem * elem -> elem
   val * : elem * elem -> elem
   val ~ : elem -> elem
   exception Div
   val inv: elem -> elem
end

signature COMPLEX =
sig
   include FIELD
   val complex: real*real -> elem
   val - : elem * elem -> elem
   val / : elem * elem -> elem
   val re: elem -> real
   val im: elem -> real
   val abs : elem -> real
   val conj : elem -> elem
   val cis: real -> elem
end

structure Cartesian : COMPLEX =
struct
  open Real
  type elem = real * real
  fun complex ri = ri
  exception Div = (* Real. *) Div
  fun eq((a,b),(c,d)) = Real.==(a,c) andalso Real.==(b,d)
  val zero = (0.0,0.0) and one = (1.0,0.0)
  val op + = fn ((a,b),(c,d)) => (a+c,b+d)
  and op - = fn ((a,b),(c,d)) => (a-c,b-d)
  and op * = fn ((a,b),(c,d)) => (a*c-b*d, a*d+b*c)
  and op / = fn ((a,b),(c,d)) => 
		let val z = c*c+d*d
		 in ((a*c+b*d)/z, (b*c-a*d)/z)
		end
  and inv = fn (a,b) => let val z = a*a+b*b 
			 in (a/z,b/z) 
			end
  and ~ = fn (a,b) => (~a,~b)
  and re = fn(a,b) => a
  and im = fn(a,b) => b
  and abs = fn (a,b) => Math.sqrt(a*a+b*b)
  and conj = fn (a,b) => (a,~b)
  and cis = fn t => (Math.cos t, Math.sin t)
end

signature POLYNOMIAL =
sig
    structure F : FIELD
    type elem
    val degree: elem -> int
    val zero : elem
    val one : elem
    val x : elem
    val const : F.elem -> elem
    val poly : F.elem list -> elem  (* low-order 1st *)
    val coeff: elem -> F.elem list  (* low-order 1st *)
    val * : elem * elem -> elem
    val + : elem * elem -> elem
end

functor Polynomial(structure F : FIELD) : POLYNOMIAL =
struct
  structure F=F
  type elem = F.elem list
  val zero = nil
  val one = [F.one]
  fun poly nil = nil 
    | poly (a::r) = case poly r
		     of nil => if F.eq(a,F.zero) 
				then nil else [a]
		      | pr => a::pr
  fun coeff nil = [F.zero] | coeff p = p
  fun degree nil = 0
    | degree p = List.length(p)-1
  val x = [F.zero,F.one]
  fun const c = if F.eq(c,F.zero) then nil else [c]
  fun []+a = a
    | a+[] = a
    | (a::b) + (c::d) = 
	let val ac = F.+(a,c)
         in case b+d
	     of nil => if F.eq(ac,F.zero) 
			then nil else [ac]
	      | bd => ac::bd
        end

  fun scalarmult(a,[]) = []
    | scalarmult(a,b::c) = 
		let val ab = F.*(a,b)
	         in case scalarmult(a,c)
		     of nil => if F.eq(ab,F.zero) 
				then nil else [ab]
		      | ac => ab::ac
		end
  fun []*a = []
    | a*[] = []
    | (a::b)*c = scalarmult(a,c) + (F.zero::(b*c))

end

structure PC : POLYNOMIAL = Polynomial(structure F = Cartesian);

val xx = "before val p";
val p =
let open PC
 in x*x+x+x+const(Cartesian.complex(0.75,0.0))
end;
