(* bug196.sml *)

signature COMPLEX =
sig
   type elem
   val complex: real*real -> elem
   val + : elem * elem -> elem
   val - : elem * elem -> elem
   val * : elem * elem -> elem
   val / : elem * elem -> elem
   val ~ : elem -> elem   
   val inv: elem -> elem
   val abs : elem -> real
   val conj : elem -> elem
   val cis: real -> elem
   val zero : elem
   val one : elem
end;

structure Complex :> COMPLEX =
struct
  open Real
  type elem = real * real
  fun complex ri = ri
  val zero = (0.0, 0.0)
  val one = (1.0, 0.0)
  val op + = fn ((a,b),(c,d)) => (a+c,b+d)
  and op - = fn ((a,b),(c,d)) => (a-c,b-d)
  and op * = fn ((a,b),(c,d)) => (a*c-b*d, a*d+b*c)
  and op / = fn ((a,b),(c,d)) => let val z = c*c+d*d
				  in ((a*c+b*d)/z, (b*c-a*d)/z)
				 end
  and inv = fn (a,b) => let val z = a*a+b*b in (a/z,b/z) end
  and ~ = fn (a,b) => (~a,~b)
  and abs = fn (a,b) => a*a+b*b
  and conj = fn (a,b) => (a,~b)
  and cis = fn t => (Math.cos t, Math.sin t)
end;

signature FIELD =
sig
   type elem
   val zero: elem
   val one: elem
   val + : elem * elem -> elem
   val * : elem * elem -> elem
   val ~ : elem -> elem
   val inv: elem -> elem
end;

signature POLYNOMIAL =
sig
    structure F : FIELD
    type poly
    val x : poly
    val const : F.elem -> poly
    val * : poly * poly -> poly
    val + : poly * poly -> poly
    val ~ : poly -> poly
    val eval: poly -> F.elem -> F.elem
    val deriv: poly -> poly
end;

functor Polynomial(F : FIELD) : POLYNOMIAL =
struct
  structure F=F
  type poly = F.elem list
  val x = [F.zero,F.one]
  fun const c = [c]
  fun []+a = a
    | a+[] = a
    | (a::b) + (c::d) = F.+(a,c)::(b+d)
  fun scalarmult(a,[]) = []
    | scalarmult(a,b::c) = F.*(a,b)::scalarmult(a,c)
  fun []*a = []
    | a*[] = []
    | (a::b)*c = scalarmult(a,c) + (F.zero::(b*c))
  fun ~ [] = []
    | ~ (a::b) = F.~(a) :: ~(b)
  fun eval p x =
    let fun f([],z,sum) = sum
          | f(a::b,z,sum) = f(b,F.*(x,z),F.+(sum,F.*(a,z)))
     in f(p,F.one,F.zero)
    end
  fun deriv [] = []
    | deriv (a::r) =
    let fun f(z,a::b) = F.*(z,a)::f(F.+(z,F.one),b)
     in f(F.one,r)
    end
end;

structure P = Polynomial(Complex);
