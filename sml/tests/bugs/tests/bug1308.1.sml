(* bug1308.1.sml *)

(* imp.sml -- modular implementation of Glynn Winskel's IMP language *)

(* ************************************************************ *)

structure Ast
= struct
  type ide = string
  datatype
    Aexp
    = LIT of int
    | VAR of ide
    | ADD of Aexp * Aexp
    | SUB of Aexp * Aexp
    | MUL of Aexp * Aexp
  and
    Bexp
    = TRUE
    | FALSE
    | EQ of Aexp * Aexp
    | LTE of Aexp * Aexp
    | NOT of Bexp
    | CUNJ of Bexp * Bexp
    | DISJ of Bexp * Bexp
  and
    Com
    = SKIP
    | ASSIGN of ide * Aexp
    | SEQ of Com * Com
    | COND of Bexp * Com * Com
    | WHILE of Bexp * Com
end

(* ************************************************************ *)

signature STO
= sig
    eqtype store
    val fetch : Ast.ide * store * (int -> 'a) * (string -> 'a) -> 'a
    val update : Ast.ide * int * store * (store -> 'a) * (string -> 'a) -> 'a
  end

structure Sto : STO
= struct
    type store = (Ast.ide * int) list

    fun fetch (x, s, succ, fail)
	= let fun loop []
		  = fail (x ^ ": undeclared identifier")
		| loop ((x', n) :: s)
		  = if x = x'
		    then succ n
		    else loop s
	  in loop s
	  end

    fun update (x, n, s, succ, fail)
        = let fun loop [] k
                  = fail (x ^ ": undeclared identifier")
                | loop ((x', n') :: s) k
                  = if x = x'
                    then k ((x, n) :: s)
                    else loop s (fn s' => k ((x', n') :: s'))
          in loop s succ
	  end
  end

structure Sto' : STO
= struct
    type store = Ast.ide list * int list

    exception SegmentationFault

    fun fetch (x, (xs, ns), succ, fail)
	= let fun loop ([], [])
		  = fail (x ^ ": undeclared identifier")
		| loop (x' :: xs, n :: ns)
		  = if x = x'
		    then succ n
		    else loop (xs, ns)
		| loop _
		  = raise SegmentationFault
	  in loop (xs, ns)
	  end

    fun update (x, n, s, succ, fail)
        = let fun loop ([], []) k
                  = fail (x ^ ": undeclared identifier")
                | loop (xs as (x' :: xs'), n' :: ns) k
                  = if x = x'
                    then k (xs, n :: ns)
                    else loop s (fn (xs, ns) => k (x' :: xs, n' :: ns))
		| loop _ _
		  = raise SegmentationFault
          in loop s succ
	  end
  end

(* ************************************************************ *)

signature COMPUTATION
= sig
    type 'a lift
    val VAL : 'a -> 'a lift
    val BOT : string -> 'a lift
    val oplift1 : ('a -> 'b) -> 'a lift -> 'b lift
    val oplift2 : ('a * 'b -> 'c) -> 'a lift * 'b lift -> 'c lift
    val uplift : ('a -> 'b lift) -> 'a lift -> 'b lift
  end

structure ComputationNaive : COMPUTATION
= struct
    type 'a lift = 'a

    exception UndeclaredIdentifier of string

    fun VAL v = v
    fun BOT msg = raise (UndeclaredIdentifier msg)

    fun oplift1 f c
	= f c

    fun oplift2 f (c1, c2)
	= f (c1, c2)

    fun uplift f c
	= f c
  end

structure ComputationDS : COMPUTATION
= struct
    datatype 'a lift = VAL of 'a
		     | BOT of string

    fun oplift1 f (VAL v)
	= VAL (f v)
      | oplift1 f (BOT msg)
	= (BOT msg)

    fun oplift2 f (VAL v1, VAL v2)
	= VAL (f (v1, v2))
      | oplift2 f (VAL v1, BOT s2)
	= BOT s2
      | oplift2 f (BOT s1, VAL v2)
	= BOT s1
      | oplift2 f (BOT s1, BOT s2)
	= BOT (s1 ^ " and " ^ s2)

    fun uplift f (VAL v)
	= f v
      | uplift f (BOT msg)
	= (BOT msg)
  end

signature ANSWER
= sig
    type result
    datatype answer = VAL of result
		    | BOT of string
    val k_init : result -> answer
  end

functor mkAnswer (S : STO)
: ANSWER
= struct
    type result = S.store
    datatype answer = VAL of result
		    | BOT of string
    val k_init = VAL
  end

functor mkComputationCPS (structure A : ANSWER)
: COMPUTATION
= struct
    type 'a lift = ('a -> A.answer) -> A.answer

    fun VAL v k = k v
    fun BOT msg k = A.BOT msg

    fun oplift1 f c k
	= c (fn v => k (f v))

    fun oplift2 f (c1, c2) k
	= c1 (fn v1 => c2 (fn v2 => k (f (v1, v2))))

    fun uplift f c k
	= c (fn v => f v k)
  end

(* ************************************************************ *)

functor Imp (structure S : STO
	     structure C : COMPUTATION)
: sig
    val A : Ast.Aexp -> S.store -> int C.lift
    val B : Ast.Bexp -> S.store -> bool C.lift
    val C : Ast.Com -> S.store -> S.store C.lift
  end
= struct
    open Ast

    fun A (LIT n) s
        = C.VAL n
      | A (VAR x) s
        = S.fetch (x, s, C.VAL, C.BOT)
      | A (ADD (a1, a2)) s
	= C.oplift2 (op +) (A a1 s, A a2 s)
      | A (SUB (a1, a2)) s
	= C.oplift2 (op -) (A a1 s, A a2 s)
      | A (MUL (a1, a2)) s
	= C.oplift2 (op * ) (A a1 s, A a2 s)
    and B TRUE s
        = C.VAL true
      | B FALSE s
        = C.VAL false
      | B (EQ (a1, a2)) s
	= C.oplift2 (op =) (A a1 s, A a2 s)
      | B (LTE (a1, a2)) s
	= C.oplift2 (op <=) (A a1 s, A a2 s)
      | B (NOT b) s
        = C.oplift1 not (B b s)
      | B (CUNJ (b1, b2)) s
	= C.oplift2 (fn (v1, v2) => v1 andalso v2) (B b1 s, B b2 s)
      | B (DISJ (b1, b2)) s
	= C.oplift2 (fn (v1, v2) => v1 orelse v2) (B b1 s, B b2 s)
    and C SKIP s
        = C.VAL s
      | C (ASSIGN (x, a)) s
        = C.uplift (fn v => S.update (x, v, s, C.VAL, C.BOT)) (A a s)
      | C (SEQ (c1, c2)) s
        = C.uplift (fn s => C c2 s) (C c1 s)
      | C (COND (b, c1, c2)) s
        = C.uplift (fn v => if v then C c1 s else C c2 s) (B b s)
      | C (WHILE (b, c)) s
	= let fun w true s
                  = C.uplift (fn s => C.uplift (fn v => w v s)
			  		       (B b s))
			     (C c s)
		| w false s
		  = C.VAL s
	  in C.uplift (fn v => w v s) (B b s)
          end
  end

(* ************************************************************ *)

structure ImpNaive
= Imp (structure S = Sto
       structure C = ComputationNaive)

structure ImpNaive'
= Imp (structure S = Sto'
       structure C = ComputationNaive)

structure ImpDS
= Imp (structure S = Sto
       structure C = ComputationDS)

structure ImpDS'
= Imp (structure S = Sto'
       structure C = ComputationDS)

structure Ans = mkAnswer (Sto)
structure ImpCPS
= Imp (structure S = Sto
       structure C = mkComputationCPS (structure A = Ans))

structure Ans' = mkAnswer (Sto')
structure ImpCPS'
= Imp (structure S = Sto'
       structure C = mkComputationCPS (structure A = Ans'))

(* ************************************************************ *)

val p0 = Ast.SKIP
val s0 = ImpNaive.C p0 []
val s0' = ImpNaive'.C p0 ([],[])

val p1 = Ast.ASSIGN ("x", Ast.LIT 42)
val s1 = ImpDS.C p1 [("x", 0)]
val s1' = ImpDS'.C p1 (["x"],[0])

val p2 = Ast.SEQ(Ast.ASSIGN ("x", Ast.LIT 42),
		 Ast.ASSIGN ("x", Ast.LIT 43))
val s2 = ImpCPS.C p2 [("x", 0)] Ans.k_init
val s2' = ImpCPS'.C p2 (["x"],[0]) Ans'.k_init

(* ************************************************************ *)
