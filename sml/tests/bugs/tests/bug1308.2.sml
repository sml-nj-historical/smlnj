(* bug1308.2.sml *)

signature STO =
sig
  type store
  val update : (store -> 'a) -> 'a
end;

signature COMP =
sig
  type 'a lift
  val rap : 'a -> 'a lift
  val uplift : ('b lift) -> 'b lift
end;

functor F (structure S : STO
	   structure C : COMP)
: sig
    val x : S.store C.lift
  end =
struct
  val x = C.uplift (S.update (C.rap: S.store -> S.store C.lift))
end;

structure Sto : STO =
struct
  type store = string list * int list
  fun update f = f (["x"],[0])
end;

structure CPS : COMP =
struct
  type 'a lift = ('a -> Sto.store) -> Sto.store
  fun rap (v: 'a) (k: 'a -> Sto.store) = k v
  fun uplift (f: 'a lift) (k: 'a -> Sto.store) = f k
end;

structure R =
  F (structure S = Sto
     structure C = CPS);

val s2' = R.x (fn x => x);

