(* bug1322.1.sml *)

signature MONAD = 
sig
  type 'a T
  val unit : 'a -> 'a T
  val bind : 'a T -> ('a -> 'b T) -> 'b T
end;

structure IdMonad =
struct
  type 'a T = 'a
  fun unit x = x
  fun bind m f = f m
end;

functor StMonad(structure Monad : MONAD type State) =
struct
  structure M =
  struct
    type 'a T = State -> (State * 'a) Monad.T
    fun unit x = fn s => Monad.unit (s, x)
    fun bind m k = fn s0 => Monad.bind (m s0) (fn (s1, a) => k a s1)
  end
  fun update f = fn s => Monad.unit (f s, s)
(* Works if you remove this defn *)
  val read = update (fn s : State => s)
end;

structure StMonad = StMonad(structure Monad = IdMonad type State = int);
