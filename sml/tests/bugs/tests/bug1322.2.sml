(* bug1322.2.sml *)

signature MONAD = 
sig
  type 'a T
  val unit : 'a -> 'a T
end;

structure IdMonad =
struct
  type 'a T = 'a
  fun unit x = x
end;

functor StMonad(structure Monad : MONAD type State) =
struct
  fun read (s: State) = Monad.unit(s,s) (* unit arg must be a pair *)
end;

structure StMonad = StMonad(structure Monad = IdMonad type State = int);
