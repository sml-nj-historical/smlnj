(* 217.sml *)
(* A .83 bug in abstracting types in functor application (fixed in .85)   *
 * O. Nora (never acknowledged in masterbug because the original code was *
 * 1000 lines long                                                        *)

(* b origin is an application, b thinned version should look at this    *
 * origin not at the parameter as it did in .83                         *)

functor g (functor f ( ) : sig type d end) =
struct structure b:sig type d end = f() end;

structure c = g(functor f () = struct datatype d = C end);
