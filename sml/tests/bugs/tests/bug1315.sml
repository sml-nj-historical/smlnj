(* bug1315.sml *)

signature ASIG =
sig
  type g
  type n
  type 'a t
  val newG: unit -> g
  val newN: g -> n
  val nodes: g -> n list
  val empty: 'a t
  val add: 'a t * n * 'a -> 'a t
  val look: 'a t * n -> 'a option
  val id: n -> string
end;

structure A :> ASIG =
struct
  type g = (int * int list) ref
  type n = g * int
  type 'a t = (n * 'a) list

  fun newG () = ref (0, [])

  fun newN (g as ref (n, l)) = let
      val n' = n + 1
      val l' = n' :: l
      val _ = print (concat ["ID of new node is: ", Int.toString n', "\n"])
  in
      g := (n', l');
      (g, n')
  end

  fun nodes (g as ref (_, l)) = map (fn n => (g, n)) l
  val empty = []

  fun add (t, n, x) = (n, x) :: t

  fun look (t, (_, ni)) = let
      fun sameNode ((_, ni'), _) = ni = ni'
  in
      Option.map #2 (List.find sameNode t)
  end

  fun id (_, ni) = Int.toString ni
end;

signature BSIG =
sig
  structure A : ASIG
  datatype t = B of { g: A.g, nstring: A.n -> string }
  val mk: string list -> t
end;

structure B : BSIG =
struct
  structure A = A
  datatype t = B of { g: A.g, nstring: A.n -> string }

  fun mk sl = let
      val g = A.newG ()
      fun loop ([], t) =
	  let
	      fun nstring n =
		  valOf (A.look (t, n))
		  handle e => let
		      val _ = print "!!!! BOGUS exception... "
		  in
		      print (concat ["node ID is: ", A.id n, "\n"]);
		      raise e
		  end
	  in
	      B { g = g, nstring = nstring }
	  end
	| loop (s :: ss, t) = let
	      val n = A.newN g
	  in
	      loop (ss, A.add (t, n, s))
	  end
  in
      loop (sl, A.empty)
  end
end

(* -------------------------------------------------- *)
(* structure C = 			(* scenario 1 *) *)
functor C (B: BSIG) =		        (* scenario 2 *)
struct
  structure A = B.A

  fun show (B.B { g, nstring }) =
      app (fn n => print (nstring n ^ "\n")) (A.nodes g)

end

structure C = C (B)			(* scenario 2 *)
(* -------------------------------------------------- *)

(* run the sucker... *)
val test = C.show (B.mk ["a", "b", "c"])
