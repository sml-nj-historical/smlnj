(* bug59.sml *)

Compiler.Control.Print.signatures := 0;

signature UTILS = sig
  (* association lists *)
  type ('a,'b) assoclist 
  exception Lookup 
  val lookup: (''a,'b) assoclist -> ''a -> 'b
  val defined: (''a,'b) assoclist -> ''a -> bool
  val extend: (''a,'b) assoclist -> ''a -> 'b -> (''a,'b) assoclist
  val makeassoc: (''a * 'b) list -> (''a,'b) assoclist
  val applAssoc: (''a,'b) assoclist 
 		 -> ((''a*'b) -> (''a*'b)) 
		 -> (''a,'b) assoclist
  val reduceAssoc: (''a,'b) assoclist 
		-> ((''a*'b) -> 'c) 
		-> (('c*'c) -> 'c)
		-> 'c
		-> 'c
  val assocDomain: (''a,'b) assoclist -> ''a list
  val printAssoc: (''a,'b) assoclist 
	       -> (''a -> string)
	       -> ('b -> string)
	       -> string

  (* finite sets *)
  type ''a finset 
  val emptyset: ''a finset 
  val empty: ''a finset -> bool
  val inset: ''a finset -> ''a -> bool
  val add: ''a finset -> ''a -> ''a finset
  val iter: ''a finset -> (''a -> ''b) -> ''b finset
  val reduce: ''a finset -> (''a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'b
  val union: ''a finset -> ''a finset -> ''a finset 
  val singleton: ''a -> ''a finset
  val chooseOne: ''a finset -> (''a * ''a finset)
  val filterFinset: ''a finset -> (''a -> bool) -> ''a finset
  val sortFinset: ''a finset -> ((''a * ''a) -> bool) -> ''a list
  val printFinset: ''a finset -> (''a -> string) -> string

  (* list utilities *)
  val elementsOfBoth: ''a list -> ''a list -> ''a list
  val elementsOfFirstOnly: ''a list -> ''a list -> ''a list
  val elementsOfEither: ''a list -> ''a list -> ''a list
  val sort: (('a * 'a) -> bool) -> 'a list -> 'a list
  val maplist: 'a list -> ('a -> 'b) -> 'b list
  val reducelist: 'a list -> ('a -> 'b) -> ('b * 'c -> 'c) -> 'c -> 'c
  val listmember: ''a list -> ''a -> bool

  (* I/O *)
  val tab: string -> int -> string
  val write: string -> unit

  (* miscellaneous utility functions *)
  val id: 'a -> 'a

  (* global exceptions *)
  exception BcpBug
  exception NotImplemented
end (* UTILS *)


structure Utils : UTILS = struct

(* Insertion sort.  Creates O(n*n) garbage.  
   Adapted from the SML compiler sources. *)

fun listmember (l: ''a list) (e: ''a) : bool
  = let fun lm (nil)    = false
  	  | lm (e'::tl) = if (e = e') then true else (lm tl)
    in (lm l)
    end

fun maplist (l: 'a list) (f: 'a -> 'b) : 'b list 
  = let fun map (nil)   = nil
  	  | map (e::tl) = (f e)::(map tl)
    in (map l)
    end

fun sort (op > : ('a * 'a -> bool)) (l: 'a list)
   = let fun insert (a, nil) = [a]
 	   | insert (a, hd::tl) = if a>hd then hd::insert(a,tl)
					  else a::hd::tl
	 fun s (nil) = nil
	   | s (a::rest) = insert (a, s rest)
      in s l
     end

fun elementsOfBoth (l1: ''a list) (l2: ''a list) : ''a list
  = let fun eob (nil) = nil
          | eob (e1::tl1) = if (listmember l2 e1) 
	  		       then e1::(eob tl1)
			       else (eob tl1)
    in (eob l1)
    end

fun elementsOfFirstOnly (l1: ''a list) (l2: ''a list) : ''a list
  = let fun eofo (nil) = nil
          | eofo (e1::tl1) = if (not (listmember l2 e1))
	  		        then e1::(eofo tl1)
			        else (eofo tl1)
    in (eofo l1)
    end

fun elementsOfEither (l1: ''a list) (l2: ''a list) : ''a list
  = let fun eoi (nil) = l2
          | eoi (e1::tl1) = if (not (listmember l2 e1))
	  		       then e1::(eoi tl1)
			       else (eoi tl1)
    in (eoi l1)
    end

fun reducelist  (l:'a list) 
		(eachfn: 'a -> 'b) 
		(combinefn:('b*'c) -> 'c)
		(e:'c)
		       : 'c
  = case l of
	(first::rest) => 
	   (combinefn (eachfn first, reducelist rest eachfn combinefn e))
     |  ([])  => 
	   e

(* ---------- *)

type ('a,'b) assoclist = ('a * 'b) list

exception Lookup 

fun lookup (l:(''a,'b) assoclist) (k:''a) =
    case l of
	((k',v)::rest) => 
	   if k = k'
	      then v
	      else lookup rest k
     |  ([])  => 
	   raise Lookup

fun defined (l:(''a,'b) assoclist) (k:''a) =
    case l of
	((k',v)::rest) => 
	   if k = k'
	      then true
	      else defined rest k
     |  ([])  => 
	   false

fun extend (l:(''a,'b) assoclist) (k:''a) (v:'b) =
    case l of 
	(l') => (k,v)::l'

fun makeassoc (l: (''a * 'b) list) =
    (l)

fun applAssoc (l:(''a,'b) assoclist) (f:(''a*'b) -> (''a*'b)) =
    case l of
	(first::rest) => 
	   (f first)::rest
     |  ([])  => 
	   ([])

fun assocDomain (l:(''a,'b) assoclist) : ''a list
  = let fun ad (nil)       = nil
  	  | ad ((a,b)::tl) = a::(ad tl)
    in (ad l)
    end

fun printAssoc (l:(''a,'b) assoclist) (fl:''a -> string) (fr: 'b -> string)
  = let fun pelem (l,r) = (fl l) ^ "=>" ^ (fr r)
        fun p ([])    = ""
          | p ([e])   = (pelem e)
          | p (e::tl) = (pelem e) ^ ", " ^ (p tl)
    in "[" ^ (p l) ^ "]"
    end

fun reduceAssoc (l:(''a,'b) assoclist) 
		(f:(''a*'b) -> 'c) 
		(g:('c*'c) -> 'c)
		(e:'c)
  = case l of
	(first::rest) => 
	   (g (f first, reduceAssoc rest f g e))
     |  ([])  => 
	   e

(* ---------- *)

type ''a finset = ''a list

exception EmptySet

val emptyset: ''a finset = []

fun inset (s: ''a finset) (e: ''a) =
    let fun f ([]) e = false
	  | f (e'::tl) e = if e = e' then true else f tl e
    in f s e
    end

fun add s e =
    if (inset s e) then s else (e::s)

fun iter (s: ''a finset) (f: ''a -> ''b) : ''b finset =
    let fun i ([]) f = []
	  | i (e::tl) f = 
	        let val s' = i tl f
		    val e' = f e
		in 
		    if inset s' e' then s' else (e'::s')
		end
    in i s f
    end

fun reduce (s: ''a finset)
	   (f: ''a -> 'b) 
	   (g: 'b -> 'b -> 'b)
	   (e: 'b) : 'b 
  = case s of
	(first::rest) => 
	   (g (f first) (reduce rest f g e))
     |  ([])  => 
	   e

fun union (s1:''a finset) (s2:''a finset) : ''a finset =
    let fun u ([]) s2 = s2
          | u (e1::tl1) s2 = if (inset s2 e1) 
			     then (u tl1 s2) 
			     else (u tl1 (e1::s2))
    in u s1 s2
    end

fun singleton e = [e]

fun empty s = (s = nil)

fun chooseOne (s: ''a finset) =
    case s of
	([]) =>      raise EmptySet
      | (hd::tl) => (hd,tl)

fun filterFinset (s: ''a finset) (pred: ''a -> bool) : ''a finset
  = let fun f (nil)   = nil
          | f (a::tl) = if (pred a) then a::(f tl) else (f tl)
    in f s
    end

fun sortFinset (s: ''a finset) (gt: ''a * ''a -> bool) : ''a list =
    sort gt s

fun printFinset (s:''a finset) (f: ''a -> string) =
    let fun p ([]) f = ""
	  | p ([e]) f = f e
	  | p (e::tl) f = (f e) ^ ", " ^ (p tl f)
    in "{" ^ (p s f) ^ "}"
    end
    

(* ---------- *)


fun tab s 0 = ""
  | tab s n = s ^ (tab s (n-1))

fun write (s: string) : unit 
  = let val dummy = print s
    in ()
    end


(* ---------- *)

fun id x = x

(* ---------- *)


exception BcpBug

exception NotImplemented

end (* UTILS *)

signature TYPEVAR = sig
  structure Utils: UTILS

  type t

  val tv: string -> t
  val eq: t -> t -> bool

  val newUniqueVar: t -> string -> (t Utils.finset) -> t
  val makefresh: unit -> t

    val tostring: t -> string
end (* TYPEVAR *)



structure Typevar : TYPEVAR = struct
  structure Utils = Utils
  open Utils

type t = string

fun tv s = s

fun eq t1 t2 = (t1 = t2)

fun newUniqueVar (v: t) (suffix: string) (dontUse: t finset) 
		 : t 
    = let val v' = (v ^ suffix)
      in
	  if (inset dontUse v')
	     then (newUniqueVar v' "'" dontUse)
	     else v'
      end
      
val nextFreshIndex = ref 0

fun makefresh() : t
  = (nextFreshIndex := !nextFreshIndex + 1;
    "V" ^ (Int.toString (!nextFreshIndex)))

fun tostring (tv:t) = tv

end (* Typevar *)

signature GNDVAR = sig
  type t

  val gv: string -> t
  val eq: t -> t -> bool
  val tostring: t -> string
end (* GNDVAR *)



structure Gndvar : GNDVAR = struct
  structure Utils = Utils
  open Utils

type t = string

fun gv s = s

fun eq t1 t2 = (t1 = t2)

fun tostring (v:t) =
    v

end (* Gndvar *)
signature TERM = sig
  structure Gndvar: GNDVAR

  datatype t =
		Var of Gndvar.t
	      | Appl of t * t
	      | Lambda of Gndvar.t * t

  val isVar: t -> bool
  val isAppl: t -> bool
  val isLambda: t -> bool
  
  val asGndvar: t -> Gndvar.t
  
  val tostring: t -> string
end (* TERM *)



structure Term : TERM = struct
  structure Utils = Utils
  structure Gndvar = Gndvar
  open Utils

datatype t =    Var of Gndvar.t
	      | Appl of t * t
	      | Lambda of Gndvar.t * t

fun isVar(Var(v)) = true
  | isVar (_)	  = false 

fun isAppl (Appl(t1,t2)) = true
  | isAppl (_)	         = false 

fun isLambda (Lambda(v,t2)) = true
  | isLambda (_)            = false 

exception NotAVar

fun asGndvar (Var(v)) = v
  | asGndvar (_)      = raise NotAVar

fun tostring (x:t) : string 
  = let fun p x needsParens =
        case x of
	  (Var(v)) =>  Typevar.tostring v
	| (Appl(t1,t2)) => (if needsParens then "(" else "")
		         ^ (p t1 false) ^ " " 
			 ^ (p t2 true)
			 ^ (if needsParens then ")" else "")
	| (Lambda(v,t1)) => (if needsParens then "(" else "")
		          ^ "\\" 
		          ^ (Gndvar.tostring v) ^ ". " 
			  ^ (p t1 false)
			  ^ (if needsParens then ")" else "")
    in p x false
    end

end (* Term *)

signature TP = sig
  structure Typevar: TYPEVAR
  structure Utils: UTILS

  datatype t =
		Omega
	      | TVar of Typevar.t
	      | Arrow of t * t
	      | Intersect of t * t


  val isOmega: t -> bool
  val isTvar:  t -> bool
  val isArrow: t -> bool
  val isInter: t -> bool

  val asTvar: t -> Typevar.t

  val tsize: t -> int

  (* occursIn t s == "s occurs in t" *)
  val occursIn: t -> t -> bool 
  val smashMatchingSubphrases: t -> t -> t -> t

  val allSubphrases: t -> t Utils.finset
  val allTypevars: t -> Typevar.t Utils.finset
  val arrowSubphrases: t -> t Utils.finset
  val nonInterSubphrs: t -> t Utils.finset

  val tostring: t -> string
end (* TP *)



structure Tp : TP = struct
  structure Typevar = Typevar
  structure Utils = Utils
  open Utils

datatype t =
		Omega
	      | TVar of Typevar.t
	      | Arrow of t * t
	      | Intersect of t * t



fun isOmega(Omega) = true
  | isOmega(_)     = false

fun isTvar(TVar(x)) = true
  | isTvar(x)       = false

fun isArrow(Arrow(_,_)) = true
  | isArrow(_)          = false

fun isInter(Intersect(_,_)) = true
  | isInter(_)              = false


exception NotATVar

fun asTvar (TVar(v)) = v
  | asTvar (_)       = raise NotATVar


fun tsize (t: t) : int =
    case t of
        (Omega) =>            1
      | (TVar(v)) =>          1
      | (Arrow(t1,t2)) =>     (tsize t1) + (tsize t2) + 1
      | (Intersect(t1,t2)) => (tsize t1) + (tsize t2) + 1


fun occursIn (t: t) (match: t) : bool
  = if t = match 
       then true
       else case t of
         (Omega) =>            false
       | (TVar(v)) =>          false
       | (Arrow(t1,t2)) =>     (occursIn t1 match) orelse (occursIn t2 match)
       | (Intersect(t1,t2)) => (occursIn t1 match) orelse (occursIn t2 match)


fun smashMatchingSubphrases (t: t) (match: t) (repl: t)
  = if t = match 
       then repl
       else case t of
         (Omega) =>            t
       | (TVar(v)) =>          t
       | (Arrow(t1,t2)) =>     Arrow (smashMatchingSubphrases t1 match repl,
			              smashMatchingSubphrases t2 match repl)
       | (Intersect(t1,t2)) => Intersect 
				(smashMatchingSubphrases t1 match repl,
			         smashMatchingSubphrases t2 match repl)
		

fun allSubphrases (tp:t) : t finset =
    case tp of
      (Omega) =>            singleton(tp)
    | (TVar(v)) =>          singleton(tp)
    | (Arrow(t1,t2)) =>     union (singleton tp)
			          (union(allSubphrases t1) (allSubphrases t2))
    | (Intersect(t1,t2)) => union (singleton tp)
			          (union(allSubphrases t1) (allSubphrases t2))

fun allTypevars (tp:t) : Typevar.t finset =
    case tp of
      (Omega) =>            emptyset
    | (TVar(v)) =>          singleton(v)
    | (Arrow(t1,t2)) =>     union (allTypevars t1) (allTypevars t2)
    | (Intersect(t1,t2)) => union (allTypevars t1) (allTypevars t2)

fun arrowSubphrases (tp:t) : t finset =
    case tp of
      (Omega) =>            emptyset
    | (TVar(v)) =>          emptyset
    | (Arrow(t1,t2)) =>     union (singleton tp)
			          (union(arrowSubphrases t1) 
				        (arrowSubphrases t2))
    | (Intersect(t1,t2)) => (union(arrowSubphrases t1) (arrowSubphrases t2))

fun nonInterSubphrs (tp:t) : t finset =
    case tp of
      (Omega) =>            singleton(tp)
    | (TVar(v)) =>          singleton(tp)
    | (Arrow(t1,t2)) =>     union (singleton tp)
			          (union(nonInterSubphrs t1) 
					(nonInterSubphrs t2))
    | (Intersect(t1,t2)) => union (nonInterSubphrs t1) 
				  (nonInterSubphrs t2)

fun tostring t =
    let fun pts t arrowNeedsParens intersectNeedsParens =
      case t of
        (Omega) =>            "Omega"
      | (TVar(v)) =>          Typevar.tostring v
      | (Arrow(t1,t2)) =>     (if arrowNeedsParens then "(" else "")
			    ^ (pts t1 true true)
			    ^ "->"
			    ^ (pts t2 false true)
			    ^ (if arrowNeedsParens then ")" else "")
      | (Intersect(t1,t2)) => (if intersectNeedsParens then "(" else "")
			    ^ (pts t1 true true)
			    ^ "&"
			    ^ (pts t2 true true)
			    ^ (if intersectNeedsParens then ")" else "")
    in pts t false false
    end

(* The version below looks nicer, but you can't tell what the structure of
   an intersection type is by looking at the way it prints, which can
   be confusing.

fun tostring t =
    let fun pts t arrowNeedsParens intersectNeedsParens =
      case t of
        (Omega) =>            "Omega"
      | (TVar(v)) =>          Typevar.tostring v
      | (Arrow(t1,t2)) =>     (if arrowNeedsParens then "(" else "")
			    ^ (pts t1 true true)
			    ^ "->"
			    ^ (pts t2 false true)
			    ^ (if arrowNeedsParens then ")" else "")
      | (Intersect(t1,t2)) => (if intersectNeedsParens then "(" else "")
			    ^ (pts t1 true false)
			    ^ "&"
			    ^ (pts t2 true false)
			    ^ (if intersectNeedsParens then ")" else "")
    in pts t false false
    end
*)

end (* Tp *)

signature BASIS = sig
  structure Gndvar: GNDVAR
  structure Tp: TP

  type t
  val emptybasis: t
  val makebasis: (Gndvar.t * Tp.t) list -> t
  val lookupbasis: t -> Gndvar.t -> Tp.t
  val basisdefined: t -> Gndvar.t -> bool
  val extendbasis: t -> Gndvar.t 
		      -> Tp.t -> t 
  val removeBinding: t -> Gndvar.t -> t 
  val map: t -> ((Gndvar.t*Tp.t) -> (Gndvar.t*Tp.t)) -> t
  val combine: t -> t -> t
  val allSubphrases: t -> Tp.t Utils.finset

  val tostring: t -> string
end (* BASIS *)




structure Basis : BASIS = struct
  structure Utils = Utils
  structure Gndvar = Gndvar
  structure Tp = Tp
  open Utils

type t = (Gndvar.t,Tp.t) assoclist

val emptybasis: t = 
    makeassoc []

fun makebasis (init:(Gndvar.t * Tp.t) list): t = 
    makeassoc init

fun lookupbasis (bas:t) (v:Gndvar.t) : Tp.t =
    lookup bas v

fun basisdefined (bas:t) (v:Gndvar.t) =
    defined bas v

fun extendbasis (bas:t) (v:Gndvar.t) (tp:Tp.t) =
    extend bas v tp

fun removeBinding (bas:t) (v:Gndvar.t) : t 
  = let fun r (nil)    = nil
          | r ((v',tp)::tl) = if (v=v') then tl else (v',tp)::(r tl)
    in r bas
    end 

fun map (b:t) (f:(Gndvar.t*Tp.t) -> (Gndvar.t*Tp.t)) 
    = applAssoc b f

fun combine (b1:t) (b2:t) : t 
  = let    val (b1d: Gndvar.t list) =     assocDomain b1
           val (b2d: Gndvar.t list) =     assocDomain b2
    in let val (justb1d: Gndvar.t list) = elementsOfFirstOnly b1d b2d
    	   val (justb2d: Gndvar.t list) = elementsOfFirstOnly b2d b1d
	   val (bothd: Gndvar.t list) =   elementsOfBoth b1d b2d
    in let fun copyout b v = (v,(lookupbasis b v))
    in 
    	makebasis (  (maplist justb1d (fn v => copyout b1 v))
		   @ (maplist justb2d (fn v => copyout b2 v))
		   @ (maplist bothd (fn v => (v, (Tp.Intersect 
		   				   (lookupbasis b1 v,
					            lookupbasis b2 v))))))
    end end end

fun allSubphrases (b: t) : Tp.t finset
  = let fun u(s1,s2) = union s1 s2
    in reduceAssoc b 
  	  	   (fn (v,tp) => Tp.allSubphrases tp)
		   u
		   emptyset
    end

fun tostring b =
      "["
    ^ (reduceAssoc b
		  (fn (v,tp) => Gndvar.tostring(v) ^ "=" ^ Tp.tostring(tp))
		  (fn (s1,s2) => s1 ^ (if s2="" then "" else ", ") ^ s2)
		  "")
    ^ "]"

end (* Basis *)

signature PAIR = sig
  structure Tp: TP
  structure Basis: BASIS

  type t 
  
  val makePair: Basis.t -> Tp.t -> t
  val destruct: t -> (Basis.t * Tp.t)
  val extractBasis: t -> Basis.t
  val extractTp: t -> Tp.t 
  val closure: t -> t -> (Tp.t * Tp.t * Tp.t)
  val tostring: t -> string
  val tostringpp: t -> int -> string
end (* PAIR *)




structure Pair : PAIR = struct
  structure Tp = Tp
  structure Basis = Basis
  structure Utils = Utils
  open Utils

type t = (Basis.t * Tp.t)

fun makePair b tp = (b,tp)

fun destruct (b,tp) = (b,tp)

fun extractBasis (b,tp) = b

fun extractTp (b,tp) = tp

fun closure (b1,pi1) (b2,pi2) : (Tp.t * Tp.t * Tp.t)
  = let val allsubjects = elementsOfEither (assocDomain b1) (assocDomain b2)
        fun alpha v = if (Basis.basisdefined b1 v) 
			 then (Basis.lookupbasis b1 v)
			 else Tp.Omega
        fun beta v  = if (Basis.basisdefined b2 v) 
			 then (Basis.lookupbasis b2 v)
			 else Tp.Omega
    in let fun sigma v = Tp.Intersect(alpha v, Tp.TVar(Typevar.makefresh()))
           fun tau v =   Tp.Intersect(Tp.TVar(Typevar.makefresh()), beta v)
    in let val phi = Tp.TVar(Typevar.makefresh())
    in let val s = reducelist allsubjects 
			      (fn v => Tp.TVar(v)) 
			      (fn (lhs,rhs) => Tp.Arrow(lhs,rhs))
			      pi1
           val t = reducelist allsubjects 
			      (fn v => Tp.TVar(v)) 
			      (fn (lhs,rhs) => Tp.Arrow(lhs,rhs))
			      (Tp.Arrow(pi2,phi))
    in (s,t,phi)
    end end end end

fun tostring (b,tp) =
      "<"
    ^ (Basis.tostring b)
    ^ ", "
    ^ (Tp.tostring tp)
    ^ ">"

fun tostringpp (b,tp) indent =
      (tab " " indent) ^ "<"
    ^ (Basis.tostring b)
    ^ ",\n"
    ^ (tab " " indent) ^ (Tp.tostring tp)
    ^ ">\n"

end (* Pair *)

signature SUBST = sig
  structure Typevar: TYPEVAR
  structure Tp: TP
  structure Pair: PAIR
  structure Utils: UTILS

  type t 
  val makesubst: (Typevar.t * Tp.t) list -> t
  val lookupSubst: t -> Typevar.t -> Tp.t
  val appl: t -> Tp.t -> Tp.t
  val applB: t -> Basis.t -> Basis.t
  val applP: t -> Pair.t -> Pair.t
  val substOmegaForAll: Typevar.t Utils.finset -> t
  val tostring: t -> string
  val renameVars: Typevar.t Utils.finset 
	          -> string 
		  -> Typevar.t Utils.finset
		  -> (Typevar.t,Tp.t) Utils.assoclist
end (* SUBST *)



structure Subst : SUBST = struct
  structure Typevar = Typevar
  structure Tp = Tp
  structure Basis = Basis
  structure Pair = Pair
  structure Utils = Utils
  open Utils

type t = (Typevar.t,Tp.t) assoclist

exception Lookup

fun makesubst (init): t = 
    makeassoc init

fun substOmegaForAll (vars: Typevar.t finset) : t
  = if empty(vars)
       then []
       else let val (v,rest) = (chooseOne vars)
	    in (v,Tp.Omega)::(substOmegaForAll rest)
	    end

fun lookupSubst (s:t) (tv:Typevar.t) =
    lookup s tv

fun substDefined (s:t) (tv:Typevar.t) =
    defined s tv

fun appl (s:t)(tp:Tp.t): Tp.t =
    case tp of
      (Tp.Omega) =>            Tp.Omega
    | (Tp.TVar(v)) =>          if substDefined s v
			         then lookupSubst s v
				 else Tp.TVar(v)
    | (Tp.Arrow(t1,t2)) =>     Tp.Arrow(appl s t1, appl s t2)
    | (Tp.Intersect(t1,t2)) => Tp.Intersect(appl s t1, appl s t2)
	
fun applB (s:t) (b: Basis.t): Basis.t =
    Basis.map b (fn (tv,tp) => (tv,appl s tp))

fun applP (s:t) ((b,tau): Pair.t): Pair.t =
    (Basis.map b (fn (tv,tp) => (tv,appl s tp)),
     appl s tau)

fun renameVars (ts: Typevar.t finset) 
	       (suffix: string) 
	       (dontUse: Typevar.t finset) 
	       : t 
  = if (empty ts) 
       then makeassoc []
       else let val (v,ts') = (chooseOne ts)
		val rest = (renameVars ts' suffix dontUse)
	        val newV = Typevar.newUniqueVar v suffix dontUse
	    in
		extend rest v (Tp.TVar newV)
	    end

fun tostring s = "subst " ^ (printAssoc s Typevar.tostring Tp.tostring)

end (* Subst *)

signature EXPANSION = sig
  structure Typevar: TYPEVAR
  structure Tp: TP
  structure Basis: BASIS
  structure Pair: PAIR
  structure Utils: UTILS

  type t
  (* val completion: Tp.t Utils.finset -> Tp.t Utils.finset
     val computeLmuA: Tp.t -> Tp.t Utils.finset -> Tp.t Utils.finset *)
  val makeExpansion:  Tp.t Utils.finset -> Tp.t -> t
  val appl: t -> Tp.t -> Tp.t
  val applB: t -> Basis.t -> Basis.t
  val applP: t -> Pair.t -> Pair.t
  val expand: Tp.t Utils.finset -> Tp.t -> Tp.t -> Tp.t
  val tostring: t -> string
end (* EXPANSION *)



structure Expansion : EXPANSION = struct
  structure Typevar = Typevar
  structure Tp = Tp
  structure Basis = Basis
  structure Pair = Pair 
  structure Utils = Utils
  open Utils


fun completion (s: Tp.t finset) : Tp.t finset =
    reduce s Tp.allSubphrases union emptyset

fun computeLmuA (mu: Tp.t) (A: Tp.t finset) : Tp.t finset =
    let val checkall = (reduce A Tp.arrowSubphrases union emptyset) 
    in let fun expandL L check =
	if (empty check)
          then L
	  else let val (sigma,check') = (chooseOne check)
	       in 
		 if (inset L sigma)
		    then expandL L check'
		    else case sigma of
		      (Tp.Arrow(nu,(Tp.Intersect(delta,alpha)))) =>
			 if (inset L delta) orelse (inset L alpha)
			    then expandL (union L (Tp.nonInterSubphrs sigma))
					 checkall
			    else expandL L check'
		    | (Tp.Arrow(nu,delta)) =>
			 if (inset L delta)
			    then expandL (union L (Tp.nonInterSubphrs sigma))
					 checkall
			    else expandL L check'
		    | (x) => raise BcpBug
	       end
    in
	(expandL (Tp.nonInterSubphrs mu) checkall)
    end end

fun sortLmuA (LmuA: Tp.t finset) : Tp.t finset =
    let fun gt (t1:Tp.t, t2:Tp.t) = (Tp.tsize t1) < (Tp.tsize t2)
    in 
	sortFinset LmuA gt
    end

type t = Tp.t finset * Tp.t

fun makeExpansion  (A: Tp.t finset) 
  	           (mu: Tp.t) : t
  = (A,mu)

fun appl (exp: t)
		   (tp: Tp.t)
	              : Tp.t
  = let    val (A,mu) = exp
    in let val LmuA  = (computeLmuA mu A)
    in let val L'muA = (sortLmuA LmuA)
    in let val I     = (reduce LmuA Tp.allTypevars union emptyset)
	   val Avars = (reduce A Tp.allTypevars union emptyset)
    in let val sf    = (Subst.renameVars I "1" Avars)
	   val sg    = (Subst.renameVars I "2" Avars)
    in let fun smash tp match = Tp.smashMatchingSubphrases  tp match 
				    (Tp.Intersect (Subst.appl sf match,
				      		   Subst.appl sg match) )
	   fun dosubst tp nil = tp
	     | dosubst tp (nu::rest) = dosubst (smash tp nu) rest
    in  let val dummy = (*write*)("LmuA: " 
			       ^  (printFinset LmuA Tp.tostring) 
			       ^  "\n"
			       ^  "I    :"
			       ^  "\n"
			      )
    in
	dosubst tp L'muA
    end
    end end end end end end

fun expand A mu tau = appl (makeExpansion A mu) tau


exception NotInExpansionsDomain

fun checkDefinedOnBasis ((A,mu): t) (B: Basis.t) : unit 
  = let    val bangA = (completion A)
    in let val dummy = Basis.map B 
			    (fn (tv,tp) => 
				    if (inset bangA tp) 
			 	       then (tv,tp)
	    			       else raise NotInExpansionsDomain)
    in ()
    end end


fun applB (exp: t) (B: Basis.t) : Basis.t
  = let val dummy = (checkDefinedOnBasis exp B)
    in (Basis.map B (fn (tv,tp) => (tv,appl exp tp)))
    end


fun checkDefinedOnPair ((A,mu): t) ((B,tau): Pair.t) : unit 
  = let    val bangA = (completion A)
    in let val dummy = if (not (inset bangA tau))
                          then raise NotInExpansionsDomain
	                  else (Basis.map 
			          B 
				  (fn (tv,tp) => 
				      if (inset bangA tp) 
				         then (tv,tp)
					 else raise NotInExpansionsDomain))
    in ()
    end end

fun applP (exp: t)
		    ((B,tau): Pair.t)
	              : Pair.t
  = let val dummy = (checkDefinedOnPair exp (B,tau))
    in (Basis.map B (fn (tv,tp) => (tv,appl exp tp)),
        appl exp tau)
    end


fun tostring (A,mu)     = "exp(" 
		          ^ (printFinset A Tp.tostring) 
		          ^ ", " 
		          ^ (Tp.tostring mu) 
		          ^ ")"

end (* Expansion *)

signature LIFT = sig
  structure Typevar: TYPEVAR
  structure Tp: TP
  structure Basis: BASIS
  structure Utils: UTILS

  type t
  val tostring: t -> string
  val appl: t -> Tp.t -> Tp.t
  val applB: t -> Basis.t -> Basis.t
  val applP: t -> Pair.t -> Pair.t
end (* LIFT *)



structure Lift : LIFT = struct
  structure Typevar = Typevar
  structure Tp = Tp
  structure Basis = Basis
  structure Utils = Utils
  structure Pair = Pair
  open Utils

type t = unit

fun tostring l = "<lift>"

fun appl (l:t) (tp: Tp.t) = raise NotImplemented

fun applB (l:t) (B: Basis.t) = raise NotImplemented

fun applP (l:t) ((B,tau): Pair.t) = raise NotImplemented

end (* Lift *)

signature CHAIN = sig
  structure Typevar: TYPEVAR
  structure Tp: TP
  structure Basis: BASIS
  structure Subst: SUBST
  structure Expansion: EXPANSION
  structure Lift: LIFT
  structure Utils: UTILS

  type t
  val emptyChain: t
  val substChain: Subst.t -> t
  val liftChain: Lift.t -> t
  val expChain: Expansion.t -> t
  val concat: t -> t -> t
  val tostring: t -> string
  val tostringpp: t -> int -> string
  val appl: t -> Tp.t -> Tp.t
  val applB: t -> Basis.t -> Basis.t
  val applP: t -> Pair.t -> Pair.t
end (* CHAIN *)



structure Chain : CHAIN = struct
  structure Typevar = Typevar
  structure Tp = Tp
  structure Basis = Basis
  structure Subst = Subst
  structure Expansion = Expansion
  structure Lift = Lift
  structure Utils = Utils
  open Utils

datatype link =  Lift of Lift.t
	       | Expansion of Expansion.t
	       | Subst of Subst.t

type t = link list

val emptyChain = []

fun expChain (e:Expansion.t) = [Expansion(e)]

fun liftChain (l:Lift.t) = [Lift(l)]

fun substChain (s:Subst.t) = [Subst(s)]

fun concat (c1: t) (c2: t) : t = c1 @ c2

fun tostring (c: t) : string  =
  let fun printLink l 
          = case l of 
               (Lift(l)) => Lift.tostring(l)
             | (Expansion(e)) => Expansion.tostring(e)
             | (Subst(s)) => Subst.tostring(s)
  in case c of
       ([]) =>  ""
     | ([l]) => printLink(l)
     | (l::tl) => printLink(l) ^ ";" ^ tostring(tl)
  end

fun tostringpp (c: t) (indent: int) : string  =
  let fun printLink l 
          = case l of 
               (Lift(l)) => Lift.tostring(l)
             | (Expansion(e)) => Expansion.tostring(e)
             | (Subst(s)) => Subst.tostring(s)
  in case c of
       ([]) =>  ""
     | ([l]) => (tab " " indent) ^ printLink(l) ^ "\n"
     | (l::tl) => (tab " " indent) ^ printLink(l) 
		^ ";\n" ^ (tostringpp tl indent)
  end

fun appl (c: t) (tp: Tp.t) : Tp.t
  = case c of
       ([])      => tp
     | (l::rest) => 
	  case l of
	      (Subst(s))       => appl rest (Subst.appl s tp)
	     | (Lift(l))       => appl rest (Lift.appl l tp)
	     | (Expansion(e))  => appl rest 
				            (Expansion.appl e tp)

fun applP (c: t) (p: Pair.t) : Pair.t
  = case c of
       ([])      => p
     | (l::rest) => 
	  case l of
	       (Subst(s))      => applP rest (Subst.applP s p)
	     | (Lift(l))       => applP rest (Lift.applP l p)
	     | (Expansion(e))  => applP rest (Expansion.applP e p)

fun applB (c: t) (b: Basis.t) : Basis.t
  = case c of
       ([])      => b
     | (l::rest) => 
	  case l of
	       (Subst(s))      => applB rest (Subst.applB s b)
	     | (Lift(l))       => applB rest (Lift.applB l b)
	     | (Expansion(e))  => applB rest (Expansion.applB e b)

end (* Chain *)

signature UNIFY = sig
  structure Tp: TP
  structure Chain: CHAIN

  val unify: Tp.t -> Tp.t -> Chain.t
end (* UNIFY *)


structure Unify : UNIFY = struct
  structure Utils = Utils
  structure Tp = Tp
  structure Typevar = Typevar
  structure Basis = Basis
  structure Chain = Chain
  open Utils

exception CantUnify

fun U (sigma:Tp.t, sigma':Tp.t, tau:Tp.t, tau':Tp.t, m:int)
       : Chain.t * int =

    (* Case 1: *)
    if (Tp.isInter sigma) andalso not (Tp.isInter tau)
       then let val A = union (singleton sigma') (singleton tau')
            in let val (c1,p1) = U(Expansion.expand A tau sigma',
				   Expansion.expand A tau sigma',
				   Expansion.expand A tau tau',
				   Expansion.expand A tau tau',
				   m)
               in (Chain.concat (Chain.expChain (Expansion.makeExpansion A tau)) c1, 1)
	       end
            end

    (* Case 2: *)
    else if (Tp.isInter tau) andalso not (Tp.isInter sigma)
       then let val A = union (singleton sigma') (singleton tau')
            in let val (c1,p1) = U(Expansion.expand A sigma sigma',
				   Expansion.expand A sigma sigma',
				   Expansion.expand A sigma tau',
				   Expansion.expand A sigma tau',
				   m)
               in (Chain.concat (Chain.expChain (Expansion.makeExpansion A sigma)) c1, 1)
	       end
            end

    (* Case 3: *)
    else if Tp.isTvar(sigma) 
       then let val sf = if (sigma = tau) 
		            then (Subst.makesubst [])
		            else if (inset (Tp.allTypevars tau) (Tp.asTvar sigma))  
				    then Subst.substOmegaForAll(Tp.allTypevars tau)
				    else (Subst.makesubst [((Tp.asTvar sigma),tau)])
	    in (Chain.substChain(sf), m)
	    end

    (* Case 4: *)
    else if Tp.isOmega(sigma) 
       then let val sf = Subst.substOmegaForAll(Tp.allTypevars tau)
	    in (Chain.substChain(sf), m)
	    end

    (* Case 5: *)
    else if Tp.isArrow(sigma) 
       then let val (Tp.Arrow(alpha,beta)) = sigma
	    in (* Case 5.1: *)
	       if Tp.isTvar(tau) then
                  let val sf = if (inset (Tp.allTypevars sigma) (Tp.asTvar tau))
				  then Subst.substOmegaForAll(Tp.allTypevars sigma)
				  else (Subst.makesubst [((Tp.asTvar tau),sigma)])
		  in (Chain.substChain(sf),m)
		  end
	       (* Case 5.2: *)
	       else if Tp.isOmega(tau) then
                  let val sf = Subst.substOmegaForAll(Tp.allTypevars sigma)
		  in (Chain.substChain(sf),m)
		  end
	       (* Case 5.3: *)
	       else if Tp.isArrow(tau) then
		  let val (Tp.Arrow(gamma,delta)) = tau
		  in let val (c1,p1) = U(alpha,sigma',gamma,tau',0)
		     in if p1=0 
			   then let val (c2,p2) = U(Chain.appl c1 beta,
						    Chain.appl c1 sigma',
						    Chain.appl c1 delta,
						    Chain.appl c1 tau',
						    m)
				in (Chain.concat c1 c2, p2)
				end
			   else (c1,m)
		     end
		  end
	       (* Error: *)
	       else raise CantUnify
	    end

    (* Case 6: *)
    else if Tp.isInter(sigma) andalso Tp.isInter(tau)
       then    let val (Tp.Intersect(alpha,beta))  = sigma
		   val (Tp.Intersect(gamma,delta)) = tau
	    in let val (c1,p1) = U(alpha,sigma',gamma,tau',m)
	    in let val (c2,p2) = U(Chain.appl c1 beta,
				   Chain.appl c1 sigma',
				   Chain.appl c1 delta,
				   Chain.appl c1 tau',
				   p1)
	    in (Chain.concat c1 c2, Int.max(p1,p2))
	    end end end

    (* Error: *)
    else raise CantUnify


fun unify (sigma:Tp.t) (tau: Tp.t) : Chain.t
  = let val (c,n) = U(sigma,sigma,tau,tau,0)
    in c
    end
  
end (* Unify *)

signature PP = sig
  structure Pair: PAIR
  structure Term: TERM

  val pp: Term.t -> Pair.t
end (* PP *)


structure Pp : PP = struct
  structure Utils = Utils
  structure Tp = Tp
  structure Typevar = Typevar
  structure Basis = Basis
  structure Chain = Chain
  structure Gndvar = Gndvar
  structure Term = Term
  structure Pair = Pair
  structure Unify = Unify
  open Utils

fun H (sigma: Tp.t) : Tp.t
  = case sigma of
      (Tp.Omega)   => sigma
    | (Tp.TVar(_)) => sigma
    | (Tp.Arrow(t1,t2)) => let val Ht2 = H(t2)
    			   in if Ht2 = Tp.Omega 
			   	 then Tp.Omega
			   	 else Tp.Arrow(H(t1),Ht2)
			   end
    | (Tp.Intersect(t1,t2)) => let val Ht1 = H(t1)
     				   val Ht2 = H(t2)
    			       in if (Ht1 = Tp.Omega) andalso (Ht2 = Tp.Omega)
			   	     then Tp.Omega
			   	  else if (Ht1 = Tp.Omega)
				     then Ht2
			   	  else if (Ht2 = Tp.Omega)
				     then Ht1
			   	  else Tp.Intersect(Ht1,Ht2)
			       end

fun HB (B: Basis.t) : Basis.t
  = Basis.map B (fn (v,tp) => (v,H(tp)))

fun pp (X: Term.t) : Pair.t
  = if Term.isVar(X) 
       then let val phi = Typevar.makefresh()
            in (Pair.makePair (Basis.makebasis[(Term.asGndvar X, 
	    					Tp.TVar(phi))])
	    		      (Tp.TVar(phi)))
	    end
    else if Term.isLambda(X) 
       then let    val (Term.Lambda(v,X')) = X 
            in let val (B',pi') = pp(X')
	    in if (Basis.basisdefined B' v)
	          then let val sigma  = (Basis.lookupbasis B' v)
		       in (Pair.makePair (Basis.removeBinding B' v)
		       			 (Tp.Arrow(sigma,pi')))
		       end
		  else (Pair.makePair B' (Tp.Arrow(Tp.Omega,pi')))
	    end end
    else let val (Term.Appl(X1,X2)) = X 
         in let val (B1,pi1) = pp(X1)
	        val (B2,pi2) = pp(X2)
	 in let val (sigma,tau,phi) = Pair.closure (B1,pi1) (B2,pi2)
	 in let val c = (Unify.unify sigma tau)
	 in let val combinecB1cB2 = (Basis.combine 
	 				(Chain.applB c B1)
	 				(Chain.applB c B2))
	        val cphi = (Chain.appl c phi)
	 in let val sktemp = (union (Tp.allSubphrases cphi) 
	 			    (Basis.allSubphrases combinecB1cB2))
	 in let val sktemp1 = (filterFinset sktemp 
	 				    (fn tp => (H(tp) = Tp.Omega)))
	 in let val sktemp2 = (reduce sktemp1
	 			      (fn tp => Tp.allTypevars tp)
				      union
				      emptyset)
	 in let val sk = (Subst.substOmegaForAll sktemp2)
	 in (* Maybe H should be applied to both components: *)
	     (HB (Subst.applB sk combinecB1cB2),
	      H  (Subst.appl sk cphi))
	 end end end end end end end end end
  
end (* Pp *)

structure Test = struct
  structure Chain = Chain
  structure Typevar = Typevar
  structure Tp = Tp
  structure Unify = Unify
  structure Pp = Pp

open Utils Term Typevar Gndvar

infixr 7 arrow
infix  8 intersect
val omega = Tp.Omega
fun tvar (tv: Typevar.t): Tp.t = Tp.TVar tv
fun op arrow (a,b): Tp.t = Tp.Arrow (a,b)
fun op intersect (a,b): Tp.t = Tp.Intersect (a,b)
val tv = Typevar.tv


val alpha = (tvar (tv "alpha"))
val beta = (tvar (tv "beta"))
val gamma = (tvar (tv "gamma"))
val delta = (tvar (tv "delta"))

fun testUnify sigma tau 
  = (let val dummy =
        write ("sigma = " ^ (Tp.tostring sigma) ^ "\n"
             ^ "tau   = " ^ (Tp.tostring tau) ^ "\n")
    in let val c = Unify.unify sigma tau
    in let val csigma = (Chain.appl c sigma)
	   val ctau   = (Chain.appl c tau)
    in let val dummy =
        write ("unify(sigma,tau) = c = \n" ^ (Chain.tostringpp c 4) 
             ^ "c(sigma) = " ^ (Tp.tostring (Chain.appl c sigma)) ^ "\n"
             ^ "c(tau) = " ^ (Tp.tostring (Chain.appl c tau)) ^ "\n"
	     ^ "\n\n")
    in ()
    end end end end
    handle Interrupt => write "... Interrupted\n\n\n")
    handle CantUnify => write "... Not unifiable\n\n\n"

fun testU() 
  = let val x = write "\nTests of the unifier for conjunctive types:\n\n"
        val x = testUnify (alpha arrow beta) 
			  (gamma intersect delta)
        val x = testUnify (omega) 
			  (omega arrow omega)
        val x = testUnify (alpha intersect (alpha arrow beta))
		   	  ((gamma intersect (gamma arrow delta)) arrow delta)
        val x = testUnify (alpha) 
			  (beta)
        val x = testUnify (alpha arrow beta) 
			  (alpha intersect delta)
        val x = testUnify (alpha arrow beta) 
			  (omega)
        val x = testUnify ((alpha intersect beta) intersect gamma) 
			  (alpha intersect (beta intersect gamma))
        val x = testUnify (alpha intersect beta) 
			  (beta intersect alpha)
    in ()
    end

fun testPP X
  = (let val dummy =
        write ("X = " ^ (Term.tostring X) ^ "\n")
    in let val p = Pp.pp X
    in let val dummy =
        write ("pp(X) = \n" ^ (Pair.tostringpp p 4) 
	     ^ "\n\n")
    in ()
    end end end
    handle Interrupt => write "... Interrupted\n\n\n")
    handle CantUnify => write "... Unify failed\n\n\n"

val x = gv "x"
val X = Var(x)
val X1 = (Lambda(x, X))
val X2 = (Appl(X,X))
val X3 = (Lambda(x,(Appl(X,X))))

fun test() 
  = (write "\nTests of the Principal Pair algorithm:\n\n";
     testPP X;
     testPP X1;
     testPP X2;
     testPP X3
    )

end (* Test *)

