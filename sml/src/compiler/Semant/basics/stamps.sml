(* Copyright 1996 by AT&T Bell Laboratories *)
(* stamps.sml *)

structure Stamps : STAMPS = 
struct

  datatype stamp_scope
    = LOCAL
    | GLOBAL of PersStamps.persstamp
    | SPECIAL of string

  datatype stamp = STAMP of {scope : stamp_scope, count : int}

  fun new () = 
    let val i = ref 0
     in fn () => STAMP{scope=LOCAL, count= !i before i := !i + 1}
    end

  fun eq (a:stamp, b) = a = b

  fun cmp (STAMP{scope=a1,count=b1}, STAMP{scope=a2,count=b2}) = 
      (case Int.compare (b1, b2)
	of EQUAL => (case (a1, a2)
                      of (LOCAL, LOCAL) => EQUAL
                       | (LOCAL, _) => LESS
                       | (_, LOCAL) => GREATER
                       | (GLOBAL pid1, GLOBAL pid2) => 
                                      PersStamps.compare(pid1, pid2)
                       | (GLOBAL _, _) => GREATER
                       | (_, GLOBAL _) => LESS
                       | (SPECIAL s1, SPECIAL s2) => String.compare(s1, s2)
                     (* end case *))
         | order => order)

  fun special s = STAMP{scope=SPECIAL s, count=0}

  fun stampToString (STAMP{scope, count}) = 
      let val scope' = (case scope
			 of LOCAL => "LOCAL"
			  | (GLOBAL pid) => PersStamps.toHex pid
			  | (SPECIAL s) => s)
       in String.concat["STAMP(", scope', ",", Int.toString count, ")"]
      end

  fun stampToShortString (STAMP{scope, count}) = 
      let val scope' = 
	      case scope
		of LOCAL => ["#L"]
		 | GLOBAL pid =>
		    let val s = PersStamps.toHex pid
			val l = String.size s
		     in ["#G",String.substring(s,l-3,3)]
		    end
		 | SPECIAL s => ["#S:",s]
       in String.concat(scope'@[".",Int.toString count])
      end

  type 'a stampMap = (stamp * 'a) list Intmap.intmap * exn

  fun newMap ex = (Intmap.new(20, ex), ex)

  fun applyMap((m,ex), st as STAMP{count,...}) = 
    let fun f((a,b)::r) = if eq(a,st) then b else f r
          | f nil = raise ex
     in f(Intmap.map m count)
    end

  fun updateMap (m,ex) (st as STAMP{count=n,...}, v) = 
    let val old = Intmap.map m n handle _ => nil
     in Intmap.add m (n, (st,v)::old)
    end

end (* structure Stamps *)

(*
 * $Log$
 *)
