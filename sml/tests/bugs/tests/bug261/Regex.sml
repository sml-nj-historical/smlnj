(*------------------------ Regex.sml ------------------------*)


import "Ordinal";
import "BitSet";
import "RedBlack";

signature CHAR_REG_EXP =
  sig
    structure Alphabet : ORD_RANGE
    structure AlphaSet : BITSET
    structure range : ORD_RANGE
    structure Set : BITSET
    sharing type Set.Elem.elem = range.elem = int

    type pattern

    datatype Ch_Reg_Exp
      = CONCAT of Ch_Reg_Exp list
      | ALTERNATE of Ch_Reg_Exp list
      | STAR of Ch_Reg_Exp
      | PLUS of Ch_Reg_Exp
      | OPTION of Ch_Reg_Exp
      | LETTER of AlphaSet.bitset
      | AUG   (* For internal use only *)

    type Aug_Reg_Exp

    val re_to_Aug : Ch_Reg_Exp -> {aug_re : Aug_Reg_Exp, count : int,
				leafList : Aug_Reg_Exp list}

    val Aug_to_Follow : {aug_re : Aug_Reg_Exp, count : int,
			 leafList : Aug_Reg_Exp list} -> Set.bitset array

    val Build_FSM :  {aug_re : Aug_Reg_Exp, count : int,
			 leafList : Aug_Reg_Exp list} * Set.bitset array
		     -> pattern

    val Print : Aug_Reg_Exp -> unit
  end

functor Reg_ExpFn() (* : CHAR_REG_EXP *) =
  struct
      structure Alphabet = CharFn()
      structure AlphaSet : BITSET = BitSetFn(Alphabet)
      structure range = NatFn()
      structure Set : BITSET = BitSetFn(range)

      type InfoTy =
	{
	  fp : Set.bitset,
	  lp : Set.bitset,
	  null : bool
	}

      datatype pattern = Pat

      datatype Ch_Reg_Exp
	= CONCAT of Ch_Reg_Exp list
	| ALTERNATE of Ch_Reg_Exp list
	| STAR of Ch_Reg_Exp
	| PLUS of Ch_Reg_Exp
	| OPTION of Ch_Reg_Exp
	| LETTER of AlphaSet.bitset
	| AUG       (* Internal use only *)

      datatype Aug_Reg_Exp
	= AugCONCAT of InfoTy * Aug_Reg_Exp list
	| AugALTERNATE of InfoTy * Aug_Reg_Exp list
	| AugSTAR of InfoTy * Aug_Reg_Exp
	| AugPLUS of InfoTy * Aug_Reg_Exp
	| AugOPTION of InfoTy * Aug_Reg_Exp
	| AugLETTER of InfoTy * AlphaSet.bitset
	| AugAUG of InfoTy

      fun mkInfo () =
	 {
	   Fpos = Set.empty,
	   Lpos = Set.empty,
	   Nullable = false
	 }

      fun info (AugCONCAT(inf, _)) = inf
	| info (AugALTERNATE(inf, _)) = inf
	| info (AugSTAR(inf, _)) = inf
	| info (AugPLUS(inf, _)) = inf
	| info (AugOPTION(inf, _)) = inf
	| info (AugLETTER(inf, _)) = inf
	| info (AugAUG(inf)) = inf

(*      type 'a Aug = {aug_re : Aug_Reg_Exp, count : int,
		 leafList : Aug_Reg_Exp list}
*)
      fun re_to_Aug re =
        let
	  fun app_map cnt nil = (nil : Aug_Reg_Exp list, cnt, nil)
	    | app_map cnt (hd::tl) =
	      let
		val hd' = pass1(cnt, hd)
		val {aug_re=ar, count=c, leafList=le} = hd'
		val (arTl, cntTl, leTl) = app_map c tl
	      in
		(ar::arTl, cntTl, le@leTl)
	      end
	  and pass1 (counter, CONCAT(re_l)) =
		let
		  val (ar', cnt', le') = app_map counter re_l
		  fun foldConcat (a, b) =
		    let
		      val {fp = fpA, lp = lpA, null = nuA} = a
		      val {fp = fpB, lp = lpB, null = nuB} = b
		      val n = nuA andalso nuB
		      val fp' = if nuB then Set.union (fpA, fpB) else fpB
		      val lp' = if nuA then Set.union (lpA, lpB) else lpA
		    in
		      print "foldConcat\n";
			print "    given A ";
			  print "  fpA="; print (Set.makeString fpA);
			  print "  lpA="; print (Set.makeString lpA);
			  print nuA; print "\n";
			print "    given B ";
			  print "  fpB="; print (Set.makeString fpB);
			  print "  lpB="; print (Set.makeString lpB);
			  print nuB; print "\n";
			print "    results ";
			  print "  fp'="; print (Set.makeString fp');
			  print "  lp'="; print (Set.makeString lp');
			  print n; print "\n";
		      {fp = fp', lp = lp', null = n}
		    end
		  val base = {fp = Set.empty, lp = Set.empty, null = true}
		  val _ = (print "    base ";
			  print "  fp'="; print (Set.makeString (#fp base));
			  print "  lp'="; print (Set.makeString (#lp base));
			  print (#null base); print "\n")
		  val info = revfold foldConcat (map info ar') base
		in
		  {aug_re = AugCONCAT(info, ar'), count = cnt',
		      leafList = le'}
		end
	    | pass1 (counter, ALTERNATE(re_l)) =
		  let
		    val (ar', cnt', le') = app_map counter re_l
		    fun foldAlt (a, b) =
		      let
			val {fp = hdA, lp = lpA, null = nuA} = a
			val {fp = hdB, lp = lpB, null = nuB} = b
		      in
			{fp = Set.union (hdA, hdB),
			 lp = Set.union (lpA, lpB),
			 null = nuA orelse nuB}
		      end
		    val base = {fp = Set.empty, lp = Set.empty, null = false}
		    val info = fold foldAlt (map info ar') base
                  in
		    {aug_re = AugALTERNATE(info, ar'), count = cnt',
			leafList = le'}
		  end
	    | pass1 (counter, STAR(re)) =
		  let
		    val {aug_re=ar, count=c, leafList=le} = pass1(counter, re)
		    val {fp = fp', lp = lp', null = nu} = info ar
		    val info = {fp = fp', lp = lp', null = true}
                  in
		    {aug_re = AugSTAR(info, ar), count = c, leafList = le}
		  end
	    | pass1 (counter, PLUS(re)) =
		  let
		    val {aug_re=ar, count=c, leafList=le} = pass1(counter, re)
		    val {fp = fp', lp = lp', null = nu} = info ar
		    val info = {fp = fp', lp = lp', null = nu}
                  in
		    {aug_re = AugPLUS(info, ar), count = c, leafList = le}
		  end
	    | pass1 (counter, OPTION(re)) =
		  let
		    val {aug_re=ar, count=c, leafList=le} = pass1(counter, re)
		    val {fp = fp', lp = lp', null = nu} = info ar
		    val info = {fp = fp', lp = lp', null = true}
                  in
		    {aug_re = AugOPTION(info, ar), count = c, leafList = le}
		  end
	    | pass1 (counter, LETTER(a)) =
		  let
		    val c = Set.singleton counter
		    val info = {fp = c, lp = c, null = false}
		    val aug_r = AugLETTER(info, a)
                  in
		    {aug_re = aug_r, count = counter+1, leafList = [aug_r]}
		  end
	    | pass1 (counter, AUG) =
		  let
		    val c = Set.singleton counter
		    val info = {fp = c, lp = c, null = false}
		    val aug_r = AugAUG(info)
                  in
		    {aug_re = aug_r, count = counter+1, leafList = [aug_r]}
		  end
        in
	  pass1 (0, CONCAT [re, AUG])
        end



      fun prFollow fp =
	let
	  val l = Array.length fp
          fun prx i = (print i; print "  "; print (Set.makeString (fp sub i));
		print "\n")
	  fun p i = if i < l then (prx i; p (i + 1)) else ()
	in
	  p 0
	end

      fun Aug_to_Follow {aug_re, count, leafList} =
	let
	  val followPos = array(count, Set.empty)
	  val count = ref 0
	  fun updSet fp i = update(followPos, i,
		Set.union(followPos sub i, fp))
	  fun pass2 (AugCONCAT(inf, re_l)) =
		let
		  fun foldConcat (x, y) =
		    let
		      val updList = Set.forall y
		      val {fp, lp, ...} = info x
		      val updSet' = updSet fp
		      fun ms nil = ""
		        | ms (x::y) = (makestring x) ^ (ms y)
		    in
		      print ("[" ^ (ms updList) ^ "]" ^ "\n"); 
		      print (Set.makeString lp ^ "\n");
		      app updSet' updList;
		      lp
		    end
		  val c = !count
		in
		  inc count;
		  print "before fold "; print c; print "\n";
		  prFollow followPos; print "\n";
		  revfold foldConcat re_l Set.empty;
		  print "after fold "; print c; print "\n";
		  prFollow followPos; print "\n";
		  app pass2 re_l;
		  print "after app "; print c; print "\n";
		  prFollow followPos; print "\n"
		end
	    | pass2 (AugALTERNATE(inf, re_l)) = app pass2 re_l
	    | pass2 (AugSTAR(inf, re)) =
		let
		  val {fp, lp, ...} = info re
		  val updSet' = updSet fp
		in
		  app updSet' (Set.forall lp);
		  pass2 re
		end
	    | pass2 (AugPLUS(inf, re)) =
		let
		  val {fp, lp, ...} = info re
		  val updSet' = updSet fp
		in
		  app updSet' (Set.forall lp);
		  pass2 re
		end
	    | pass2 (AugOPTION(inf, re)) = pass2 re
	    | pass2 (AugLETTER(inf, _)) = ()
	    | pass2 (AugAUG(inf)) = ()
	in
	  pass2 aug_re;
	  followPos
        end

	datatype transition = TR of AlphaSet.bitset * state
	and state = ST of {posSet : Set.bitset,
			        stId : int,
			        trans : transition list}
	fun le (ST{posSet = pS1,...}, ST{posSet = pS2,...}) =
		Set.totOrder (pS1, pS2)
        fun getPosSet (ST{posSet,...}) = posSet
        structure table = RedBlack(struct type key = state
					  val op > = le end)

	fun Build_FSM ({aug_re, count, leafList}, followPos) =
	  let
	         (* get character set at position i *)
	    fun cSetAt i =
	      case nth (leafList, i) of
                AugLETTER(inf, x) => x
	      | AugAUG(_) => AlphaSet.empty

		 (* test to see if character has transition at position i *)
	    fun atPos c i = AlphaSet.exists c (cSetAt i)

		 (* Is this position a final position *)
	    fun final i =
	      case nth (leafList, i) of
	        AugAUG(_) => true
              | _ => false

		 (* return only those elements which match query *)
	    fun sublist query l =
	      let
		fun ss nil = nil
	          | ss (hd::tl) =
		     let val x = (ss tl)
		     in if query hd then hd::x else x
		     end
	      in
		ss l
              end

	    val cnt = ref 1

	    fun build_auto states unmarked =
	      if unmarked = nil then states else
	      let
	        val T = hd unmarked
		val _ = print ("build_auto " ^ (Set.makeString T) ^ "\n")
		val allchar =
		    let
		      fun f (i, x)  = AlphaSet.union(cSetAt i, x)
		    in
		      fold f (Set.forall T) AlphaSet.empty
		    end
		val _ = print ("    allchar = " ^ (AlphaSet.makeString allchar)
			^ "\n");
		fun eachChar states unmarked trans allchar =
		  if AlphaSet.isempty allchar then
		    (states, unmarked, trans)
		  else
		    let
		      val _ = print "eachChar\n"
		      fun next cSet =
			let
			  val x = AlphaSet.lowest cSet
			  val _ = print ("next cSet=" ^
				  (AlphaSet.makeString cSet) ^ "  ")
			  val _ = print (makestring (AlphaSet.Elem.ord x)
				   ^ "\n")
			  val posSet = Set.select (T, atPos x)
			  val _ = print "Check\n"
			  fun findSet i ch =
			    if i = count then ch
			    else
			      let val y = if Set.exists i posSet then
					    AlphaSet.intersect(ch, cSetAt i)
					  else
					    AlphaSet.difference(ch, cSetAt i)
			      in
				findSet (i + 1) y
			      end
			in
			  (findSet 0 cSet, posSet)
			end (* next *)

		      val (cSet, posSet) = next allchar
		      val _ = print ("     cSet=" ^ (AlphaSet.makeString cSet)
				   ^ ", posSet = " ^ (Set.makeString posSet)
				   ^ "\n")

		      fun makeU s =
			let
			  fun f (i, x) = Set.union (followPos sub i, x)
			in
			  fold f (Set.forall posSet) Set.empty
			end  (* makeU *)

		      val U = makeU posSet
		      val _ = print ("     U=" ^ (Set.makeString U) ^ "\n") 


		      fun FindInsert st u =
			let
			  val dummy = ST{posSet = u, stId = 0, trans = []}
			in
			  (table.lookup(dummy, st), st, unmarked)
			    handle table.notfound _ =>
			      let
				val u' = ST{posSet = u, stId = !cnt, trans=[]}
				val st' = table.insert(u', st)
			      in
				inc cnt;
				(u', st', unmarked@[u])
			      end
			end  (* FindInsert *)
		      val (ToState, states', unmarked') = FindInsert states U
		      val trans' = TR(cSet, ToState)::trans
		    in
		      eachChar states' unmarked' trans'
			 (AlphaSet.difference (allchar, cSet))
		    end  (* eachChar *)

		val (states', unmarked', trans') =
		    eachChar states unmarked [] allchar
		val dummy = ST{posSet = T, stId = 0, trans = []}
		val ST{stId = Tid,...} = table.lookup(dummy, states')
		val s = ST{posSet = T, stId = Tid, trans = trans'}
		val states2 = table.insert(s, states')
	      in
		build_auto states' (tl unmarked')
	      end  (* build_auto *)
	    val {fp = st',...} = info aug_re
	    val startstate = ST{posSet = st', stId = 0, trans = []}
	    val stTable = table.insert (startstate, table.empty)
	    val autoList = build_auto stTable [st']
	  in
	    autoList
	  end

      fun Print re =
        let
          val depth = ref 0
          fun printInfo ({fp, lp, null} : InfoTy) =
	      (
		print "Fpos=";
		print (Set.makeString (fp));
		print "  Lpos=";
		print (Set.makeString (lp));
		if null then
		  print "  nullable\n"
		else
		  print "\n"
	      )
	  fun Pr (AugCONCAT(inf, re_l)) =
	      (
		print "CONCAT  ";
		printInfo inf;
	        app Pr1 re_l
	      )
	    | Pr (AugALTERNATE(inf, re_l)) =
	      (
		print "ALTERN  ";
		printInfo inf;
	        app Pr1 re_l
	      )
	    | Pr (AugSTAR(inf, re)) =
	      (
		print "KLEENE  ";
		printInfo inf;
	        Pr1 re
	      )
	    | Pr (AugPLUS(inf, re)) =
	      (
		print "POSITV  ";
		printInfo inf;
	        Pr1 re
	      )
	    | Pr (AugOPTION(inf, re)) =
	      (
		print "OPTION  ";
		printInfo inf;
	        Pr1 re
	      )
	    | Pr (AugLETTER(inf, _)) =
	      (
		print "LETTER  ";
		printInfo inf
	      )
	    | Pr (AugAUG(inf)) =
	      (
		print "AUGMEN  ";
		printInfo inf
	      )
          and
	      Pr1 x =
	      let
		val i = ref 0;
	      in
		(
		  while (!i) < (!depth) do
		    (print "  "; inc i);
		  inc depth;
		  Pr x;
		  dec depth
	        )
	      end
        in
	   Pr1 re
        end;
  end;


structure RRP_Test = Reg_ExpFn();

open RRP_Test;

val a = LETTER(AlphaSet.singleton "a")
val b = LETTER(AlphaSet.singleton "b")
val c = LETTER(AlphaSet.singleton "c")
val d = LETTER(AlphaSet.singleton "d")

val a_b_c = ALTERNATE([a, b, c]);
val aug = re_to_Aug a_b_c;
Print (#aug_re aug);
val fol = Aug_to_Follow aug;
prFollow fol;
print "Before Build_FSM\n";
val t = Build_FSM(aug, fol);

print "\n\n\n";
val abc = CONCAT([a, b, c]);
val aug' = re_to_Aug abc;
Print (#aug_re aug');
val fol' = Aug_to_Follow aug';
prFollow fol';
val t' = Build_FSM(aug', fol');
