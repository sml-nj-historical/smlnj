(*
 * A pattern matching compiler. 
 * This is a literal translation of Pettersson's paper
 * ``A Term Pattern-Match Compiler Inspired by Finite Automata Theory''
 *
 *)
local
   val sanityCheck = true
   val debug       = false
in

functor MatchCompiler
   (structure Con : (* datatype constructors *)
    sig
       type con 
       val compare     : con * con -> order
       val toString    : con -> string
       val allVariants : con -> con list
       val arity       : con -> int
    end  

    structure Literal : (* literals *)
    sig
       type literal
       val compare  : literal * literal -> order
       val toString : literal -> string
    end

    structure Action :   
    sig type action  (* an action *)
        val toString : action -> string
    end

    structure Guard  : (* a guard expression *)
    sig type guard 
        val toString : guard -> string
    end

               (* a variable *)
    structure Var  : 
    sig type var 
        val compare : var * var -> order 
        val toString : var -> string
    end
   ) : MATCH_COMPILER =
struct

   structure PP = PP

   val i2s = Int.toString

   fun listify (l,s,r) list = 
       l^List.foldr (fn (x,"") => x | (x,y) => x^s^y) "" list^r

   datatype index = INT of int | LABEL of Var.var

   datatype path  = PATH of index list

   (* Internal rep of pattern after every variable has been renamed *) 
   datatype pat = 
     WILD                    (* wild card *)
   | APP of decon * pat list
   | TUPLE of pat list
   | RECORD of (Var.var * pat) list

   and decon = CON of Con.con          
             | LIT of Literal.literal   

   exception MatchCompiler of string

   fun error msg = raise MatchCompiler msg 
   fun bug msg   = error("bug: "^msg)

   structure Con     = Con
   structure Action  = Action
   structure Literal = Literal
   structure Guard   = Guard
   structure Var     = Var

   structure Index =
   struct
      fun compare(INT i, INT j) = Int.compare(i,j)
        | compare(LABEL i, LABEL j) = Var.compare(i,j)
        | compare(INT _, LABEL _) = LESS
        | compare(LABEL _,INT _) = GREATER
      fun equal(x,y) = compare(x,y) = EQUAL
      fun toString(INT i) = i2s i
        | toString(LABEL l) = Var.toString l
   end

   structure Path =
   struct
      fun compare(PATH p1, PATH p2) =
      let fun loop([], []) = EQUAL
            | loop([], _)  = LESS
            | loop(_, [])  = GREATER
            | loop(x::xs, y::ys) =
              (case Index.compare(x,y) of
                EQUAL => loop(xs,ys)
              | ord   => ord
              )
      in  loop(p1, p2) 
      end
      fun equal(p1,p2) = compare(p1,p2) = EQUAL
      fun append(PATH p1, PATH p2) = PATH(p1@p2)
      fun dot(PATH p, i) = PATH(p @ [i])
      fun toString(PATH p) =
          "["^List.foldr (fn (i,"") => Index.toString i
                           | (i,s) => Index.toString i^"."^s) "" p^"]"
      fun toIdent(PATH p) = 
          "v_"^List.foldr (fn (i,"") => Index.toString i
                            | (i,s) => Index.toString i^"_"^s) "" p
      structure Map = RedBlackMapFn(type ord_key = path val compare = compare)
      structure Set = RedBlackSetFn(type ord_key = path val compare = compare)
   end

   structure Decon =
   struct
      fun kind(CON _) = 0
        | kind(LIT _) = 1
      fun compare(CON x,CON y) = Con.compare(x,y)
        | compare(LIT x,LIT y) = Literal.compare(x,y)
        | compare(x,y) = Int.compare(kind x,kind y)

      fun toString(CON c) = Con.toString c
        | toString(LIT l) = Literal.toString l

      fun equal(x,y) = compare(x,y) = EQUAL
      structure Map = RedBlackMapFn(type ord_key = decon val compare = compare)
      structure Set = RedBlackSetFn(type ord_key = decon val compare = compare)
   end 

   structure Pat =
   struct
      fun sortByLabel l =
          ListMergeSort.sort 
            (fn ((x,_),(y,_)) => Var.compare(x,y) = GREATER) l

      fun toString(WILD) = "_"
        | toString(APP(c,[])) = Decon.toString c
        | toString(APP(c,xs)) = Decon.toString c^
                                 listify("(",",",")") (map toString xs)
        | toString(TUPLE pats) = listify("(",",",")") (map toString pats)
        | toString(RECORD lps) = listify("{",",","}") 
                                 (map (fn (l,p) =>
                                      Var.toString l^"="^toString p) lps)

      fun kind(WILD) = 0
        | kind(APP _) = 1
        | kind(TUPLE _) = 2
        | kind(RECORD _) = 3

      and compareList([], []) = EQUAL
        | compareList([], _)  = LESS
        | compareList(_, [])  = GREATER
        | compareList(x::xs, y::ys) =
          (case compare(x,y) of
            EQUAL => compareList(xs,ys)
          | ord   => ord 
          ) 
 
      and compare(WILD, WILD) = EQUAL
        | compare(APP(f,xs), APP(g,ys)) =
          (case Decon.compare(f,g) of
             EQUAL => compareList(xs, ys)
          |  ord => ord  
          )
        | compare(TUPLE xs, TUPLE ys) = compareList(xs, ys)
        | compare(RECORD xs,RECORD ys) =
          let val xs = sortByLabel xs
              val ys = sortByLabel ys
              fun loop([], []) = EQUAL
                | loop([], _)  = LESS
                | loop(_, [])  = GREATER
                | loop((i,x)::xs, (j,y)::ys) =
                  (case Var.compare(i, j) of 
                     EQUAL => (case compare(x,y) of
                                  EQUAL => loop(xs,ys)
                               | ord => ord
                              )
                  |  ord => ord
                  )
          in  loop(xs, ys)
          end
        | compare(x, y) = Int.compare(kind x, kind y)

      fun isRefutable WILD = false
        | isRefutable (APP(f,_)) = true
        | isRefutable (TUPLE ps) = List.exists isRefutable ps
        | isRefutable (RECORD ps) = List.exists (fn (l,p) => isRefutable p) ps
      
      structure Set = RedBlackSetFn(type ord_key = pat val compare = compare)
      fun equal(p1,p2) = compare(p1,p2) = EQUAL
   end

   type subst = Var.var Path.Map.map

   datatype dfa = 
       DFA of  
       { stamp    : int,              (* unique dfa stamp *)
         freeVars : Path.Set.set ref, (* free variables *)
         refCount : int ref,          (* reference count *)
         generated: bool ref,         (* has code been generated? *)
         test     : test              (* type of tests *)
       }

   and test = 
         CASE  of path * (decon * path list * dfa) list * 
                  dfa option (* multiway *)
       | WHERE of subst * guard * dfa * dfa              (* if test *)
       | OK    of subst * Action.action                  (* final dfa *)
       | BIND  of path * (path * index) list * dfa       (* bind *)
       | FAIL                                            (* error dfa *)
       | ROOT  of Path.Set.set * dfa  (* root *)

   and guard = GUARD of Guard.guard ref

   and matrix = 
       MATRIX of
       { rows  : row list,
         paths : path list                       (* path (per column) *)
       }

   withtype row = 
              {pats  : pat list, 
               guard : (subst * guard) option,
               dfa   : dfa
              } 
   type compiled_rule = pat list * guard option * subst * Action.action

   (* Utilities for dfas *)
   structure DFA =
   struct
      val itow = Word.fromInt 

      fun hash(DFA{stamp, test, ...}) = 
          (case test of
            FAIL    => 0w0
          | OK _    => 0w123 + itow stamp
          | CASE(path, cases, default) => 0w1234 
          | BIND(_, _, dfa) => 0w2313 + hash dfa
          | WHERE(_, g, yes, no) => 0w2343
          | ROOT _ => 0w3414
          )

      (* pointer equality *)
      fun eq(DFA{stamp=s1, ...}, DFA{stamp=s2, ...}) = s1=s2
      fun eqOpt(NONE, NONE) = true
        | eqOpt(SOME x, SOME y) = eq(x,y)
        | eqOpt _ = false

      (* one-level equality *)
      fun equal(DFA{test=t1, stamp=s1,...},
                DFA{test=t2, stamp=s2,...}) =
             (case (t1, t2) of
                (FAIL, FAIL) => true
              | (OK _, OK _) => s1 = s2
              | (BIND(p1, b1, x), BIND(p2, b2, y)) => 
                 Path.equal(p1,p2) andalso eq(x,y) andalso
                 ListPair.all(fn ((px,ix),(py,iy)) =>
                    Path.equal(px,py) andalso Index.equal(ix,iy))
                     (b1,b2)
              | (CASE(p1,c1,o1), CASE(p2,c2,o2)) =>
                  Path.equal(p1,p2) andalso 
                  ListPair.all
                     (fn ((u,_,x),(v,_,y)) => 
                          Decon.equal(u,v) andalso eq(x,y)) 
                        (c1,c2) andalso
                  eqOpt(o1,o2)
              | (WHERE(_, GUARD(g1), y1, n1), 
                 WHERE(_, GUARD(g2), y2, n2)) =>
                  g1 = g2 andalso eq(y1,y2) andalso eq(n1,n2) 
              | _ => false
             )

      structure HashTable = 
         HashTableFn(type hash_key = dfa
                     val sameKey = equal
                     val hashVal = hash
                    )

      fun toString dfa =
      let exception NotVisited
          val visited = IntHashTable.mkTable(32, NotVisited)
          fun mark stamp = IntHashTable.insert visited (stamp, true)
          fun isVisited stamp = 
              Option.getOpt(IntHashTable.find visited stamp, false)
          open PP
          infix ++
          fun prArgs [] = nop
            | prArgs ps = seq(!!"(",!!",",!!")") (map (! o Path.toString) ps)
          fun walk(DFA{stamp, test=FAIL, ...}) = ! "fail"
            | walk(DFA{test=ROOT(_,dfa), ...}) = walk dfa
            | walk(DFA{stamp, test, refCount=ref n, ...}) =
              if isVisited stamp then !"goto" ++ int stamp 
              else (mark stamp;
                    !!"<" ++ int stamp ++ !!">" ++
                    (if n > 1 then !! "*" else nop) ++
                    (case test of
                      OK(_,a) => !"Ok" ++ !(Action.toString a)
                    | FAIL => !"Fail"
                    | BIND(root,bindings,body) => 
                      line(!"Let") ++
                      block(seq (nop,nl,nop) 
                              (map (fn (p,i) =>
                               tab ++
                               !(Path.toString p) ++ !"=" ++ 
                               !(Path.toString root) ++ !"." ++ 
                                 !(Index.toString i)
                                ) bindings) 
                           ) ++
                      line(!"in") ++
                      block(walk body)
                    | CASE(p,cases,default) =>
                      line(!"Case" ++ !!(Path.toString p)) ++
                       block(
                          seq (nop,nl,nop) 
                           ((map (fn (decon,args,dfa) =>
                             tab ++ !(Decon.toString decon) ++ prArgs args
                                 ++ !"=>" ++ sp ++ walk dfa)
                               cases) @
                             (case default of
                               NONE => []
                             | SOME dfa => [!"_" ++ !"=>" ++ sp ++ walk dfa]
                             )
                          )
                       )
                    | WHERE(_,GUARD(ref g),y,n) =>
                      line(!"If" ++ !(Guard.toString g)) ++
                      block(tab ++ ! "then" ++ walk y ++ nl ++
                            tab ++ ! "else" ++ walk n)
                    | _ => bug "walk"
                    )
                   )
      in  PP.text(walk dfa ++ nl)
      end
   end

   (* Utilities for the pattern matrix *)
   structure Matrix =
   struct
       datatype expandType = SWITCH of (decon * path list * matrix) list 
                                     * matrix option
                           | REBIND of path * (path * index) list * matrix

       fun row(MATRIX{rows, ...}, i) = List.nth(rows,i)
       fun col(MATRIX{rows, ...}, i) = 
             List.map (fn {pats, ...} => List.nth(pats, i)) rows
       fun pathOf(MATRIX{paths, ...}, i) = List.nth(paths, i)
       fun columnCount(m) = List.length(#pats(row(m,0)))
       fun isEmpty(MATRIX{rows=[], ...}) = true
         | isEmpty _ = false

       fun removeFirstRow(MATRIX{rows=_::rows, paths}) = 
             MATRIX{rows=rows, paths=paths}
         | removeFirstRow _ = error "removeFirstRow"

       fun check(MATRIX{rows, paths, ...}) =
       let val arity = length paths
       in  app (fn {pats, ...} =>
                 if length pats <> arity then bug "bad matrix" else ())
               rows
       end

       fun toString(MATRIX{rows, paths, ...}) =
           listify("","\n","\n")
             (map (fn {pats, ...} =>
                    listify("[","\t","]") (map Pat.toString pats)) rows)

       (*
        * Given a matrix, find the best column for matching.
        *
        * I'm using the heuristic that John (Reppy) uses:
        * the first column i where pat_i0 is not a wild card, and
        * with the maximum number of distinct constructors in the
        * the column. 
        *
        * If the first row is all wild card, then return NONE.
        *)
       fun findBestMatchColumn(m as MATRIX{rows, ...}) = 
       let val _ = if sanityCheck then check m else ()
           val _ = if debug then
                      (print(toString m))
                   else ()
           val nCol = columnCount m

           fun score i = (* score of doing pattern matching on column i *)
           let val pats_i = col(m, i)
               val pats_i0 = hd pats_i 
           in  case pats_i0 of 
                 WILD => 0
               | _  =>
                 let val (cons, score) =
                    (* count distinct constructors; skip refutable cards 
                     * Give records or tuples high scores so that
                     * they are immediately expanded
                     *)
                       List.foldr (fn (WILD, (S, n)) => (S, n)
                                 | (TUPLE _, (S, n)) => (S, 100000)
                                 | (RECORD _, (S, n)) => (S, 100000)
                                 | (pat, (S, n)) => (Pat.Set.add(S, pat), n))
                           (Pat.Set.empty, 0) pats_i
                 in score + Pat.Set.numItems cons end
           end

           (* Find column with the highest score *)
           fun findBest(i, bestSoFar) =
               if i >= nCol then bestSoFar else 
               let val score_i = score i
                   val best = 
                       if case bestSoFar of
                            NONE                => true
                          | SOME(_, best_score) => score_i > best_score
                       then SOME(i, score_i)
                       else bestSoFar
               in  findBest(i+1, best)
               end

       in  case findBest(0, NONE) of
             SOME(i, 0) => NONE   (* a score of zero means all wildcards *)
           | SOME(i, _) => SOME i
           | NONE => NONE 
       end

       structure LSet = RedBlackSetFn(type ord_key = Var.var 
                                      val compare = Var.compare)

       (*
        * Expand column i, 
        * Return a new list of matrixes indexed by the deconstructors.
        *) 
       fun expandColumn(m as MATRIX{rows, paths, ...}, i) = 
       let val ithCol = col(m, i)
           val path_i = pathOf(m, i)
           val _ = if debug then
                      (print("Expanding column "^i2s i^"\n"))
                   else ()
 
           fun split_i ps =
           let fun loop(j, p::ps, ps') =
                   if i = j then (rev ps', p, ps) 
                   else loop(j+1, ps, p::ps')
                 | loop _ = bug "split_i"
           in  loop(0, ps, []) end
 
           (* find first non-wild card constructor *)
           val firstCon = List.find (fn WILD => false | _ => true) ithCol

            (* Split the paths *)
           val (prevPaths, _, nextPaths) = split_i paths

       in  case firstCon of
             SOME(TUPLE pats) => (* expand a tuple along all the columns *)
             let val arity = length pats
                 val wilds = map (fn _ => WILD) pats
                 fun processRow{pats, dfa, guard} =
                 let val (prev, pat_i, next) = split_i(pats)
                 in  case pat_i of
                        TUPLE ps' =>
                        let val n = length ps'
                        in  if n <> arity then error("tuple arity mismatch")
                            else ();
                            {pats=prev @ ps' @ next, dfa=dfa, guard=guard}
                        end
                     |  WILD => {pats=prev @ wilds @ next,dfa=dfa,guard=guard}
                     |  _   => error("mixng tuple and constructors")
                 end
                 val rows  = map processRow rows
                 val path_i' = List.tabulate (arity, fn i => Path.dot(path_i, INT i))
                 val paths = prevPaths @ path_i' @ nextPaths
                 val bindings = List.tabulate (arity, fn i => 
                                       (Path.dot(path_i, INT i), INT i))
             in  REBIND(path_i,bindings,
                        MATRIX{rows=rows, paths=paths}
                       )
             end
           | SOME(RECORD _) => (* expand a tuple along all the columns *)
             let (* All the labels that are in this column *)
                 val labels = 
                     LSet.listItems
                     (List.foldr 
                      (fn (RECORD lps, L) => 
                            List.foldr (fn ((l,p), L) => LSet.add(L,l)) L lps
                        | (_, L) => L)
                        LSet.empty ithCol)

                 val _ = if debug then
                            print("Labels="^listify("",",","") 
                                     (map Var.toString labels)^"\n")
                         else ()

                 fun lp2s(l,p) = Var.toString l^"="^Pat.toString p
                 fun lps2s lps = listify("","\t","") (map lp2s lps)
                 fun ps2s ps = listify("","\t","") (map Pat.toString ps)

                 val wilds = map (fn _ => WILD) labels

                 fun processRow{pats, dfa, guard} =
                 let val (prev, pat_i, next) = split_i(pats)
                 in  case pat_i of
                        RECORD lps =>
                        (* Put lps in canonical order *)
                        let val lps = Pat.sortByLabel lps
                            val _   = if debug then
                                         print("lpats="^lps2s lps^"\n")
                                      else ()
 
                            fun collect([], [], ps') = rev ps'
                              | collect(x::xs, [], ps') = 
                                   collect(xs, [], WILD::ps')
                              | collect(x::xs, this as (l,p)::lps, ps') =
                                (case Var.compare(x,l) of
                                  EQUAL => collect(xs, lps, p::ps')
                                | LESS  => collect(xs, this, WILD::ps')
                                | GREATER => error "labels out of order"
                                )
                              | collect _ = bug "processRow"
                            val ps = collect(labels, lps, [])
                            val _   = if debug then
                                         print("new pats="^ps2s ps^"\n")
                                      else ()
                        in  {pats=prev @ ps @ next, dfa=dfa, guard=guard}
                        end
                     |  WILD => {pats=prev @ wilds @ next,dfa=dfa,guard=guard}
                     |  _   => error("mixing tuple and constructors")
                 end
                  
                 val rows  = map processRow rows

                 val path_i' = map (fn l => Path.dot(path_i, LABEL l)) labels
                 val paths = prevPaths @ path_i' @ nextPaths

                 val bindings = map (fn l => 
                                       (Path.dot(path_i, LABEL l), LABEL l))
                                     labels
             in  REBIND(path_i,bindings,
                        MATRIX{rows=rows, paths=paths}
                       )
             end
           | SOME(APP(decon,_)) => 
           (* Find out how many variants are there in this case *)
             let val (allVariants, hasDefault) =
                   case decon of
                     CON c   => (map CON (Con.allVariants c), false)
                   | LIT i   => 
                      (Decon.Set.listItems 
                        (List.foldr 
                           (fn (APP(x as LIT _,_),S) => Decon.Set.add(S,x)
                             | (_,S) => S) Decon.Set.empty ithCol),
                       true) 

                (* function from con -> matrix; initially no rows 
                 *)
                fun insert(tbl, key, x) = Decon.Map.insert(tbl, key, x)
                fun lookup(tbl, key) = 
                    case Decon.Map.find(tbl, key) of 
                      SOME x => x
                    | NONE => bug("can't find constructor "^Decon.toString key)
                val empty = Decon.Map.empty
     
                fun create([], tbl) = tbl
                  | create((con as CON c)::cons, tbl) =
                    let val n = Con.arity c
                        val paths = List.tabulate(n, fn i => Path.dot(path_i, INT i))
                    in  create(cons, insert(tbl, con, {args=paths, rows=[]}))
                    end
                  | create((con as LIT l)::cons, tbl) =
                        create(cons, insert(tbl, con, {args=[], rows=[]}))
     
                val tbl = create(allVariants, empty)
     
                fun insertRow(tbl, decon, row) =
                    let val {args, rows} = lookup(tbl, decon)
                    in  insert(tbl, decon, {args=args, rows=rows@[row]})
                    end
    
                fun foreachRow([], tbl) = tbl
                  | foreachRow({pats, dfa, guard}::rows, tbl) =
                    let val (prev, pat_i, next) = split_i pats
     
                        fun addRow(tbl, decon, pats) = 
                            insertRow(tbl, decon, 
                                   {pats=pats, dfa=dfa, guard=guard})
     
                        fun addWildToEveryRow(tbl) =
                            foldr (fn (c, tbl) => 
                                   let val {args, rows} = lookup(tbl, c)
                                       val wilds = map (fn _ => WILD) args
                                       val pats  = prev @ wilds @ next
                                   in  addRow(tbl, c, pats)
                                   end) tbl allVariants
      
                        val tbl = 
                           case pat_i of
                             WILD => addWildToEveryRow tbl
                           | APP(decon, args) =>
                             let val pats = prev @ args @ next
                             in  addRow(tbl, decon, pats)
                             end
                           | _ => error 
                             "expecting constructor but found tuple/record"
                    in  foreachRow(rows, tbl)
                    end
     
                val tbl = foreachRow(rows, tbl)
     
                fun collectCases(decon, {args, rows}, rules) = 
                let val matrix = 
                        MATRIX{rows=rows, paths=prevPaths @args@nextPaths}
                in  (decon, args, matrix)::rules
                end

                val cases = Decon.Map.foldri collectCases [] tbl

                (* If we have a default then the default matrix
                 * contains the original matrix with rows whose
                 * column i is the wild card.
                 *)
                val default =
                    if hasDefault then 
                       SOME(
                        MATRIX{rows=List.filter 
                                     (fn {pats, ...} =>
                                        case List.nth(pats, i) of
                                          WILD => true
                                        | _ => false) rows,
                               paths=paths}
                       )   
                    else NONE
     
             in  SWITCH(Decon.Map.foldri collectCases [] tbl, default)
             end
           | _ => error "expandColumn.1"
       end (* expandColumn *)
   end (* Matrix *)

   val toString = DFA.toString

   (*
    * Rename user pattern into internal pattern.
    * The path business is hidden from the client.
    *)
   fun rename doIt (pats, guard, exp) = 
   let val empty = Path.Map.empty
       val bind  = Path.Map.insert

       fun process(path, subst, pat) = 
       let fun idPat id = (WILD, bind(subst, path, id))
           fun asPat(id, p) = 
           let val (p, subst) = process(path, subst, p)
           in  (p, bind(subst, path, id))
           end
           fun wildPat() = (WILD, subst)
           fun litPat(lit) = (APP(LIT lit, []), subst)

           fun processPats(pats) = 
           let fun loop([], _, ps', subst) = (rev ps', subst)
                 | loop(p::ps, i, ps', subst) = 
                   let val path' = Path.dot(path, INT i)
                       val (p, subst) = process(path', subst, p)
                   in  loop(ps, i+1, p::ps', subst)
                   end
           in  loop(pats, 0, [], subst) end

           fun processLPats(lpats) = 
           let fun loop([], ps', subst) = (rev ps', subst)
                 | loop((l,p)::ps, ps', subst) = 
                   let val path' = Path.dot(path, LABEL l)
                       val (p, subst) = process(path', subst, p)
                   in  loop(ps, (l,p)::ps', subst)
                   end
           in  loop(lpats, [], subst) end
 
           fun consPat(c,args) = 
           let val (pats, subst) = processPats(args)
           in  (APP(c, pats), subst) end

           fun tuplePat(pats) = 
           let val (pats, subst) = processPats(pats)
           in  (TUPLE pats, subst) end

           fun recordPat(lpats) = 
           let val (lpats, subst) = processLPats(lpats)
           in  (RECORD lpats, subst) end

       in  doIt {idPat=idPat,
                 asPat=asPat,
                 wildPat=wildPat,
                 consPat=consPat,
                 tuplePat=tuplePat,
                 recordPat=recordPat,
                 litPat=litPat
                } pat
       end

       fun processAllPats(i, [], subst, ps') = (rev ps', subst)
         | processAllPats(i, p::ps, subst, ps') =
           let val (p, subst) = process(PATH[INT i], subst, p)
           in  processAllPats(i+1, ps, subst, p::ps')  end

       val (pats, subst) = processAllPats(0, pats, empty, [])  
   in  (pats, Option.map (GUARD o ref) guard, subst, exp)
   end

   structure DFAMap = 
      RedBlackMapFn(type ord_key = dfa 
                    fun st(DFA{stamp, ...}) = stamp
                    fun compare(x,y) = Int.compare(st x, st y)
                   )

   (*
    * Give the arguments to case, factor out the common case and make it 
    * the default.
    *)
   fun factorCase(p, cases, d as SOME _) = (p, cases, d)
     | factorCase(p, cases, NONE) = 
       let fun count(m,dfa) = getOpt(DFAMap.find(m,dfa),0)
           fun inc((_,_,dfa),m) = DFAMap.insert(m, dfa, 1 + count(m, dfa))
           val m = foldr inc DFAMap.empty cases
           val best = DFAMap.foldri 
                   (fn (dfa,c,NONE) => SOME(dfa,c)
                     | (dfa,c,best as SOME(_,c')) =>
                       if c > c' then SOME(dfa,c) else best)
                      NONE m  
           fun neq(DFA{stamp=x, ...},DFA{stamp=y,...}) = x<>y
       in  case best of
             NONE => (p, cases, NONE) 
           | SOME(_,1) => (p, cases, NONE) 
           | SOME(defaultCase,n) => 
             let val others = List.filter(fn (_,_,x) => neq(x,defaultCase))
                                cases
             in  (p, others, SOME defaultCase) 
             end
       end 

   (* 
    * The main pattern matching compiler.
    * The dfa states are constructed with hash consing at the same time
    * so no separate DFA minimization step is needed.
    *)
   fun compile{compiled_rules, compress} =
   let exception NoSuchState

       fun simp x = if compress then factorCase x else x

       (* Table for hash consing *)
       val dfaTable = DFA.HashTable.mkTable(32,NoSuchState) :
                           dfa DFA.HashTable.hash_table
       val lookupState = DFA.HashTable.lookup dfaTable
       val insertState = DFA.HashTable.insert dfaTable

       val stampCounter = ref 0

       fun mkState(test) =   
       let val stamp = !stampCounter
       in  stampCounter := stamp + 1;
           DFA{stamp=stamp, freeVars=ref Path.Set.empty, 
               refCount=ref 0, generated=ref false, test=test}
       end

       fun newState test =
       let val s = mkState(test)
       in  lookupState s handle NoSuchState => (insertState(s, s); s)
       end

       (* State constructors *)
       fun Fail() = newState(FAIL)
       fun Ok(subst, action) = newState(OK(subst, action))
       fun Case(_, [], SOME x) = x
         | Case(_, [], NONE) = Fail()
         | Case(p, cases as (_,_,c)::cs, default) = 
           (* if List.all(fn (_,_,c') => DFA.eq(c,c')) cs andalso
              (case default of
                 SOME x => DFA.eq(c,x)     
               | NONE => true
              )
           then c
           else *) newState(CASE(simp(p, cases, default)))
       fun Root(used, dfa) = newState(ROOT(used, dfa))
       fun Bind(p, bindings, x) = newState(BIND(p, bindings, x))
       fun Where(subst, g, yes, no) = 
           if DFA.eq(yes,no) then yes else newState(WHERE(subst, g, yes, no))

       (*
        * Generate the DFA
        *)
       fun match matrix =
           if Matrix.isEmpty matrix then Fail()
           else
           case Matrix.findBestMatchColumn matrix of
             NONE =>   (* first row is all wild cards *) 
               (case Matrix.row(matrix, 0) of
                 {guard=SOME(subst, g), dfa, ...} => (* generate guard *)
                   Where(subst, g, dfa, 
                         match(Matrix.removeFirstRow matrix
                        ))
               | {guard=NONE, dfa, ...} => dfa
               )
           | SOME i => 
              (* mixture rule; split at column i *)
             (case Matrix.expandColumn(matrix, i) of
               (* splitting a constructor *)
               Matrix.SWITCH(cases, default) =>
               let val cases = map (fn (c,p,m) => (c,p,match m)) cases
               in  Case(Matrix.pathOf(matrix, i), cases, 
                        Option.map match default)
               end
               (* splitting a tuple or record;
                * recompute new bindings.
                *)
             | Matrix.REBIND(p,bindings,m) => Bind(p, bindings, match m)
             )

       fun makeMatrix rules =
       let val (pats0, _, _, _) = hd rules
           val arity = length pats0
           fun makeRow(pats, NONE, subst, action) =
               {pats=pats, guard=NONE, dfa=Ok(subst, action)}
             | makeRow(pats, SOME g, subst, action) = 
               {pats=pats, guard=SOME(subst,g), dfa=Ok(Path.Map.empty, action)}
       in  MATRIX{rows  = map makeRow rules,
                  paths = List.tabulate(arity, fn i => PATH[INT i]) 
                 }
       end

       val dfa = match(makeMatrix compiled_rules)

       (*
        * 1. Update the reference counts. 
        * 2. Compute the set of free path variables at each state. 
        * 3. Compute the set of path variables that are actually used.
        *)
       exception NotVisited
       val visited = IntHashTable.mkTable (32, NotVisited)
       fun mark s = IntHashTable.insert visited (s,true)
       fun isVisited s = getOpt(IntHashTable.find visited s, false)

       fun set(fv, s) = (fv := s; s)
       val union = Path.Set.union
       val add   = Path.Set.add
       val empty = Path.Set.empty

       val used = ref Path.Set.empty
       fun occurs s = used := Path.Set.union(!used,s)

       fun vars subst = Path.Set.addList(empty,Path.Map.listKeys subst)

       fun visit(DFA{stamp, refCount, test, freeVars, ...}) = 
           (refCount := !refCount + 1;
            if isVisited stamp then !freeVars
            else (mark stamp;
                  case test of
                    FAIL => empty
                  | OK(subst, _) => 
                    let val s = vars subst
                    in  occurs s; set(freeVars, s) end
                  | CASE(p, cases, opt) =>
                    let val fvs = 
                         List.foldr(fn ((_,_,x),s) => union(visit x,s))
                             empty cases 
                        val fvs =  
                            case opt of NONE => fvs 
                                      | SOME x => union(visit x,fvs)
                        val fvs = add(fvs, p) 
                    in  occurs fvs; set(freeVars, fvs) 
                    end 
                  | WHERE(subst, _, y, n) => 
                    let val s = union(vars subst, union(visit y, visit n))
                    in  occurs s; set(freeVars, s) end
                  | BIND(p, bs, x) => 
                    let val s = add(visit x, p)
                        val bs = foldr (fn ((p,_),S) => add(S,p)) s bs 
                    in  occurs bs; set(freeVars, s) end 
                  | ROOT _ => bug "visit"
                 )
           )
   in  visit dfa; Root(!used, dfa)
   end

   (*
    * Generate final code for pattern matching.
    *)
   fun codeGen 
        { genFail : unit -> 'exp,
          genOk,   
          genBind,   
          genCase,
          genIf   : Guard.guard * 'exp * 'exp -> 'exp,
          genGoto : int * Var.var list -> 'exp, (* call a function *)
          genFun  : int * Var.var list * 'exp -> 'decl, (* function def *)
          genLet  : 'decl list * 'exp -> 'exp,
          genProj : path * (path option * index) list -> 'decl,
          genVar  : path -> Var.var,
          genVal  : Var.var * 'exp -> 'decl
        } (root, dfa) = 
   let
       val DFA{test=ROOT(used,dfa), ...} = dfa

       fun arg p = if Path.Set.member(used, p) then SOME p else NONE 
       (* fun arg p = SOME p *)

       fun mkVars freeVarSet = 
           map genVar (Path.Set.listItems (!freeVarSet))


       fun enque((F,B),x) = (F,x::B)

       (* Walk a state, if it is shared then just generate a goto to the
        * state function; otherwise expand it 
        *)  
       fun walk(dfa as DFA{stamp, refCount, generated, freeVars, ...},
                           workList) = 
           if !refCount > 1 then 
              (* just generate a goto *)
              let val code = genGoto(stamp, mkVars freeVars)
              in  if !generated then (code, workList)
                  else (generated := true; (code, enque(workList, dfa)))
              end
           else
              expandDfa(dfa, workList) 

           (* generate a new function definition *)
       and genNewFun(dfa as DFA{stamp, freeVars, ...}, workList) =
           let val (body, workList) = expandDfa(dfa, workList)
           in  (genFun(stamp, mkVars freeVars, body), workList) 
           end

       and expandYesNo(yes, no, workList) =
           let val (yes, workList) = walk(yes, workList)
               val (no, workList) = walk(no, workList)
           in  (yes, no, workList)
           end
 
           (* expand the dfa always *)
       and expandDfa(DFA{stamp, test, ...}, workList) =  
              (case test of
                (* action *)
                OK(subst,action) => 
                let val bindings = 
                       Path.Map.foldri (fn (p,v,b) => (v,p)::b) [] subst
                in  (genLet(genBind bindings, genOk(action)), workList)
                end
                (* failure *)
              | FAIL => (genFail(), workList)
                (* guard *)
              | WHERE(subst, GUARD(ref g), yes, no) =>
                let val (yes, no, workList) = expandYesNo(yes, no, workList)
                    val bindings = 
                       Path.Map.foldri (fn (p,v,b) => (v,p)::b) [] subst
                in  (genLet(genBind bindings, genIf(g, yes, no)), workList)
                end
                (* case *)
              | CASE(path, cases, default) =>
                let val (cases, workList) = 
                      List.foldr 
                      (fn ((con, paths, dfa), (cases, workList)) =>
                           let val (code, workList) = walk(dfa, workList)
                           in  ((con, map arg paths, code)::cases, workList) 
                           end
                      ) ([], workList) cases

                    (* find the most common case and make it the default *)

                    val (default, workList) = 
                        case default of
                          NONE => (NONE, workList)
                        | SOME dfa => 
                          let val (code, workList) = walk(dfa, workList)
                          in  (SOME code, workList) end
                                     
                in  (genCase(genVar path, cases, default), workList)
                end
              | BIND(path, bindings, body) =>
                let val (body, workList) = walk(body, workList)
                    val bindings = map (fn (p,v) => (SOME p,v)) bindings
                in  (genLet([genProj(path, bindings)], body), workList)
                end
              | ROOT _ => bug "expandDfa"
              )

           (* Generate code for the dfa; accumulate all the auxiliary   
            * functions together and generate a let.
            *)
       fun genAll(root,dfa) =
           let val (exp, workList) = walk(dfa, ([],[]))
               fun genAuxFunctions(([],[]), funs) = funs
                 | genAuxFunctions(([],B), funs) = 
                   genAuxFunctions((rev B,[]), funs) 
                 | genAuxFunctions((dfa::F,B), funs) =
                   let val (fun1, workList) = genNewFun(dfa, (F,B))
                   in  genAuxFunctions(workList, fun1::funs)
                   end
               val rootDecl = genVal(genVar(PATH [INT 0]), root)
               val decls = genAuxFunctions(workList, []) 
           in  genLet(rootDecl::decls, exp)
           end
   in  genAll(root,dfa)
   end

end

end (* local *)
