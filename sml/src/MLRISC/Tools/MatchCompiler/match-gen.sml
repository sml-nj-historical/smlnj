(*
 * Interface with the match compiler to generate ML code.
 *)
functor MatchGen
    (structure AstPP       : MDL_AST_PRETTY_PRINTER
     structure AstUtil     : MDL_AST_UTIL
     structure AstRewriter : MDL_AST_REWRITER
       sharing AstPP.Ast = AstUtil.Ast = AstRewriter.Ast
    ) : MATCH_GEN =
struct
   structure Ast = AstPP.Ast
   structure A   = Ast
   structure R   = AstRewriter

   val NO = R.noRewrite

   val i2s = Int.toString

   structure Guard =
     struct
        type guard = A.exp
        val toString = PP.text o AstPP.exp
     end

   structure Literal =
     struct
        type literal = A.literal
        val toString = PP.text o AstPP.literal
        val compare  = AstUtil.compareLiteral
     end

   datatype conrep = CONREP of A.id list * A.consbind * A.datatypebind 

   structure Con =
     struct
        type con = conrep 

        fun toString(CONREP(path,A.CONSbind{id, ...},_)) = 
             PP.text(AstPP.ident(A.IDENT(path,id)))

        fun compare(x,y) = String.compare(toString x, toString y)

        fun allVariants(CONREP(path,_,dt as A.DATATYPEbind{cbs, ...})) =
             map (fn c => CONREP(path,c,dt)) cbs

        fun arity(CONREP(_,A.CONSbind{ty=NONE, ...},_)) = 0
          | arity(CONREP(_,A.CONSbind{ty=SOME ty, ...},_)) = 1

        fun compareIdent(A.IDENT(_,x),A.IDENT(_,y)) = String.compare(x,y)

        structure Map = RedBlackMapFn(type ord_key = A.ident 
                                      val compare = compareIdent)
     end

   structure Action =
     struct
        type action = A.exp
        val toString = PP.text o AstPP.exp
     end

   structure Var =
     struct
        type var = A.id
        val compare = String.compare 
        fun toString x = x
        structure Map = RedBlackMapFn(type ord_key = var 
                                      val compare = compare)
     end

   structure MC  =
     MatchCompiler(structure Guard   = Guard
                   structure Literal = Literal
                   structure Con     = Con
                   structure Var     = Var
                   structure Action  = Action
                  )

   fun ID x = A.IDexp(A.IDENT([],x))
   fun STATE x = "state_"^(i2s x)

   exception MatchCompiler = MC.MatchCompiler

   type compiled_type_info = conrep Con.Map.map

   fun compileTypes datatypebinds =
       List.foldr 
           (fn (t as A.DATATYPEbind{cbs, ...}, tyTbl) =>
                List.foldr (fn (c as A.CONSbind{id,...},tbl) =>
                     Con.Map.insert (tbl, A.IDENT([],id), CONREP([],c,t)))
                    tyTbl cbs
           ) Con.Map.empty datatypebinds

   
   fun compile tyTbl clauses =
   let (* rename all rules *)

       fun hasCon x = isSome(Con.Map.find(tyTbl, A.IDENT([],x))) 
       fun lookupCon (x as A.IDENT(p,_)) =
           case Con.Map.find(tyTbl, x) of
             SOME (CONREP(_,c,t)) => MC.CON(CONREP(p,c,t))
           | NONE => 
               raise MatchCompiler
                  ("undefined constructor "^PP.text(AstPP.ident x))

       (* Rewrite list patterns *)
       fun transListPat p = 
       let fun Cons(x,y) = A.CONSpat(A.IDENT([],"::"), SOME(A.TUPLEpat[x,y]))
           val Nil = A.CONSpat(A.IDENT([],"nil"),NONE)

           fun listify([], SOME p) = p
             | listify([], NONE) = Nil
             | listify(p::ps, t) = Cons(p, listify(ps, t))
           fun pat _ (A.LISTpat(ps, t)) = listify(ps, t)
             | pat _ p = p
       in  #pat(R.rewrite{pat=pat,exp=NO,decl=NO,sexp=NO,ty=NO}) p
       end 

       val empty = MC.Path.Map.empty
       val bind = MC.Path.Map.insert

       fun renameRule(c as A.CLAUSE([pat],guard,e)) = 
           MC.rename
               (fn {idPat, asPat, consPat, wildPat, 
                    tuplePat, recordPat, litPat, ...} =>
                   fn A.IDpat id    => 
                       if hasCon id then consPat(lookupCon(A.IDENT([],id)),[])
                       else idPat id
                    | A.ASpat(id,p) => asPat(id,p)
                    | A.WILDpat         => wildPat()
                    | A.CONSpat(c,NONE) => consPat(lookupCon c,[])
                    | A.CONSpat(c,SOME(p)) => consPat(lookupCon c,[p])
                    | A.TUPLEpat ps => tuplePat ps
                    | A.RECORDpat(lps,_) => recordPat lps
                    | A.LITpat lit => litPat lit
                    | p => raise MC.MatchCompiler("illegal pattern "^
                                       PP.text(AstPP.pat p))
               ) ([transListPat pat],guard,e)
           handle MC.MatchCompiler msg =>
              raise MC.MatchCompiler(msg^" in "^ PP.text(AstPP.clause c))

       val rules = map renameRule clauses
       
       (* compile the rules into a dfa *)
       val dfa = MC.compile{compiled_rules=rules, compress=true}
   in  dfa
   end

   fun codeGen {root, dfa, fail=genFail} =
   let (* make unique name for path variables *)
       val nameCounter = ref 0
       val nameTbl = ref MC.Path.Map.empty

       fun getName path =
           case MC.Path.Map.find(!nameTbl, path) of
             SOME name => name
           | NONE =>
             let val v = "v_"^i2s(!nameCounter)
             in  nameCounter := !nameCounter + 1;
                 nameTbl := MC.Path.Map.insert(!nameTbl, path, v);
                 v
             end

       (* Now generate the code; we just have to hook things up with the MC *)
       fun genVar path = getName path
       fun genBind [] = []
         | genBind bindings =
           [A.VALdecl(map (fn (v,p) =>
                      A.VALbind(A.IDpat v,ID(genVar p))) bindings )]
       fun genOk(e) = e
       fun pathToPat(path) = A.IDpat(getName path)
       fun arg NONE = A.WILDpat
         | arg (SOME p) = A.IDpat(getName p)
       fun fromRep(CONREP(path,A.CONSbind{id, ...},_)) = A.IDENT(path,id)
       fun genConPat(MC.CON con, []) = A.CONSpat(fromRep con,NONE)
         | genConPat(MC.CON con, paths) = 
                A.CONSpat(fromRep con, SOME(A.TUPLEpat(map arg paths)))
         | genConPat(MC.LIT lit, _) = A.LITpat lit
       fun genCase(v, cases, default) = 
           A.CASEexp(ID v,
              map (fn (con, paths, e) =>
                    A.CLAUSE([genConPat(con, paths)],NONE,e)) cases @
                  (case default of
                     NONE => []
                  |  SOME default => [A.CLAUSE([A.WILDpat], NONE, default)]
                  )
              )   
       fun genIf(e, y, n) = A.IFexp(e, y, n)
       fun genGoto(f, args) = A.APPexp(ID(STATE f), A.TUPLEexp(map ID args)) 
       fun genFun(f, args, body) = 
           A.FUNdecl[A.FUNbind(STATE f,
                       [A.CLAUSE([A.TUPLEpat(map A.IDpat args)],NONE,body)])
                     ]
       fun genLet([], e) = e
         | genLet(d, e) = A.LETexp(d,[e])
       fun genVal(v, e) = A.VALdecl[A.VALbind(A.IDpat v, e)]
       fun genProj(path, bindings) =
       let val pat = case bindings of
                       [] => A.WILDpat
                     | (p, MC.INT _)::ps  => 
                       A.TUPLEpat(map (fn (p,_) => arg p) bindings)
                     | (p, MC.LABEL _)::ps =>
                       A.RECORDpat(map (fn (p,MC.LABEL l) => 
                                        (l, arg p)) bindings, true)
       in  A.VALdecl[A.VALbind(pat,ID(getName path))]
       end
   in  MC.codeGen 
         {genFail = genFail,
          genOk   = genOk,
          genBind = genBind,
          genCase = genCase, 
          genIf   = genIf,
          genGoto = genGoto,
          genFun  = genFun,
          genLet  = genLet,
          genVar  = genVar,
          genVal  = genVal,
          genProj = genProj
         } (root, dfa)
   end

end
