(*
 * Generate the <arch>RTLProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for SSA optimizations.
 *)

functor MDLGenRTLProps(RTLComp : MDL_RTL_COMP) : MDL_GEN_MODULE2 =
struct

   structure RTLComp = RTLComp
   structure Comp    = RTLComp.Comp
   structure Consts  = Comp.Consts
   structure Ast     = Comp.Ast
   structure Env     = Comp.Env
   structure Tr      = Comp.Trans
   structure RTL     = RTLComp.RTL
   structure T       = RTL.T
   structure C       = CellsBasis

   open Ast Comp.Util Comp.Error

   exception Undefined
   exception NotFound

   (* Function to make a new RTL *)
   val makeNewRTL = IDexp(IDENT(["RTL"],"new"))

   (*------------------------------------------------------------------------
    *
    * Generate a table of compiled RTLs templates
    *
    *------------------------------------------------------------------------*)
   fun genRTLTable compiled_rtls =
   let val md   = RTLComp.md compiled_rtls

       val rtls = RTLComp.rtls compiled_rtls

       val rtlStrName = Comp.strname md "RTL" 

       val constTbl = Consts.newConstTable()

       fun makeEntry(RTLComp.RTLDEF{id, args, rtl, ...}) =  
       let val lookup = RTL.argOf rtl

           fun param i = APPexp(IDexp(IDENT(["T"],"PARAM")),INTexp i)

           fun makeArg name =
           let val (exp,pos) = lookup name
               val e =
                   case pos of
                     RTL.IN i    => param i
                   | RTL.OUT i   => param i
                   | RTL.IO(i,_) => param i
           in  (name, e)
           end handle RTL.NotAnArgument =>
               (warning("'"^name^"' is unused in rtl "^id);
                (name,param 0)
               )

           val arg = Consts.const constTbl (RECORDexp(map makeArg args))
       in  VALdecl[VALbind(IDpat id,
                    APPexp(makeNewRTL,
                           APPexp(IDexp(IDENT([rtlStrName],id)),
                           arg)))
                  ]
       end

       val body = map makeEntry rtls  

   in  STRUCTUREdecl("Arch",[],
                     NONE,DECLsexp
                       [LOCALdecl(Comp.Consts.genConsts constTbl,body)
                       ])
   end

   (*------------------------------------------------------------------------
    *
    * Create the function rtl : instruction -> rtl
    *
    *------------------------------------------------------------------------*)
   fun mkRtlQueryFun compiled_rtls =
   let fun body{instr, rtl=RTLComp.RTLDEF{id,...}, const} = 
           {exp=IDexp(IDENT(["Arch"],id)), casePats=[]}
   in  RTLComp.mkQuery compiled_rtls
          {name          = "rtl",
           namedArguments= true,
           args          = [["instr"]], 
           decls         = [RTLComp.complexErrorHandler "rtl"],
           caseArgs      = [],
           body          = body
          }
   end

   (*------------------------------------------------------------------------
    *
    * Create the function defUse : instruction -> cell list * cell list
    *
    *------------------------------------------------------------------------*)
   fun mkDefUseQueryFun compiled_rtls name user =
   let val md = RTLComp.md compiled_rtls

       fun cellOf(k,r) = 
       let val CELLdecl{from,...} = 
               Comp.lookupCellKind md (C.cellkindToString k)
       in  INTexp(!from + r)
       end

       fun join("::",x,y) = cons(x,y)
         | join(typ,x,y)  = APP(typ,TUPLEexp[x,y])

       fun body{instr, rtl=RTLComp.RTLDEF{id,rtl,...}, const} = 
       let val (d, u) = RTL.defUse rtl

           fun arg(k,x) = user(IDexp(IDENT(["C"],C.cellkindToString k)),x)

           fun collect(T.ARG(_,ref(T.REP k),x), e) = 
                  join("get"^k,arg(C.GP,ID x),e)
             | collect(T.$(_,C.MEM,_),e) = e (* XXX *)
             | collect(T.$(_,k,T.ARG(_,_,x)), e) = join("::", arg(k,ID x), e)
             | collect(T.$(_,k,T.LI r), e) = join("::",arg(k, cellOf(k,r)), e)
             | collect(t,e) = fail("collect "^RTL.Util.rexpToString t)

           val def = foldr collect (LISTexp([],NONE)) d
           val use = foldr collect (LISTexp([],NONE)) u
           val exp = TUPLEexp[def,use]
       in  {exp=exp, casePats=[]}
       end
   in  RTLComp.mkQuery compiled_rtls
          {name          = name,
           namedArguments= true,
           args          = [["instr"]], 
           decls         = [RTLComp.complexErrorHandler name],
           caseArgs      = [],
           body          = body
          }
   end


   (*------------------------------------------------------------------------
    *
    * Main routine
    *
    *------------------------------------------------------------------------*)
   fun gen compiled_rtls =
   let (* The machine description *)
       val md = RTLComp.md compiled_rtls

       (* name of the structure/signature *)
       val strName = Comp.strname md "RTLProps"  
       val sigName = "RTL_PROPERTIES"
 
       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES",
            "structure RTL : MLTREE_RTL",
            "structure Asm : INSTRUCTION_EMITTER where I = Instr",
            "  sharing Instr.Region = RegionProps.Region",
            "  sharing type Instr.C.cellkind = RTL.T.CellsBasis.cellkind"
           ]

       (* The functor *)
       val strBody = 
           [$ ["structure I   = Instr",
               "structure C   = I.C",
               "structure RTL = RTL",
               "structure T   = RTL.T",
               "",
               "datatype opnkind =",
               "  IMM     (* a constant operand *)",
               "| REG     (* can be renamed *)",
               "| FIX     (* cannot be renamed *)",
               "| MEM     (* memory *)",
               "| CTRL    (* control dependence *)",
               ""
              ],
            Comp.errorHandler md "RTLProps",
            RTLComp.complexErrorHandlerDef (),
            STRUCTUREdecl(Comp.strname md "RTL",[],NONE,
               APPsexp(IDENT([],Comp.strname md "RTL"),
                  DECLsexp[
                  $[ "structure RTL = RTL",
                     "structure C   = C"
                   ]]
                  )
            ),
            genRTLTable compiled_rtls,
            mkRtlQueryFun compiled_rtls,
            mkDefUseQueryFun compiled_rtls "defUse" (fn (k,x) => x), 
            mkDefUseQueryFun compiled_rtls "defUseWithCellKind" 
                  (fn (k,x) => TUPLEexp[k,x]) 
           ]

   in  Comp.codegen md "mltree/RTLProps"
         [Comp.mkFct md "RTLProps" args sigName
             (map Tr.simplifyDecl strBody)
         ]
   end
end
