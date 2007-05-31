(* amd64SpillInstr.sml
 * 
 * This functor contains callback functions for spilling and reloading 
 * instructions.
 *)

functor AMD64SpillInstr (
      structure I : AMD64INSTR
      structure Props : AMD64INSN_PROPERTIES
          where I = I
    ) : ARCH_SPILL_INSTR =
  struct

    structure CB = CellsBasis
    structure I = I
    structure C = I.C
    
    fun error msg = MLRiscErrorMsg.impossible("AMD64Spill: "^ msg)
    
    fun liveKill(add, rmv) ({regs, spilled}, reg) = 
        {regs=rmv(reg, regs), spilled=add(reg, spilled)}
    val fLiveKill = liveKill (C.addFreg, C.rmvFreg)
    val rLiveKill = liveKill (C.addReg, C.rmvReg)
    
    val newReg = C.newReg
    val newFreg = C.newFreg
    
    fun annotate(instr,[]) = instr
      | annotate(instr,a::an) = annotate(I.ANNOTATION{i=instr,a=a},an)

    fun mark(instr, an) = annotate(I.INSTR instr, an)
    
    fun immed(I.Immed _) = true
      | immed(I.ImmedLabel _) = true
      | immed _ = false

    fun immedOrReg(I.Direct r) = true
      | immedOrReg(I.Immed _) = true
      | immedOrReg(I.ImmedLabel _) = true
      | immedOrReg _ = false

    fun isMemory(I.Displace _) = true
      | isMemory(I.Indexed _) = true
      | isMemory(I.LabelEA _) = true
      | isMemory _ = false

    fun mvInstr instr = let
        fun mvOp 8 = I.MOVB
          | mvOp 16 = I.MOVW
          | mvOp 32 = I.MOVL
          | mvOp 64 = I.MOVQ
          | mvOp _ = error "mvInstr"
        val sz = Props.szOfInstr instr
        in
          (sz, mvOp sz)
        end (* mvInstr *)
    
    fun fmvInstr instr = let
        fun mvOp 32 = I.MOVSS
          | mvOp 64 = I.MOVSD
          | mvOp _ = error "fmvInstr"
        val sz = Props.szOfFinstr instr
        in
          (sz, mvOp sz)
        end (* fmvInstr *)

    fun spillToEA CB.GP (r, ea) = let
        fun move () = {code=[I.move {mvOp=I.MOVQ, dst=ea, src=I.Direct (64, r)}],
                       proh=[], newReg=NONE}
        in
          (case ea
            of ( I.Displace _ | I.Indexed _ ) => move ()
             | _ => error "spillToEA"
          (* end case *))
        end 
      | spillToEA CB.FP _ = error "spillToEA: FP"
      | spillToEA _ _ = error "spillToEA"

    fun reloadFromEA CB.GP (r, ea) = let
        fun move () = {code=[I.move {mvOp=I.MOVQ, dst=I.Direct (64, r), src=ea}],
                       proh=[], newReg=NONE}
        in
          (case ea
            of ( I.Displace _ | I.Indexed _ ) => move ()
             | _ => error "reloadFromEA"
          (* end case *))
        end
      | reloadFromEA CB.FP _ = error "reloadFromEA: FP"
      | reloadFromEA _ _ = error "reloadFromEA"

    fun spillR (i, r, spillLoc) = let
        fun spill (instr, an) = let
            fun done (instr, an) = {code=[mark (instr, an)], proh=[], newReg=NONE}
            val (sz, sMvOp) = mvInstr instr
            in
              (case instr
                of I.CALL {opnd=addr, defs, uses, return, cutsTo, mem, pops} =>
		   done (I.CALL {opnd=addr, defs=C.rmvReg (r, defs), 
		    	        return=return, uses=uses, 
			        cutsTo=cutsTo, mem=mem, pops=pops}, an)
		 | I.MOVE {mvOp as (I.MOVZBQ|I.MOVSBQ|I.MOVZWQ|I.MOVSWQ|
		                    I.MOVSLQ|I.MOVZBL|I.MOVSBL|I.MOVZWL|
		                    I.MOVSWL), src, dst} => let
		   val tmpR = newReg ()
		   val tmp = I.Direct (sz, tmpR)
		   in
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark (I.MOVE {mvOp=mvOp, src=src, dst=tmp}, an),
		            I.move {mvOp=sMvOp, src=tmp, dst=spillLoc}]}
		   end
		 | I.MOVE {mvOp, src as I.Direct (_, rs), dst} =>
		   if CB.sameColor (rs, r) then {code=[], proh=[], newReg=NONE}
		   else done (I.MOVE {mvOp=mvOp, src=src, dst=spillLoc}, an)
		 | I.MOVE {mvOp, src, dst=I.Direct _} => 
		   if Props.eqOpn(src, spillLoc) 
		     then {code=[], proh=[], newReg=NONE}
		     else if immed src then 
			  done(I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}, an)
		      else let 
		        val tmpR = newReg ()
	                val tmp  = I.Direct (sz, tmpR)
			in  
			  {proh=[tmpR],
			   newReg=SOME tmpR,
			   code=[mark(I.MOVE {mvOp=mvOp, src=src, dst=tmp}, an),
				 I.move {mvOp=sMvOp, src=tmp, dst=spillLoc}]}
			end
	         | I.LEAL {addr, r32} => let 
	           val tmpR = newReg()
		   in  
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.LEAL {addr=addr, r32=tmpR}, an),
				 I.move{mvOp=I.MOVL, src=I.Direct (32, tmpR),
				        dst=spillLoc}]}
		   end 
		 | I.LEAQ {addr, r64} => let 
                   val tmpR = newReg()
		   in  
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.LEAQ{addr=addr, r64=tmpR}, an),
				 I.move{mvOp=I.MOVQ, src=I.Direct (64,tmpR),
				        dst=spillLoc}]}
		      end 
		 | I.BINARY {binOp=I.XORL, src as I.Direct (_,rs), 
		             dst=I.Direct (_,rd)} => 
		   if CB.sameColor (rs,rd) 
		     then {proh=[], newReg=NONE,
		           code=[mark(I.MOVE{mvOp=I.MOVL, src=I.Immed 0,
		                 dst=spillLoc}, an)]}
		      else
			  {proh=[], newReg=NONE,
			   code=[mark(I.BINARY{binOp=I.XORL, src=src,
			         dst=spillLoc}, an)]}
		 | I.BINARY{binOp=I.XORQ, src as I.Direct (_,rs), 
		            dst=I.Direct (_,rd)} => 
		   if CB.sameColor(rs,rd) 
		     then {proh=[], newReg=NONE,
			   code=[mark(I.MOVE{mvOp=I.MOVQ, src=I.Immed 0,
			         dst=spillLoc}, an)]}
		     else {proh=[], newReg=NONE,
			   code=[mark(I.BINARY{binOp=I.XORQ, src=src,
			         dst=spillLoc}, an)]}
			  
		 | I.BINARY {binOp, src, dst} => let 
		   (* note: dst = r *)
		   fun multBinOp(I.MULQ|I.MULL|I.MULW|I.MULB|
				 I.IMULQ|I.IMULL|I.IMULW|I.IMULB) = true
		     | multBinOp _ = false
		   in
		     if multBinOp binOp 
		       then let (* destination must remain a register *)
		         val tmpR = newReg()
		         val tmp = I.Direct (sz,tmpR)
			 in
		           {proh=[tmpR], newReg=SOME tmpR,
		            code=[I.move{mvOp=sMvOp, src=spillLoc, dst=tmp},
			          I.binary{binOp=binOp, src=src, dst=tmp},
			          I.move{mvOp=sMvOp, src=tmp, dst=spillLoc}]}
			 end
			else if immedOrReg src 
			      then (* can replace the destination directly *)
			        done(I.BINARY{binOp=binOp, src=src, dst=spillLoc},
			             an)
			  else let (* a memory src and non multBinOp  
				   * --- cannot have two memory operands
				   *)
				val tmpR = newReg()
				val tmp = I.Direct (sz,tmpR)
			        in 
				  { proh=[tmpR], newReg=NONE,
				    code=[I.move{mvOp=sMvOp, src=src, dst=tmp},
					  I.binary{binOp=binOp, src=tmp,
					           dst=spillLoc}]}
			        end
		        end 
		 | I.SHIFT {shiftOp, count=I.Direct (_, ecx), src, dst} => 
		   error "implement shift"
		 | I.SHIFT {shiftOp, count, src, dst} => 
		   if immedOrReg src
		     then done (I.SHIFT {shiftOp=shiftOp, src=src, dst=spillLoc,
		             count=count}, an)
		     else let
		       val tmpR = newReg ()
		       val tmp = I.Direct (sz, tmpR)
		       in
		         {proh=[tmpR], newReg=NONE,
		          code=[I.move {mvOp=sMvOp, src=src, dst=tmp},
		                I.shift {shiftOp=shiftOp, src=tmp, dst=spillLoc,
		                   count=count}]}
		       end
		 | I.CMOV {cond, src, dst} => 
		   (* note: dst must be a register *)
		   (case spillLoc 
		     of I.Direct (_,r) =>
			   {proh=[], newReg=NONE,
			    code=[mark(I.CMOV{cond=cond,src=src,dst=r},an)]
			   }
		       | _ => let 
		         val tmpR = newReg()
			 val tmp  = I.Direct (sz, tmpR)
			 in  
			   {proh=[tmpR], newReg=SOME tmpR,
		            code=[I.move{mvOp=I.MOVQ, src=spillLoc, dst=tmp},
				  mark(I.CMOV{cond=cond,src=src,dst=tmpR},an),
				  I.move{mvOp=I.MOVQ, src=tmp, dst=spillLoc}]}
			   end 
		    (* end case *))
		 | I.CMPXCHG{lock,sz=isz,src,dst} => 
		      if immedOrReg src 
		        then {proh=[], newReg=NONE,
			   code=[mark(I.CMPXCHG{lock=lock,sz=isz,src=src,
			        dst=spillLoc},an)]}
		      else let 
		        val tmpR = newReg()
			val tmp  = I.Direct (sz, tmpR)
			in 
			  {proh=[], newReg=NONE,
		          code=[I.move{mvOp=I.MOVQ, src=src, dst=tmp},
		  	        mark(I.CMPXCHG{lock=lock,sz=isz,src=tmp,
		  	             dst=spillLoc},an)]}
			end
		 | I.MULTDIV _ => error "spill: MULTDIV"
		 | I.MUL3 {src1, src2, dst} => let 
		   val tmpR = newReg() 
		   in  
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.MUL3{src1=src1, src2=src2, dst=tmpR}, an),
		            I.move{mvOp=I.MOVL, src=I.Direct (32,tmpR),
                            dst=spillLoc}]
			  }
		   end
		 | I.MULQ3 {src1, src2, dst} => let 
		   val tmpR = newReg() 
		   in  
		     {proh=[tmpR], newReg=SOME tmpR,
		      code=[mark(I.MULQ3{src1=src1, src2=src2, dst=tmpR}, an),
				 I.move{mvOp=I.MOVQ, src=I.Direct (64,tmpR),
				        dst=spillLoc}]}
		      end
		 | I.UNARY{unOp, opnd} => 
		   done(I.UNARY{unOp=unOp, opnd=spillLoc}, an)
		 | I.SET{cond, opnd} => 
		   done(I.SET{cond=cond, opnd=spillLoc}, an)
		 | I.POP _ => done (I.POP spillLoc, an)
		 | _ => error "AMD64SpillInstr.spillR"
	       (* end case *))
            end (* spill *)
        fun f (i, ans) = (case i
            of I.INSTR instr => spill (instr, ans)
             | I.ANNOTATION {i, a} => f (i, a :: ans)
             | I.KILL lk => {code=[annotate (I.KILL (rLiveKill (lk, r)), ans)],
                             proh=[], newReg=NONE}
             | _ => error "AMD64SpillInstr.spillR"
            (* end case *))
        in
          f (i, [])
        end (* spillR *)

    fun spillF (i, r, spillLoc) = let
    	fun spill (instr, an) = let
    	    val (sz, fmvOp) = fmvInstr instr
    	    fun withTmp f = let
    	        val tmpR = newFreg ()
    	        val tmp = I.FDirect tmpR
    	        in
    	          {code=[mark (f tmpR, an),
    	                 I.fmove {fmvOp=fmvOp, src=tmp, dst=spillLoc}],
    	           proh=[tmpR], newReg=SOME tmpR}
    	        end (* withTmp *)
    	    fun inTmpR (I.FDirect r) = (r, [])
    	      | inTmpR opnd = let
    	        val tmp = newFreg ()
    	        in
    	          (tmp, [I.FMOVE {fmvOp=fmvOp, src=opnd, dst=I.FDirect tmp}])
    	        end
    	    in
    	      (case instr
    	        of I.FMOVE {fmvOp, src=src as I.FDirect r', dst} => 
    	           if CB.sameColor (r, r')
    	             then {code=[], proh=[], newReg=NONE}
    	             else {code=[mark (I.FMOVE {fmvOp=fmvOp, src=src, 
    	                                  dst=spillLoc}, an)],
    	                   proh=[], newReg=NONE}
    	         | I.FMOVE {fmvOp, src, dst as I.FDirect _} => withTmp (fn tmpR =>
    	           I.FMOVE {fmvOp=fmvOp, src=src, dst=I.FDirect tmpR})
    	         | I.FBINOP {binOp, src, dst} => withTmp (fn tmpR =>
    	           I.FBINOP {binOp=binOp, src=src, dst=tmpR})
     	         | I.FCOM {comOp, dst, src} => withTmp (fn tmpR =>
     	           I.FCOM {comOp=comOp, src=src, dst=tmpR})
     	         | I.FSQRTS {dst, src} => withTmp (fn tmpR =>
     	           I.FSQRTS {src=src, dst=I.FDirect tmpR})
     	         | I.FSQRTD {dst, src} => withTmp (fn tmpR =>
     	           I.FSQRTD {src=src, dst=I.FDirect tmpR})
     	         | I.CALL {opnd, defs, uses, return, cutsTo, mem, pops} =>
		   {code=[mark (I.CALL {opnd=opnd, defs=C.rmvFreg (r, defs), 
			    return=return, uses=uses, 
			    cutsTo=cutsTo, mem=mem, pops=pops}, an)],
	            proh=[], newReg=NONE}
	         | I.CALLQ {opnd, defs, uses, return, cutsTo, mem, pops} =>
		   {code=[mark (I.CALLQ {opnd=opnd, defs=C.rmvFreg (r, defs), 
			    return=return, uses=uses, 
			    cutsTo=cutsTo, mem=mem, pops=pops}, an)],
	            proh=[], newReg=NONE}
     	         | _ => error "spillF"
    	      (* end case *))
    	    end (* spill *)
    	fun f (i, ans) = (case i
    	    of I.INSTR instr => spill (instr, ans)
    	     | I.ANNOTATION {i, a} => f (i, a :: ans)
    	     | I.KILL lk => {code=[annotate (I.KILL (fLiveKill (lk, r)), ans)],
		             proh=[], newReg=NONE}
             | _ => error "spillF"
            (* end case *))
        in
          f (i, [])
        end (* spillF *)
    
    fun spill CB.GP = spillR
      | spill CB.FP = spillF
      | spill _ = error "spill"

    fun reloadR (i, r, spillLoc) = let
        fun reload (instr, an) = let
            val (sz, rMvOp) = mvInstr instr
	    fun done (instr, an) = {code=[mark (instr, an)], proh=[], newReg=NONE}
	    fun replace (opnd as I.Direct (_, r')) = if CB.sameColor (r, r')
	        then spillLoc
	        else opnd
	      | replace opnd = opnd
	    fun operand (opnd, tmp) = let 
	        fun replaceR (r', f) = if CB.sameColor (r, r')
	            then f tmp
	            else opnd
	        in
		  (case opnd
	            of I.Direct (opndSz, opndR) => replaceR (opndR, fn r' =>
	               I.Direct (opndSz, r'))
	             | I.Displace {base, disp, mem} => replaceR (base, fn r' =>
	               I.Displace {base=r', disp=disp, mem=mem})
	             | I.Indexed {base=NONE, index, scale, disp, mem} =>
	               replaceR (index, fn index' =>
	                 I.Indexed {base=NONE, index=index', scale=scale,
	                            disp=disp, mem=mem})
	             | I.Indexed {base=SOME b, index, scale, disp, mem} => 
	               replaceR (b, fn b' =>
	                 replaceR (index, fn index' =>
	                   I.Indexed {base=SOME b', index=index', scale=scale,
	                              disp=disp, mem=mem}))
	             | opnd => opnd
	           (* end case *))
	         end (* operand *)
	    fun operand' (I.Direct _, _) = spillLoc
	      | operand' (opnd, tmp) = operand (opnd, tmp)
	    (* assume that tmpR gets killed *)
	    fun withTmp' avail f = (case spillLoc
	        of I.Direct (_, tmpR) => if avail
	           then {code=[mark (f tmpR, an)], proh=[tmpR], newReg=SOME tmpR}
	           else {code=[mark (f tmpR, an)], proh=[], newReg=NONE}
	         | _ => let
	           val tmpR = newReg ()
	           in
	             {code=[I.move {mvOp=rMvOp, src=spillLoc, 
	                            dst=I.Direct (sz, tmpR)},
	                    mark (f tmpR, an)], 
	              proh=[tmpR], newReg=SOME tmpR}
	           end
	        (* end case *))
	    val withTmp = withTmp' false
	    val withTmpAvail = withTmp' true
	    fun reloadCmp (cmp, lsrc, rsrc, an) = let
	        fun reload () = withTmp (fn tmp =>
	            cmp {lsrc=operand (lsrc, tmp), rsrc=operand (rsrc, tmp)})
	        in
	          if immedOrReg lsrc andalso immedOrReg rsrc
	            then let
	              val rsrc' = replace rsrc
	              val lsrc' = replace lsrc
	              in
	                if isMemory rsrc' andalso isMemory lsrc'
	                  then reload ()
	                  else done (cmp {lsrc=lsrc', rsrc=rsrc'}, an)
	              end
	            else reload ()
	        end (* reloadCmp *)
	    fun reloadTest (test, lsrc, rsrc, an) = let
	        fun reload () = withTmp (fn tmp =>
	            test {lsrc=operand (lsrc, tmp), rsrc=operand (rsrc, tmp)})
	        in
	          if immedOrReg lsrc andalso immedOrReg rsrc
	            then let
	              val rsrc' = replace rsrc
	              val lsrc' = replace lsrc
	              in
	                if isMemory rsrc' 
	                  then if isMemory lsrc' 
	                         then reload ()
	                         else done (test {lsrc=rsrc', rsrc=lsrc'}, an)
	                  else done (test {lsrc=lsrc', rsrc=rsrc'}, an)
	              end
	            else reload ()
	        end (* reloadTest *)
	    fun reloadBT (bitOp, lsrc, rsrc, an) = 
		reloadCmp (fn {lsrc,rsrc} => 
		  I.BITOP {bitOp=bitOp,lsrc=lsrc,rsrc=rsrc}, lsrc, rsrc, an)
	    fun reloadPush(push, arg as I.Direct _, an) =
		done(push(replace arg), an)
	      | reloadPush(push, arg, an) =
		withTmpAvail(fn tmpR => push (operand(arg, tmpR)))
            in
              (case instr
                of I.JMP (opnd, labs) => withTmp (fn tmp =>
                   I.JMP (operand' (opnd, tmp), labs))
                 | I.JCC {opnd, cond} => withTmp (fn tmp =>
                   I.JCC {opnd=operand' (opnd, tmp), cond=cond})
                 | I.CALL {opnd, defs, uses, return, cutsTo, mem, pops} => 
                   withTmp (fn tmp =>
                     I.CALL {opnd=operand (opnd, tmp), defs=defs, 
                             uses=C.rmvReg (r, uses), return=return, pops=pops,
                             cutsTo=cutsTo, mem=mem})
                 | I.MOVE {mvOp, src as I.Direct _, dst as I.Direct _} =>
                   done (I.MOVE {mvOp=mvOp, src=replace src, dst=dst}, an)
                 | I.MOVE {mvOp, src, dst as I.Direct _} => withTmpAvail (fn tmp =>
                   I.MOVE {mvOp=mvOp, src=operand (src, tmp), dst=dst})
                 | I.MOVE {mvOp, src as I.Direct _, dst} =>
                   if (Props.eqOpn (dst, spillLoc))
                      then {code=[], proh=[], newReg=NONE}
                      else withTmpAvail (fn tmp =>
                             I.MOVE {mvOp=mvOp, src=operand (src, tmp),
                                     dst=operand (dst, tmp)})
                 | I.MOVE {mvOp, src, dst} => withTmpAvail (fn tmp =>
                   I.MOVE {mvOp=mvOp, src=operand (src, tmp), 
                           dst=operand (dst, tmp)})
                 | I.LEAL {r32, addr} => withTmpAvail (fn tmp =>
                   I.LEAL {r32=r32, addr=operand (addr, tmp)})
                 | I.LEAQ {r64, addr} => withTmpAvail (fn tmp =>
                   I.LEAQ {r64=r64, addr=operand (addr, tmp)})
                 | I.CMPQ {lsrc, rsrc} => reloadCmp (I.CMPQ, lsrc, rsrc, an) 
		 | I.CMPL {lsrc, rsrc} => reloadCmp (I.CMPL, lsrc, rsrc, an) 
		 | I.CMPW {lsrc, rsrc} => reloadCmp (I.CMPW, lsrc, rsrc, an) 
		 | I.CMPB {lsrc, rsrc} => reloadCmp (I.CMPB, lsrc, rsrc, an) 
		 | I.TESTQ {lsrc, rsrc} => reloadTest (I.TESTQ, lsrc, rsrc, an) 
		 | I.TESTL {lsrc, rsrc} => reloadTest (I.TESTL, lsrc, rsrc, an) 
		 | I.TESTW {lsrc, rsrc} => reloadTest (I.TESTW, lsrc, rsrc, an) 
		 | I.TESTB {lsrc, rsrc} => reloadTest (I.TESTB, lsrc, rsrc, an) 
		 | I.BITOP{bitOp,lsrc, rsrc} => reloadBT (bitOp, lsrc, rsrc, an) 
		 | I.BINARY{binOp, src, dst} => withTmp (fn tmp =>
		   I.BINARY{binOp=binOp, src=operand(src, tmp), 
			    dst=operand(dst, tmp)})
		 | I.CMOV{cond, src, dst} => 
		   if CB.sameColor(dst,r) 
		     then error "CMOV"
		      else done (I.CMOV{cond=cond, src=spillLoc, dst=dst}, an)
		 | I.CMOVQ{cond, src, dst} => 
		   if CB.sameColor(dst,r) 
		     then error "CMOV"
		     else done (I.CMOVQ{cond=cond, src=spillLoc, dst=dst}, an)
		 | I.SHIFT {shiftOp, count as I.Direct _, src, dst} => 
		   withTmp (fn tmp =>
		     I.SHIFT {shiftOp=shiftOp, count=count, 
		              src=operand (src, tmp),
		               dst=operand (dst, tmp)})
		 | I.CMPXCHG{lock,sz,src,dst} => withTmp(fn tmpR =>
		   I.CMPXCHG{lock=lock, sz=sz,
			     src=operand(src, tmpR),
			     dst=operand(dst, tmpR)})
		 | I.MULTDIV{multDivOp, src as I.Direct _} => 
		   done (I.MULTDIV{multDivOp=multDivOp, src=replace src}, an)
		 | I.MULTDIV{multDivOp, src} => withTmp(fn tmpR => 
		   I.MULTDIV{multDivOp=multDivOp, src=operand(src, tmpR)})
		 | I.MUL3{src1, src2, dst} => withTmp (fn tmpR => 
		   I.MUL3{src1=operand(src1, tmpR), src2=src2, 
		          dst=if CB.sameColor(dst,r) 
			      then error "reload:MUL3" else dst})
		 | I.MULQ3{src1, src2, dst} => withTmp (fn tmpR => 
		   I.MULQ3{src1=operand(src1, tmpR), src2=src2, 
			   dst=if CB.sameColor(dst,r) 
		               then error "reload:MULQ3" else dst})
		 | I.UNARY{unOp, opnd} => withTmpAvail (fn tmpR =>
		   I.UNARY{unOp=unOp, opnd=operand(opnd, tmpR)})
		 | I.SET{cond, opnd} => withTmpAvail (fn tmpR => 
		   I.SET{cond=cond, opnd=operand(opnd, tmpR)})
		 | I.PUSHQ arg => reloadPush (I.PUSHQ, arg, an)
		 | I.PUSHL arg => reloadPush (I.PUSHL, arg, an)
		 | I.PUSHW arg => reloadPush (I.PUSHW, arg, an)
		 | I.PUSHB arg => reloadPush (I.PUSHB, arg, an)
		 | I.FMOVE {fmvOp, src, dst} => withTmpAvail (fn tmp =>
		   I.FMOVE {fmvOp=fmvOp, src=operand (src, tmp), 
		            dst=operand (dst, tmp)})
		 | I.FCOM {comOp, dst, src} => withTmpAvail (fn tmp =>
		   I.FCOM {comOp=comOp, dst=dst, src=operand (src, tmp)})
		 | _ => error "reloadR"
              (* end case *))
            end (* reload *)
        fun f (i, ans) = (case i
            of I.INSTR instr => reload (instr, ans)
             | I.ANNOTATION {a, i} => f (i, a :: ans)
             | I.LIVE lk => {code=[annotate (I.LIVE (rLiveKill (lk, r)), ans)],
                             proh=[], newReg=NONE}
             | _ => error "reloadR"
            (* end case *))
        in
          f (i, [])
        end (* reloadR *)

    fun reloadF (i, r, spillLoc) = let
        fun reload (instr, an) = let
            fun replace (opnd as I.FDirect r') = 
                if CB.sameColor (r, r') then spillLoc else opnd
              | replace opnd = opnd
            val (sz, fmvOp) = fmvInstr instr
            in
              (case instr
                of I.FMOVE {fmvOp, src, dst=dst as I.FDirect _} =>
                   {code=[mark (I.FMOVE {fmvOp=fmvOp, src=replace src, dst=dst},
                           an)],
                    proh=[], newReg=NONE}
                 | I.FMOVE {fmvOp, src, dst} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
                   in
                     {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                            mark (I.FMOVE {fmvOp=fmvOp, src=tmp, dst=dst}, an)],
                      proh=[tmpR], newReg=SOME tmpR}
                   end
                 | I.FBINOP {binOp, src, dst} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
                   in
                     {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                            mark (I.FBINOP {binOp=binOp, src=tmpR, dst=dst}, an)],
                      proh=[tmpR], newReg=SOME tmpR}
                   end
                 | I.FCOM {comOp, src, dst} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
                   in
                     {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                            mark (I.FCOM {comOp=comOp, src=tmp, dst=dst}, an)],
                      proh=[tmpR], newReg=SOME tmpR}
                   end
                 | I.FSQRTS {dst, src} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
                   in
                     {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                             mark (I.FSQRTS {src=tmp, dst=dst}, an)],
                      proh=[tmpR], newReg=SOME tmpR}
                   end
                 | I.FSQRTD {dst, src} => let
                   val tmpR = newFreg ()
                   val tmp = I.FDirect tmpR
                   in
                     {code=[I.fmove {fmvOp=fmvOp, src=spillLoc, dst=tmp},
                             mark (I.FSQRTD {src=tmp, dst=dst}, an)],
                      proh=[tmpR], newReg=SOME tmpR}
                   end
                 | I.CALL {opnd, defs, uses, return, cutsTo, mem, pops} =>
                   {code=[mark (I.CALL {opnd=opnd, defs=C.rmvReg (r, defs), 
                                        uses=uses, return=return, cutsTo=cutsTo,
                                        mem=mem, pops=pops}, an)],
                    proh=[], newReg=NONE}
                 | I.CALLQ {opnd, defs, uses, return, cutsTo, mem, pops} =>
                   {code=[mark (I.CALLQ {opnd=opnd, defs=C.rmvReg (r, defs), 
                                        uses=uses, return=return, cutsTo=cutsTo,
                                        mem=mem, pops=pops}, an)],
                    proh=[], newReg=NONE}
                 | _ => error "reloadF"
               (* end case *))
            end (* reload *)
        fun f (i, ans) = (case i
            of I.INSTR i => reload (i, ans)
             | I.ANNOTATION {i, a} => f (i, a :: ans)
             | I.LIVE lk => 
               {code=[annotate (I.LIVE (fLiveKill (lk, r)), ans)],
                proh=[], newReg=NONE}
             | _ => error "reloadF.f"
            (* end case *))
        in
          f (i, [])
        end (* reloadF *)

    fun reload CB.GP = reloadR
      | reload CB.FP = reloadF
      | reload _ = error "reload"

  end (* AMD64SpillInstr *)