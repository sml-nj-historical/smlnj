(* mipsgen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor MipsCodeGen(Endian : ENDIAN) : CODEGENERATOR =  
  FLINTComp(
    structure CoderInstr = MipsInstr(structure E=Endian)
    structure MachSpec = MipsSpec(Endian)
    structure MipsCoder = Coder(
	structure M=CoderInstr
	      and E=MipsMCode(structure MSpec = MachSpec and E=Endian))
    structure Gen = CPSgen(structure M = MipsCM(structure C=MipsCoder
						and E=Endian
						and MachSpec=MachSpec)
			   structure MachSpec = MachSpec)
    fun collect () = (MipsCoder.finish(); KeepMipsMCode.getCodeString())
)


(*
 * $Log$
 *)
