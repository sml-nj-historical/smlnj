(*
 * Generate the <arch>Instr signature and functor.
 * This structure contains the definition of the instruction set.
 *)

functor MDLGenInstr(Comp : MDL_COMPILE) : MDL_GEN_MODULE =
struct

   structure Ast  = Comp.Ast
   structure Comp = Comp

   open Ast Comp.Util

   fun gen md =
   let (* name of the structure/signature *)
       val strName = Comp.strname md "Instr"  
       val sigName = Comp.signame md "INSTR"

       (* The datatype that defines the instruction set *)
       val instrDatatype =
           DATATYPEdecl
             ([DATATYPE("instruction",[],Comp.instructions md)],[])

       (* Arguments to the instruction functor *)
       val args =
           ["structure LabelExp : LABELEXP",
            "structure Region   : REGION"
           ]

       (* The signature *)
       val sigBody =
          [$ ["structure C : "^Comp.signame md "CELLS",
              "structure Constant: CONSTANT",
              "structure LabelExp: LABELEXP",
              "structure Region : REGION",
              "   sharing Constant = LabelExp.Constant"
              ],
           Comp.typeOf md "Instruction",
           instrDatatype
          ]

       (* The functor *)
       val strBody = 
           [$ ["structure C = "^Comp.strname md "Cells",
               "structure Region = Region",
               "structure LabelExp = LabelExp",
               "structure Constant = LabelExp.Constant"
              ],
            Comp.declOf md "Instruction",
            instrDatatype
           ]

       val _ = Comp.require md "Instruction"
                  {types =["ea","operand", "addressing_mode"],
                   values=[]
                  }

   in  Comp.codegen md "instructions/Instr"
         [Comp.mkSig md "INSTR" (map Comp.Trans.stripMarks sigBody),
          Comp.mkFct md "Instr" args sigName 
                (map Comp.Trans.stripMarks strBody)
         ]
   end
end
