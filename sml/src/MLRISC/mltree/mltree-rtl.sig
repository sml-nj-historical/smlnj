(*
 * How to represent an RTL
 *)
signature MLTREE_RTL =
sig
   structure Basis : MLTREE_BASIS
   structure Region : REGION
   structure RTLExt : sig
       datatype  ('s,'r,'f,'c) sx = 
          ASSIGN of 'r loc * 'r
       |  PAR of 's * 's  

       and ('s,'r,'f,'c) rx = 
          FORALL of 'r
       |  FETCH  of 'r loc
       |  ARG    of string * string
       |  PARAM  of int * int
       |  OP     of Basis.misc_op ref * 'r list
       |  SLICE  of {from:'r, to:'r} list * Basis.ty * 'r 

       and  'r loc  = AGG of Basis.ty * endian * 'r cell

       and  'r cell = CELL of string * Basis.ty * 'r * 'r 

       and   endian = LITTLE_ENDIAN | BIG_ENDIAN

       and  ('s,'r,'f,'c) fx = FX
       and  ('s,'r,'f,'c) ccx = CCX
   end

   structure T : MLTREE
   structure Util : MLTREE_UTILS 
   structure Rewrite : MLTREE_REWRITE
      sharing Util.T = Rewrite.T = T
      sharing RTLExt = T.Extension
      sharing Basis  = T.Basis
      sharing T.Region = Region

   datatype rtlOp     = datatype RTLExt.rx
   datatype rtlAction = datatype RTLExt.sx
   datatype rtlCell   = datatype RTLExt.cell
   datatype rtlLoc    = datatype RTLExt.loc
   datatype rtlEndian = datatype RTLExt.endian

   type action = T.stm
   type rtl    = action
   type exp    = T.rexp
   type cond   = T.ccexp
   type loc    = T.rexp rtlLoc
   type cell   = T.rexp rtlCell
   type ty     = T.ty

   (* Hashing and Equality *) 
   val hashRTL : action -> word
   val eqRTL  : action * action -> bool

   (* Pretty Printing *)
   val showRTL : {def:int->string, use:int->string,
                  regionDef:int->string, regionUse:int->string} -> T.printer
   val rtlToString : action -> string
   val expToString : exp -> string
   val newOp  : {name:string, attribs:T.Basis.attribs} -> T.Basis.misc_op ref
   val new    : action -> rtl 
   val pin    : rtl -> rtl

   val COPY   : rtl
   val JMP    : rtl 

   (* Queries *)
   val can'tMoveUp         : rtl -> bool
   val can'tMoveDown       : rtl -> bool
   val pinned              : rtl -> bool
   val hasSideEffect       : rtl -> bool
   val can'tBeRemoved      : rtl -> bool

   val isConditionalBranch : rtl -> bool
   val isJump              : rtl -> bool
   val isLooker            : rtl -> bool

end 
