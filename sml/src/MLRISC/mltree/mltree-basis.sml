structure MLTreeBasis : MLTREE_BASIS =
struct
 
  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU 

  datatype fcond = ? | !<=> | == | ?= | !<> | !?>= | < | ?< | !>= | !?> |
                   <= | ?<= | !> | !?<= | > | ?> | !<= | !?< | >= | ?>= |
                   !< | !?= | <> | != | !? | <=> | ?<>

  datatype ext = SIGN_EXTEND | ZERO_EXTEND

  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

  type attribs = word

  type misc_op = {ty:int,name:string,attribs:attribs,hash:word}

  (* Should be datatypes, but FLINT does not optimize them well *)
  type ty = int
  type fty = int

end
