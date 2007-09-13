structure AMD64InstrExt =
  struct

    datatype fsz = SINGLE | DOUBLE

    datatype ('s, 'r, 'f, 'c) sext 
	= PUSHQ of 'r
 	| POP of 'r 
        | LEAVE
        | RET of 'r
        | LOCK_CMPXCHGL of ('r * 'r)

  end (* AMD64InstrExt *)