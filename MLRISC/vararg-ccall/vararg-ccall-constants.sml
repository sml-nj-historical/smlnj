structure VarargCCallConstants =
  struct

  (* the default byte width for storing vararg information *)
    val defaultWidthB = 8

  (* encodings for the kinds of argument locations *)
    val GPR = 0
    val FPR = 1
    val STK = 2
    val FSTK = 3

  (* offsets into the zipped argument *)
    val argOff = 0
    val kindOff = 1
    val locOff = 2
    val tyOff = 3

  (* number of bytes for a zipped argument *)
    val zippedArgSzB = 4 * 8

  end
