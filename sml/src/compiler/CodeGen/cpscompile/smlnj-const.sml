structure SMLNJConstant = struct
  type const = int 
  fun toString(n) = ""
  fun valueOf(n) = MLRiscErrorMsg.impossible ("SMLNJConstant")
  fun hash(n) = 0w0
  fun == (x : const,y : const) = false
end

structure SMLNJLabelExp = LabelExp(SMLNJConstant)
