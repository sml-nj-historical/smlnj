structure SMLNJConstant = struct
  type const = unit
  fun toString () = ""
  fun valueOf () = 0
  fun hash _ = 0w0
  fun == (x,y) = true
end

structure SMLNJLabelExp = LabelExp(SMLNJConstant)
