structure MathInlineT = struct
  val sqrt   : real -> real = InLine.f64sqrt 
  val sine   : real -> real = InLine.f64sin
  val cosine : real -> real = InLine.f64cos
  val tangent: real -> real = InLine.f64tan
end