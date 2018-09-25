(* math-built-in-x86.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Built-in math functions where there is hardware support (i.e., x86).
 *)

structure MathInlineT =
  struct
    val sqrt   : real -> real = InLine.f64sqrt
    val sine   : real -> real = InLine.f64sin
    val cosine : real -> real = InLine.f64cos
    val tangent: real -> real = InLine.f64tan
  end
