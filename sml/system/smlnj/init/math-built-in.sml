(* math-built-in.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Built-in math functions where there is hardware support for sqrt.
 *)

structure MathInlineT =
  struct

    val sqrt : real -> real = InLine.f64sqrt

  end
