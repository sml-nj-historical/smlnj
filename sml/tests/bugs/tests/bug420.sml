(* bug420.sml *)

signature EXCHANGE_STRUCTURE =
sig
   type tree 
   val new_node : tree -> tree
end

structure ex : EXCHANGE_STRUCTURE =
struct
datatype tree = Subwindow of subwindow
	      | Canvas of canvas
	      | Frame of frame
	      | Baseframe of baseframe
	      | NULL
withtype subwindow = {t_node: tree}
and canvas = {subwindow: subwindow}
and frame = {tree_node: tree}
and baseframe = {frame: frame,foog:bool} 

exception Tube_Bug

fun position (Canvas c) = position(Subwindow(#subwindow c))
  | position (Baseframe bf) = position(Frame (#frame bf))
  | position _ = raise Tube_Bug

fun tn_set_position(t,p) = ()
fun set_position (Subwindow sb) = tn_set_position(#t_node sb,0)
  | set_position (Frame f) = tn_set_position(#tree_node f,0)
  | set_position (Canvas c) = set_position(Subwindow(#subwindow c))
  | set_position (Baseframe bf) = set_position(Frame(#frame bf))
  | set_position _ = raise Tube_Bug

fun components(Canvas c) = components(Subwindow (#subwindow c))
  | components(Baseframe bf) = components(Frame (#frame bf))
  | components _ = raise Tube_Bug

fun bounding_box(Canvas c) = bounding_box(Subwindow (#subwindow c))
  | bounding_box(Baseframe bf) = bounding_box(Frame (#frame bf))
  | bounding_box _ = raise Tube_Bug

fun tn_set_bounding_box(t,r) = ()
fun set_bounding_box(Subwindow sb) = tn_set_bounding_box(#t_node sb,0)
  | set_bounding_box(Frame f) = tn_set_bounding_box(#tree_node f,0)
  | set_bounding_box(Canvas c) = set_bounding_box(Subwindow(#subwindow c))
  | set_bounding_box(Baseframe bf) = set_bounding_box(Frame(#frame bf))
  | set_bounding_box _ = raise Tube_Bug

fun new_node tl =
   let
      val pos = position(Frame {tree_node = tl})
   in
      NULL
   end
end
