(* bug1570.1.sml *)

structure X =
struct
  fun main n = let
      fun a (x, 0) = d x
	| a (x, n) = b (x, n - 1)
      and b (x, n) = c (x, n)
      and c (x, n) = a (x, n)
      and d x = e (x, 3)
      and e (x, 0) = f x
	| e (x, n) = e (x, n - 1)
      and f 0 = 1
	| f n = n * g (n - 1)
      and g n = a (n, 3)
  in
      f n
  end
end
