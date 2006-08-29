signature FSIG =
sig
  type 'a q
  val put : 'a * 'a q -> 'a q
end

structure F : FSIG =
struct
  type 'a q = unit * 'a list
  fun put (a, (x,y)) = (x, a::y)     (* put: 'a * ('b * 'a list) -> ('b * 'a list) *)
end

structure LrParser =
struct

  fun f (z, q) = F.put(z, q)

end
