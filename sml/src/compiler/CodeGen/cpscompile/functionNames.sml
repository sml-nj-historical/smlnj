structure FunctionNames = struct
  type name = int
  val default = ~1 
  fun toString ~1 = ""
    | toString f = Int.toString f
  fun ==(x:name,y:name) = x=y
end

