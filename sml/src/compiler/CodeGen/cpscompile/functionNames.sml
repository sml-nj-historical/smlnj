structure FunctionNames = struct
  type name = string
  val default = ""
  fun toString name = name
  fun ==(x:name,y:name) = x=y
end

