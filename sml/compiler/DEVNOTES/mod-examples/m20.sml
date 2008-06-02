
functor F() () () =
struct
  type s = int
end

structure A = F() () ()

