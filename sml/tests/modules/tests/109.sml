signature S4 = sig
    type t
    type u

    sharing type t = u
end;

functor F4 (X:S4) = struct end
