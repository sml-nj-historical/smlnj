(* bug257/b.sml *)

signature classes =
    sig
        datatype symbol_class = 
            DataClass of data_class
          | SpecialClass of special_class
            
        and data_class = Int | Real | Bool | String
        and special_class = Any | None
    end

functor nodes ( structure Classes : classes ) =
    struct
        structure Classes = Classes
        local
            open Classes
        in
            datatype node =
              ClassNode of symbol_class
            | ValueNode of data_class
        end
    end
