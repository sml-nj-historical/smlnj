(* bug257/a.sml *)

functor classes () =
    struct
        datatype symbol_class = 
            DataClass of data_class
          | SpecialClass of special_class
            
        and data_class = Int | Real | Bool | String
        and special_class = Any | None
    end
