module type STATE = sig 
  type player = {mutable mon_list: steammon list;
                 mutable inventory: inventory; 
                 mutable credits: int} 

  type t = {mutable red: player ; 
            mutable blue: player; 
            mutable first_player : color; 
            mutable mon_table : steammon Table.t; 
            mutable move_list: move_set }

  val create: unit -> t
end