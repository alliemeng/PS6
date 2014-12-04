module State : STATE  = struct
  (*type player represents each team's data*)
  type player = {mutable mon_list: steammon list ; 
                 mutable inventory: inventory ; 
                 mutable credits:  int }  

  (*type t is a record ref that contains both players' data, a list of all remaining
   * steammon, and a list of all remaining moves*)
  type t = {mutable red: player ; 
            mutable blue: player; 
            mutable first_player : color; 
            mutable mon_table : steammon Table.t; 
            mutable move_list: move_set }
  
  (*Initializes the state *)
  let create () : t = 
    let player : player = 
      {mon_list = []; inventory = []; credits = cSTEAMMON_CREDITS} in
    {red = player; blue = player; first_player = Red; mon_table = Table.create 0; 
    move_list = []}



(*   let pick_mons (game:t) (c:color) = 
 *)
end