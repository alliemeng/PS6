module type STATE = sig 
  type player = {mutable mon_list: steammon list;
                 mutable inventory: inventory; 
                 mutable credits: int} 
  type t = {mutable red: player;
            mutable blue: player;
            mutable mon_table: steammon Table.t}
  val create: unit -> t
end
module State : STATE
type phase

val find_player: color -> player
