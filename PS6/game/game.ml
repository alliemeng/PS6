open Definitions
open Util
open Constants
open Netgraphics

module State : STATE = struct
  (*type player represents each team's data*)
  type player = {mons_list: steammon list; inventory: inventory; credits: int} ref 

  (*type t is a record ref that contains both players' data*)
  type t = {red:player;blue:player}
  
  (*Initializes the state *)
  let create () : t = 
    let player : player = 
      ref {mons_list = [];inventory = [];credits = cSTEAMMON_CREDITS} in
    {red = player; blue = player}
end

(* You have to implement this. Change it from int to yout own state type*)
type game = State.t  

let game_datafication g =
	failwith 
		"This is my grandson. He’s been your rival since you were a baby. 
		…Erm, what is his name again?"

                        (* Professor Oak, Steammon researcher extraordinaire *)
	
let game_from_data game_data = 
	failwith 
    "I like shorts! They're comfy and easy to wear!"

                        (* Youngster, upon challenging a stranger to battle *)

let handle_step g ra ba =
	failwith 
    "Remember my super cool Rattata? My Rattata is different from regular 
    Rattata. It’s like my Rattata is in the top percentage of all Rattata."

                        (* Youngster Joey, about his Raticate *)

let init_game () =
	Initialization.init_pool "moves.csv" "steammon.csv";
    let moves = Initialization.move_table in
    let mons = Initialization.mon_table in 
    
    (State.create (), TeamNameRequest,TeamNameRequest,hash_to_list moves, hash_to_list mons)