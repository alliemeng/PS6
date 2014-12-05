open Definitions
open Util

module type STATE = sig 
  type player = {mutable mon_list: steammon list;
                 mutable inventory: inventory; 
                 mutable credits: int} 
  type t = {mutable red: player; 
            mutable blue: player}
  val create: unit -> t
end
module State : STATE

type phase
type game

val first: color ref
val move_table: hashtable ref
val mon_table: hashtable ref
val total_draft_count: int ref
val last_drafted: color ref
val last_request_sent: phase ref

val game_datafication: game -> game_status_data
val find_player: color -> game -> State.player
val pick_request_helper: game -> color -> game_output
val handle_SendTeamName: game -> string -> string -> game_output
val draft_mon_helper: game -> steammon -> color -> unit
val pick_cheap_mon: game -> int -> steammon
val handle_PickSteammon: game -> color -> string
val inventory_price: inventory -> int
val set_inventory: game -> color -> inventory -> unit
val handle_PickInventory: game -> inventory -> inventory -> game_output
val switch_steammon: game -> color -> string -> unit
val handle_SelectStarter: game -> string -> string -> game_output
val use_item: game -> color -> item -> string -> 
val handle_ActionRequest: game -> color -> string -> unit