open Definitions
open Util
open Constants
open Netgraphics

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

type game = State.t  
let first = ref Red
let move_lst = ref []
let tbl = ref (Table.create 0)

(* let red_draft_count = ref 0
let blue_draft_count = ref 0  *)

let total_draft_count = ref 0
let last_drafted = ref Red

let game_datafication (g:game) : game_status_data =
  let red : team_data = (g.red.mon_list, g.red.inventory, g.red.credits) in
  let blue : team_data = (g.blue.mon_list, g.blue.inventory, g.blue.credits) in 
  (red,blue) 

let game_from_data (game_data: game_status_data) : game = 
    match game_data with
    | (red_mons, red_inv, red_credits), (blue_mons,blue_inv,blue_credits) ->
      let g = State.create () in  
        begin 
        g.red.mon_list <- red_mons;
        g.red.inventory <- red_inv;
        g.red.credits <- red_credits;

        g.blue.mon_list <- blue_mons;
        g.blue.inventory <- red_inv;
        g.blue.credits <- blue_credits;

        g.first_player <- !first;
        g.mon_table <- !tbl;
        g.move_list <- !move_lst
        end; g 


(*sends PickRequest if it is [c]'s turn to pick*)
let pick_request_helper (g:game) (c:color) = 
  let game_data = game_datafication g in 
  if c = Blue then 
  (None, game_data, None, 
    Some (Request(PickRequest(Blue, game_data,g.move_list, 
    hash_to_list g.mon_table))))
  else 
  (None, game_data, Some (Request(
    PickRequest(Red, game_data,g.move_list, 
    hash_to_list g.mon_table))),None) 

(*Drafts [mon] to [c]'s team.
 * Side effects: removes [mon] from steammon_pool, updates GUI, subtracts
 * cost of [mon] from [c]'s credits, adds [mon] to [c]'s team, 
 * increments total_draft_count*)
let draft_mon (g:game) (mon:steammon) (c:color) : unit = 
  if (c = Red) then
    begin
      incr total_draft_count;
      Table.remove g.mon_table mon.species; 
      g.red.mon_list <- mon :: g.red.mon_list;
      g.red.credits <- g.red.credits - mon.cost;
      send_update (UpdateSteammon (mon.species,mon.curr_hp,mon.max_hp,Red))
    end
  else 
    begin
      incr total_draft_count;
      Table.remove g.mon_table mon.species; 
      g.blue.mon_list <- mon :: g.blue.mon_list;
      g.blue.credits <- g.blue.credits - mon.cost;
      send_update (UpdateSteammon (mon.species,mon.curr_hp,mon.max_hp,Blue))
    end

let pick_cheap_mon (g:game) (mon: steammon) : steammon = 
  let (_,cheap_mons) = Table.fold(fun k v (cost,acc) -> 
  if v.cost < cost then (v.cost,v::[]) 
  else (cost,acc)) g.mon_table (mon.cost,[]) in 
  List.hd cheap_mons


let send_request (g:game) = 
  match !last_drafted with
  | Red -> 
    if !total_draft_count mod 2 = 1 then pick_request_helper g Blue
    else pick_request_helper g Red
  | Blue -> 
    if !total_draft_count mod 2 = 1 then pick_request_helper g Red
    else pick_request_helper g Blue

let rec inventory_price (inv: inventory) (acc:int) : int = 
  match inv with 
  | [] -> acc
  | h::t -> 
    if List.length inv = 7 then inventory_price t (acc+cCOST_ETHER)
    else if List.length inv = 6 then inventory_price t (acc+cCOST_MAXPOTION) 
    else if List.length inv = 5 then inventory_price t (acc+cCOST_REVIVE)
    else if List.length inv = 4 then inventory_price t (acc+cCOST_FULLHEAL)
    else if List.length inv = 3 then inventory_price t (acc + cCOST_XATTACK)
    else if List.length inv = 2 then inventory_price t (acc + cCOST_XDEFEND)
    else inventory_price t (acc + cCOST_XSPEED)



 

let handle_step (g:game) (ra:command) (ba:command) : game_output =
  match ra, ba with
    | Action(SendTeamName red_name), Action (SendTeamName blue_name) -> 
      send_update (InitGraphics (red_name,blue_name));
      if g.first_player = Red then
        pick_request_helper g Red
      else 
        pick_request_helper g Blue
         
    | Action(PickSteammon steammon), DoNothing -> 
      if !total_draft_count / 2 = cNUM_PICKS then 
        let game_data = game_datafication g in 
        (None, game_data, Some(Request(PickInventoryRequest(game_data))),
          Some(Request(PickInventoryRequest(game_data))))
      else
        begin
          last_drafted := Red;
          let mon = Table.find g.mon_table steammon in 
          if g.red.credits < mon.cost then 
          let cheap_mon = pick_cheap_mon g mon in 
            begin
              draft_mon g cheap_mon Red; 
              send_request g
            end
          else 
            begin
              draft_mon g mon Red; 
              send_request g 
            end  
        end 
    | DoNothing, Action(PickSteammon steammon) ->
      if !total_draft_count / 2 = cNUM_PICKS then
        let game_data = game_datafication g in  
        (None, game_data, Some(Request(PickInventoryRequest(game_data))),
          Some(Request(PickInventoryRequest(game_data))))
      else
        begin
          last_drafted := Blue;

          let mon = Table.find g.mon_table steammon in 
          if g.blue.credits < mon.cost then 
            let cheap_mon = pick_cheap_mon g mon in
            begin
              draft_mon g cheap_mon Blue; 
              send_request g 
            end 
          else 
            begin
              draft_mon g mon Blue; 
              send_request g 
            end   
        end
       
      | Action(PickInventory red_inv), Action(PickInventory blue_inv) ->
        let default = cNUM_ETHER::cNUM_MAX_POTION::
          cNUM_REVIVE::cNUM_FULL_HEAL::cNUM_XATTACK::
          cNUM_XDEFENSE::cNUM_XSPEED::[] in 
        let r_price = inventory_price red_inv 0 in
        let b_price = inventory_price blue_inv 0 in 
        let r_inv = if r_price > cINITIAL_CASH then default else red_inv in 
        let b_inv = if b_price > cINITIAL_CASH then default else blue_inv in
        g.red.inventory <- r_inv;
        g.blue.inventory <- b_inv;
        let game_data = game_datafication g in 
        (None, game_data, Some(Request(StarterRequest(game_data))),
          Some(Request(StarterRequest(game_data))))
      | Action(SelectStarter red_starter), Action(SelectStarter blue_starter) ->
        failwith "implement battle phase"   
      | _, _ -> failwith "Not here yet"



let init_game () : game * request * request * move list * steammon list =
  let s = State.create () in 

    Initialization.init_pool "moves.csv" "steammon.csv";
(*     move_table := Initialization.move_table;
    mon_table := Initialization.mon_table; *)

    let move_list = hash_to_list (Initialization.move_table) in 
    let mon_list = hash_to_list (Initialization.mon_table) in 


    let c = Random.int 2 in 
    if c = 0 then 
      begin 
      s.first_player <- Red;
      first:= Red
      end
    else 
      begin
        s.first_player <- Blue; 
        first := Blue 

      end;


    s.mon_table <- Initialization.mon_table;
    s.move_list <- move_list;  

    move_lst := move_list;
    tbl := Initialization.mon_table;

    (s, TeamNameRequest,TeamNameRequest,move_list, mon_list)
