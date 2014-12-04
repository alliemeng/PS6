open Definitions
open Util
open Constants
open Netgraphics

module type STATE = sig 
  type player = {mutable mon_list: steammon list;
                 mutable inventory: inventory; 
                 mutable credits: int} 

  type t = {mutable red: player ; 
            mutable blue: player}

  val create: unit -> t
end

module State : STATE  = struct
  (*type player represents each team's data*)
  type player = {mutable mon_list: steammon list ; 
                 mutable inventory: inventory ; 
                 mutable credits:  int}  

  (*type t is a record ref that contains both players' data, a list of all remaining
   * steammon*)
  type t = {mutable red: player ; 
            mutable blue: player} 
  
  (*Initializes the state *)
  let create () : t = 
    let player1 : player = 
      {mon_list = []; inventory = []; credits = cSTEAMMON_CREDITS} in
    let player2 : player = 
      {mon_list = []; inventory = []; credits = cSTEAMMON_CREDITS} in
    {red = player1; blue = player2}

end



type game = State.t  
let first = ref Red
let move_table = ref (Table.create 0)
let mon_table = ref (Table.create 0)


let total_draft_count = ref 0
let last_drafted = ref Red

(* Type phase enumerates the types of requests that can be sent out to players *)
type phase = TeamNameRequest | PickRequest of color | PickInventoryRequest |
                StarterRequest | ActionRequest 

(* Records the last type of request sent out to each player*)
let last_request_sent : phase ref = ref TeamNameRequest

(*Converts [g] to game_status_data*)
let game_datafication (g:game) : game_status_data =
  let red : team_data = (g.red.mon_list, g.red.inventory, g.red.credits) in
  let blue : team_data = (g.blue.mon_list, g.blue.inventory, g.blue.credits) in 
  (red,blue) 

(*Converts [game_data] to a game*)
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

        end; g 





(* Returns a PickRequest output for the [c] player. i.e: If it is 
 * the blue player's turn to pick, then pick_request_helper sends 
 * out a PickRequest to the blue team. 
 * Side Effects: Sets last_requested to PickRequest [c]*)
let pick_request_helper (g:game) (c:color) : game_output = 
  let game_data = game_datafication g in 
  match c with 
  | Blue -> 
      last_request_sent:= PickRequest Blue;
      (None, game_data, None, 
        Some (Request(PickRequest(Blue, game_data, hash_to_list !move_table(* g.move_list *), 
        hash_to_list !mon_table))))
  | Red -> 
      last_request_sent := PickRequest Red;
      (None, game_data, Some (Request(
        PickRequest(Red, game_data,hash_to_list !move_table(* g.move_list *), 
        hash_to_list !mon_table))),None) 

(* Updates the GUI with the red player's name [r_name] and 
 * the blue player's name [b_name] and returns a PickRequest output
 * Invariant: handle_SendTeamName is called only when last_requested
 *            is set to TeamNameRequest
 * Side Effects: Updates team names on GUI, sets last_requested to 
 *               some PickRequest*)
let handle_SendTeamName (g: game) (r_name: string) 
  (b_name: string) : game_output = 
  send_update (InitGraphics (r_name,b_name)); 
  if !first = Red then pick_request_helper g Red
  else pick_request_helper g Blue

(* Drafts [mon] to the [c] player's team and returns a unit.
 * Side effects: removes [mon] from mon_table, updates the
 *               player's team on the GUI, subtracts the cost of [mon]
 *               from [c]'s credits, adds [mon] to the [c] player's team, 
 *               increments total_draft_count*)
let draft_mon_helper (g:game) (mon:steammon) (c:color) : unit = 
  let player = match c with
  | Red -> g.red 
  | Blue -> g.blue in 
      
    begin
      incr total_draft_count;
      Table.remove !mon_table mon.species; 
      player.mon_list <- mon :: player.mon_list;
      player.credits <- player.credits - mon.cost;

      send_update (UpdateSteammon (mon.species,mon.curr_hp,mon.max_hp,c))

    end
  
(* Returns the cheapest steammon available. 
 * Side Effects: removes steammon from mon_table *)
let pick_cheap_mon (g:game) (mon_cost: int) : steammon = 
  let (_,cheap_mons) = Table.fold(fun k v (cost,acc) -> 
  if v.cost < cost then (v.cost,v::[]) 
  else (cost,acc)) !mon_table (mon_cost,[]) in 
  List.hd cheap_mons

(* Returns the price of the items in [inventory]. *)
let inventory_price (inventory: inventory) : int = 
  let rec inv_price_helper (inv: inventory) (acc: int) = 
    match inv with 
    | [] -> acc
    | h::t -> 
      if List.length inv = 7 then inv_price_helper t (acc + h * cCOST_ETHER)
      else if List.length inv = 6 then inv_price_helper t (acc+h*cCOST_MAXPOTION) 
      else if List.length inv = 5 then inv_price_helper t (acc+h*cCOST_REVIVE)
      else if List.length inv = 4 then inv_price_helper t (acc+h*cCOST_FULLHEAL)
      else if List.length inv = 3 then inv_price_helper t (acc + h*cCOST_XATTACK)
      else if List.length inv = 2 then inv_price_helper t (acc + h*cCOST_XDEFEND)
      else inv_price_helper t (acc + h*cCOST_XSPEED) in 
    inv_price_helper inventory 0

(* Returns [lst] with the active [mon_name] at the head of the list. 
 * If the [mon_name] isn't found in [lst], it raises a Failure exception. *)
let rec set_active_steammon (lst: steammon list) 
  (mon_name: string) : steammon list = 
  let (result,new_lst) = List.fold_right (fun elem (mon,mon_list) -> 
    if elem.species = mon_name 
      then Some elem, mon_list 
    else 
      None,elem::mon_list) lst (None, []) in 
  match result with
  | None -> failwith "Error: steammon not found"
  | Some x -> x::new_lst

(* Switches out the currently active steammon in the [c] player's team
 * with the [mon_name] and returns a unit. If [mon_name] isn't 
 * in [c] player's team, the function raises a Failure exception.
 * Side effects: Puts [mon_name] at the head of [c] player's team list,
 *               resets the switched-out steammon's mods to zero,
 *               updates the active steammon on the GUI with [mon_name] *)
let switch_steammon (g:game) (c:color) (mon_name:string) : unit = 
  let default_modifier = {
    attack_mod = 0;
    defense_mod = 0;
    spl_attack_mod = 0;
    spl_defense_mod = 0;
    speed_mod = 0} in

  let reset_mods (lst: steammon list) : steammon list = 
    match lst with
    | [] -> failwith "No steammon drafted!"
    | h::t -> 
      {h with mods = default_modifier} :: t in 

  let player = match c with
  | Red -> g.red
  | Blue -> g.blue in 
    begin
      player.mon_list <- reset_mods player.mon_list;
      player.mon_list <- set_active_steammon player.mon_list mon_name;
      send_update (SetChosenSteammon mon_name)
    end



(* Adds [mon_name] to the [c] player's team. If [mon_name] costs more
 * than the player's number of credits, or if no steammon is given 
 * (i.e. [mon_name] = ""), then a random, cheap steammon
 * is added to the team instead. Returns a PickRequest output if each player
 * doesn't have a full team yet, otherwise returns a PickInventory output.
 * Invariant: handle_PickSteammon is called only when last_request_sent 
 *            is set to PickRequest
 * Side Effects: adds a steammon to [c] player's team, subtracts the cost
 *               of the steammon from the player's credits, updates the GUI
 *               with the updated team, increments the total_draft_count and 
 *               last_drafted, updates last_request_sent to PickRequest [c] *)  
let handle_PickSteammon (g:game) (c: color) (mon_name:string) = 
  let player = match c with 
  | Red -> g.red 
  | Blue -> g.blue in 

  (*Helper function: sends a PickRequest to the appropriate team*)
  let send_request (g:game) : game_output = 
    match !last_drafted with
    | Red -> 
      if !total_draft_count mod 2 = 1 then pick_request_helper g Blue
      else pick_request_helper g Red
    | Blue -> 
      if !total_draft_count mod 2 = 1 then pick_request_helper g Red
      else pick_request_helper g Blue in 
  
  if !total_draft_count / 2 = cNUM_PICKS then 
    begin 
    last_request_sent := PickInventoryRequest; 
    let game_data = game_datafication g in 
        (None, game_data, Some(Request(PickInventoryRequest(game_data))),
          Some(Request(PickInventoryRequest(game_data))))
    end 
      else
        begin
          last_drafted := c;
          if mon_name = "" then 
            let cheap_mon = pick_cheap_mon g 500 in 
                begin
                  draft_mon_helper g cheap_mon c; 
                  send_request g
                end
          else
            begin 
              let mon = Table.find !mon_table mon_name in 
              if player.credits < mon.cost then 
                  let cheap_mon = pick_cheap_mon g mon.cost in 
                    begin
                      draft_mon_helper g cheap_mon c; 
                      send_request g
                    end
              else 
                begin
                  draft_mon_helper g mon c; 
                  send_request g 
                end  
            end
        end 


(* Sets the [c] player's inventory to [inv] if the player has 
 * enough cash to buy the items in [inv]. Otherwise, set_inventory
 * gives the player a default inventory 
 * Side Effects: initilializes [c] player's inventory*) 
let set_inventory (g:game) (c:color) (inv:inventory) : unit = 
  let default = cNUM_ETHER::cNUM_MAX_POTION::
          cNUM_REVIVE::cNUM_FULL_HEAL::cNUM_XATTACK::
          cNUM_XDEFENSE::cNUM_XSPEED::[] in 

  let player = match c with 
  | Red -> g.red
  | Blue -> g.blue in 

  match inv with
  | [] -> player.inventory <- default
  | h::t -> 
    let price = inventory_price inv  in 
    print_endline("inventory price: " ^ string_of_int price);
    let inv' = if price > cINITIAL_CASH then default else inv in
    player.inventory <- inv' 

(* Initializes the red player's inventory with [r_inv] and the 
 * blue player's inventory with [b_inv]. If a player does not have 
 * enough cash to purchase the inventory, they get a default inventory
 * (see constants.ml for details). Returns a StarterRequest output.
 * Invariant: handle_PickInventory is called only when last_request_sent 
 *            is set to PickInventory
 * Side Effects: sets each player's inventory, updates last_request_sent 
                 to StarterRequest *)
let handle_PickInventory (g: game) (r_inv:inventory) 
  (b_inv:inventory) : game_output = 
  set_inventory g Red r_inv;
  set_inventory g Blue b_inv;
  
  last_request_sent := StarterRequest;  
  let game_data = game_datafication g in 
  (None, game_data, Some(Request(StarterRequest(game_data))),
    Some(Request(StarterRequest(game_data))))

(* Sets [mon_name] as the active, starter steammon in [c] player's team.
 * If no steammon is given ([mon_name] = ""), then the steammon at the 
 * head of the player's team list is selected instead. 
 * Side Effects: sets [mon_name] at the head of the player's team list,
 *               updates the GUI with the active steammon *)
let set_starter (g:game) (c:color) (mon_name:string) : unit = 
  let player = match c with 
  | Red -> g.red
  | Blue -> g.blue in

  if mon_name = "" then ()
  else  
    player.mon_list <- set_active_steammon player.mon_list mon_name;
    send_update (SetChosenSteammon mon_name)  

(* Sets the red player's starter with [r_starter] and the blue player's 
 * starter with [b_starter]. Returns an ActionRequest output
 * Invariants: handle_SelectStarter is called only when last_request_sent
 *             is set to StarterRequest
 * Side Effects: sets each player's starter at the head of their team list,
 *               updates the GUI with each player's starter, updates 
 *               last_request_sent to ActionRequest *)
let handle_SelectStarter (g:game) (r_starter: string) 
  (b_starter: string) : game_output = 
  set_starter g Red r_starter;
  set_starter g Blue b_starter;

  last_request_sent := ActionRequest;  
  let game_data = game_datafication g in 
  (None, game_data, Some(Request(ActionRequest game_data)),
    Some(Request(ActionRequest game_data)))


let handle_step (g:game) (ra:command) (ba:command) : game_output =
  match !last_request_sent with 
    | TeamNameRequest -> (match ra,ba with
      | Action(SendTeamName red_name), Action (SendTeamName blue_name) -> 
        handle_SendTeamName g red_name blue_name 
      | DoNothing, Action (SendTeamName blue_name) -> 
        handle_SendTeamName g "Red" blue_name 
      | Action(SendTeamName red_name), DoNothing -> 
        handle_SendTeamName g red_name "Blue" 
      | DoNothing,DoNothing -> handle_SendTeamName g "Red" "Blue"
      | _,_ -> failwith "Doesn't happen")
    | PickRequest c -> (match ra,ba with
      | Action(PickSteammon steammon), DoNothing -> 
        handle_PickSteammon g Red steammon
      | DoNothing, Action(PickSteammon steammon) ->
        handle_PickSteammon g Blue steammon 
      | DoNothing, DoNothing -> handle_PickSteammon g c ""
      | _,_ -> failwith "Doesn't happen")
    | PickInventoryRequest -> (match ra,ba with
      | Action(PickInventory red_inv), Action(PickInventory blue_inv) ->
        handle_PickInventory g red_inv blue_inv
      | Action(PickInventory red_inv), DoNothing ->
        handle_PickInventory g red_inv []
      | DoNothing, Action(PickInventory blue_inv) ->
        handle_PickInventory g [] blue_inv
      | DoNothing, DoNothing -> handle_PickInventory g [] []
      | _,_ -> failwith "Doesn't happen")
    | StarterRequest -> (match ra,ba with
      | Action(SelectStarter red_starter), Action(SelectStarter blue_starter)->
        handle_SelectStarter g red_starter blue_starter
      | Action(SelectStarter red_starter), DoNothing ->
        handle_SelectStarter g red_starter ""
      | DoNothing, Action(SelectStarter blue_starter)->
        handle_SelectStarter g "" blue_starter
      | DoNothing, DoNothing -> handle_SelectStarter g "" ""
      | _,_ -> failwith "Doesn't happen")
    | ActionRequest -> (match ra, ba with
      | DoNothing, DoNothing -> 
        let game_data = game_datafication g in 
          (None, game_data, Some(Request(ActionRequest game_data)),
            Some(Request(ActionRequest game_data)))
      | _,_ -> failwith "Doesn't happen")


let init_game () : game * request * request * move list * steammon list =
  let s = State.create () in 

    Initialization.init_pool "moves.csv" "steammon.csv";
    
    move_table := Initialization.move_table;
    mon_table := Initialization.mon_table;

    let c = Random.int 2 in 
    if c = 1 then  
      first:= Red
    else 
        first := Blue;

    last_request_sent := TeamNameRequest;
    (s, TeamNameRequest,TeamNameRequest,hash_to_list (!move_table), hash_to_list (!mon_table))
