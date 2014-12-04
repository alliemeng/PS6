open Definitions
open Util
open Constants
open Netgraphics

module type STATE = sig 
  type player = {mutable mon_list: steammon list;
                 mutable inventory: inventory; 
                 mutable credits: int} 
  type t = {mutable red: player; 
            mutable blue: player}

  val create: unit -> t
end

module State : STATE  = struct
  (*type player represents each team's data*)
  type player = {mutable mon_list: steammon list; 
                 mutable inventory: inventory; 
                 mutable credits:  int}  
  (*type t is a record ref that contains both players' data, a list of all remaining
   * steammon*)
  type t = {mutable red: player; 
            mutable blue: player} 
  
  (*Initializes the state *)
  let create () : t = 
    let player1 : player = 
      {mon_list = []; inventory = []; credits = cSTEAMMON_CREDITS} in
    let player2 : player = 
      {mon_list = []; inventory = []; credits = cSTEAMMON_CREDITS} in
    {red = player1; blue = player2}
end

(* Type phase enumerates the types of requests that can be sent out to players *)
type phase = TeamNameRequest | PickRequest of color | PickInventoryRequest |
              StarterRequest | ActionRequest 

type game = State.t  
let first = ref Red
let move_table = ref (Table.create 0)
let mon_table = ref (Table.create 0)
let total_draft_count = ref 0
let last_drafted = ref Red
let last_request_sent = ref TeamNameRequest

let game_datafication (g:game) : game_status_data =
  let red : team_data = (g.red.mon_list, g.red.inventory, g.red.credits) in
  let blue : team_data = (g.blue.mon_list, g.blue.inventory, g.blue.credits) in 
  (red,blue)

(* finds player corresponding to color type *)
(* requires: type color either red or blue *)
(* returns: its corresponding player *)
let find_player (c:color) (g:game) : State.player =
  match c with
  | Red -> g.red
  | Blue -> g.blue


(*                                       *)
(*    INITIALIZATION & DRAFTING PHASE    *)
(*                                       *)

(* Returns a PickRequest output for the [c] player. i.e: If it is 
 * the blue player's turn to pick, then pick_request_helper sends 
 * out a PickRequest to the blue team. 
 * Side Effects: Sets last_requested to PickRequest [c]*)
let pick_request_helper (g:game) (c:color) : game_output = 
  let game_data = game_datafication g in 
  match c with 
  | Blue -> 
      last_request_sent := PickRequest Blue;
      (None, game_data, None, 
        Some (Request(PickRequest(Blue, game_data, hash_to_list !move_table, 
        hash_to_list !mon_table))))
  | Red -> 
      last_request_sent := PickRequest Red;
      (None, game_data, Some (Request(
        PickRequest(Red, game_data,hash_to_list !move_table, 
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
  let player = find_player c g in 
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
  let (_,cheap_mons) = Table.fold (fun k v (cost,acc) -> 
    if v.cost < cost then (v.cost,v::[]) 
    else (cost,acc)) !mon_table (mon_cost,[]) in
  List.hd cheap_mons

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
  let player = find_player c g in

  (* Helper function: sends a PickRequest to the appropriate team *)
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


(*                                       *)
(*            INVENTORY PHASE            *)
(*                                       *)

(* Returns the price of the items in [inventory]. *)
let inventory_price (inventory: inventory) : int = 
  let rec inv_price_helper (inv: inventory) (acc: int) = 
    match inv with 
    | [] -> acc
    | h::t -> 
      if List.length inv = 7 then inv_price_helper t (acc+h*cCOST_ETHER)
      else if List.length inv = 6 then inv_price_helper t (acc+h*cCOST_MAXPOTION) 
      else if List.length inv = 5 then inv_price_helper t (acc+h*cCOST_REVIVE)
      else if List.length inv = 4 then inv_price_helper t (acc+h*cCOST_FULLHEAL)
      else if List.length inv = 3 then inv_price_helper t (acc+h*cCOST_XATTACK)
      else if List.length inv = 2 then inv_price_helper t (acc+h*cCOST_XDEFEND)
      else inv_price_helper t (acc+h*cCOST_XSPEED) in 
    inv_price_helper inventory 0

(* Sets the [c] player's inventory to [inv] if the player has 
 * enough cash to buy the items in [inv]. Otherwise, set_inventory
 * gives the player a default inventory 
 * Side Effects: initilializes [c] player's inventory*) 
let set_inventory (g:game) (c:color) (inv:inventory) : unit = 
  let default = cNUM_ETHER::cNUM_MAX_POTION::
          cNUM_REVIVE::cNUM_FULL_HEAL::cNUM_XATTACK::
          cNUM_XDEFENSE::cNUM_XSPEED::[] in
  let player = find_player c g in 
  match inv with
  | [] -> player.inventory <- default
  | h::t ->
      let price = inventory_price inv in 
      (* print_endline("inventory price: " ^ string_of_int price); *)
      let inv' = if price > cINITIAL_CASH then default else inv in
      let string_inv = List.fold_right (fun x acc ->
        string_of_int x ^ " " ^ acc
      ) inv' "" in
      Netgraphics.add_update (Message(
        (string_of_color c) ^ " team inventory:" ^ string_inv));
      player.inventory <- inv'

(* Initializes the red player's inventory with [r_inv] and the 
 * blue player's inventory with [b_inv]. If a player does not have 
 * enough cash to purchase the inventory, they get a default inventory
 * (see constants.ml for details). Returns a StarterRequest output.
 * Invariant: handle_PickInventory is called only when last_request_sent 
 *            is set to PickInventory
 * Side Effects: sets each player's inventory, updates last_request_sent 
                 to StarterRequest *)
let handle_PickInventory (g:game) (r_inv:inventory) 
  (b_inv:inventory) : game_output = 
  set_inventory g Red r_inv;
  set_inventory g Blue b_inv;
  
  last_request_sent := StarterRequest;  
  let game_data = game_datafication g in 
  (None, game_data, Some(Request(StarterRequest(game_data))),
    Some(Request(StarterRequest(game_data))))


(*                                       *)
(*             BATTLE PHASE              *)
(*                                       *)

(* Switches a non-fainted steammon to be active and returns a unit.
 * If [mon_name] isn't in [c] player's team, this move is ignored.
 * Side effects: Puts [mon_name] at the head of [c] player's team list,
 *               resets the switched-out steammon's mods to zero,
 *               updates the active steammon on the GUI with [mon_name] *)
let switch_steammon (g:game) (c:color) (mon_name:string) : unit = 
  let player = find_player c g in
  if mon_name = "" then ()
  else
    begin 
      (*find steammon in team*)
      let (found,lst) = List.fold_right (fun elem (mon,acc) -> 
        if (elem.species = mon_name && elem.curr_hp > 0)
        then Some elem,acc 
        else None,elem::acc) 
        player.mon_list (None, []) in
      
      match found with
      | None -> (Netgraphics.add_update (Message("You don't own that steammon!")));
      | Some new_guy ->
        begin
          (*default mods*)
          let default_modifier = {
          attack_mod = 0;
          defense_mod = 0;
          spl_attack_mod = 0;
          spl_defense_mod = 0;
          speed_mod = 0} in

          (*resets mods of active steammon*)
          let reset_mods (lst: steammon list) : steammon list = 
          match lst with
          | [] -> failwith "No steammon drafted!"
          | h::t -> 
            {h with mods = default_modifier} :: t in 

          Netgraphics.add_update (SetChosenSteammon(new_guy.species));
          Netgraphics.add_update (Message((string_of_color c) ^
            " team brought out " ^ new_guy.species ^ "!"));
          player.mon_list <- new_guy::lst
          
        end
    end

    

(* Sets the red player's starter with [r_starter] and the blue player's 
 * starter with [b_starter]. Returns an ActionRequest output
 * Invariants: handle_SelectStarter is called only when last_request_sent
 *             is set to StarterRequest
 * Side Effects: sets each player's starter at the head of their team list,
 *               updates the GUI with each player's starter, updates 
 *               last_request_sent to ActionRequest *)
let handle_SelectStarter (g:game) (r_starter:string) 
  (b_starter:string) : game_output = 
  switch_steammon g Red r_starter;
  switch_steammon g Blue b_starter;

  last_request_sent := ActionRequest;
  let game_data = game_datafication g in 
  (None, game_data, Some(Request(ActionRequest game_data)),
    Some(Request(ActionRequest game_data)))

let use_item (g:game) (c:color) (item:item) (s:string) : unit =
  Netgraphics.add_update (Message((string_of_color c) ^ " team used " ^
    string_of_item item ^ "on " ^ s ^ "!"));
  let player = find_player c g in
  let target = List.find player.mon_list in
  let (own_item, new_inventory) = match player.inventory with
    | [ether;max;revive;heal;attack;defense;speed] ->
        begin
          match item with
            | Ether -> if ether > 0 then
                (true,[ether-1;max;revive;heal;attack;defense;speed])
                else (false, [])
            | MaxPotion ->if max > 0 then
                (true,[ether;max-1;revive;heal;attack;defense;speed])
                else (false, [])
            | Revive -> if revive > 0 then
                (true,[ether;max;revive-1;heal;attack;defense;speed])
                else (false, [])
            | FullHeal -> if heal > 0 then
                (true,[ether;max;revive;heal-1;attack;defense;speed])
                else (false, [])
            | XAttack -> if attack > 0 then
                (true,[ether;max;revive;heal;attack-1;defense;speed])
                else (false, [])
            | XDefense -> if defense > 0 then
                (true,[ether;max;revive;heal;attack;defense-1;speed])
                else (false, [])
            | XSpeed ->if speed > 0 then
                (true,[ether;max;revive;heal;attack;defense;speed-1])
                else (false, [])
        end
    | _ -> failwith "Invalid inventory" in
  let use_on (target:steammon) : unit =
    List.fold_left (fun acc x ->
      if x.species = target.species then
        match item with
        | Ether ->
          begin
            let inc_pp (m:move) = {
              name = m.name;
              element = m.element;
              target = m.target
              max_pp = m.max_pp;
              pp_remaining =
                if m.pp_remaining + 5 >= m.max_pp then m.max_pp
                else m.pp_remaining + 5;
              power = m.power;
              accuracy = m.accuracy;
              effects = m.effects
            } in
            let new_guy = {
              species = target.species;
              curr_hp = target.curr_hp;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = inc_pp target.first_move;
              second_move = inc_pp target.second_move;
              third_move = inc_pp target.third_move;
              fourth_move = inc_pp target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = target.status;
              mods = target.mods;
              cost = target.cost
            } in
            if target.curr_hp > 0 then
              Netgraphics.add_update (Item(string_of_item item,RestoredPP 5,team,
                target.species));
              Netgraphics.add_update (
                UpdateSteammon(target.species,target.curr_hp,target.max_hp,team));
              new_guy::acc
            else
              Netgraphics.add_update (Message (target.species ^ " is fainted and can't use ether!"));
              x::acc
          end
        | MaxPotion ->
          begin
            let new_guy = {
              species = target.species;
              curr_hp = target.max_hp;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = target.first_move;
              second_move = target.second_move;
              third_move = target.third_move;
              fourth_move = target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = target.status;
              mods = target.mods;
              cost = target.cost
            } in
            if target.curr_hp > 0 then
              Netgraphics.add_update (Item(string_of_item item,Recovered 100,
                team,target.species));
              Netgraphics.add_update (
                UpdateSteammon(target.species,target.max_hp,target.max_hp,team));
              new_guy::acc
            else
              Netgraphics.add_update (Message
                (target.species ^ " is fainted and can't be healed!"));
              x::acc
          end
        | Revive ->
          begin
            let new_guy = {
              species = target.species;
              curr_hp = target.max_hp/2;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = target.first_move;
              second_move = target.second_move;
              third_move = target.third_move;
              fourth_move = target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = target.status;
              mods = target.mods;
              cost = target.cost
            } in
            if target.curr_hp <= 0 then
              Netgraphics.add_update (Item(string_of_item item,
                Recovered 50,team,target.species));
              Netgraphics.add_update (
                UpdateSteammon(target.species,target.curr_hp,target.max_hp,team));
              new_guy::acc
            else
              Netgraphics.add_update (Message
                (target.species ^ " is not fainted and can't be revived!"));
              x::acc
          end
        | FullHeal ->
          begin
            let new_guy = {
              species = target.species;
              curr_hp = target.curr_hp;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = target.first_move;
              second_move = target.second_move;
              third_move = target.third_move;
              fourth_move = target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = None;
              mods = target.mods;
              cost = target.cost
            } in
            Netgraphics.add_update (Item(string_of_item item,HealedStatus 
              target.status,team,target.species));
            Netgraphics.add_update (
              UpdateSteammon(target.species,target.curr_hp,target.max_hp,team))
            if target.status = None then
              Netgraphics.add_update (Message
                (target.species ^ " is not fainted and can't be revived!"));
              x::acc
            else
              Netgraphics.add_update (Item(string_of_item item,HealedStatus 
                status,team,target.species));
              Netgraphics.add_update (
                UpdateSteammon(target.species,target.curr_hp,target.max_hp,team));
              new_guy::acc
          end
        | XAttack ->
          begin
            let new_mods = {
              attack_mod =
                if target.mods.attack_mod + 1 >= 6 then 6
                else target.mods.attack_mod + 1;
              defense_mod = target.mods.defense_mod;
              spl_attack_mod = target.mods.spl_attack_mod;
              spl_defense_mod = target.mods.spl_defense_mod;
              speed_mod = target.mods.speed_mod;
            } in
            let new_guy = {
              species = target.species;
              curr_hp = target.curr_hp;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = target.first_move;
              second_move = target.second_move;
              third_move = target.third_move;
              fourth_move = target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = target.status;
              mods = new_mods;
              cost = target.cost
            } in
            Netgraphics.add_update (Item(string_of_item item,StatModified 
              (Atk,1),team,target.species));
            Netgraphics.add_update (
              UpdateSteammon(target.species,target.curr_hp,target.max_hp,team))
            if target.curr_hp > 0 then new_guy::acc
            else
              Netgraphics.add_update (Message
                (target.species ^ " is fainted and can't use XAttack!"));
              x::acc
          end
        | XDefense ->
          begin
            let new_mods = {
              attack_mod = target.mods.attack_mod;
              defense_mod =
                if target.mods.defense_mod + 1 >= 6 then 6
                else target.mods.defense_mod + 1;
              spl_attack_mod = target.mods.spl_attack_mod;
              spl_defense_mod = target.mods.spl_defense_mod;
              speed_mod = target.mods.speed_mod;
            } in
            let new_guy = {
              species = target.species;
              curr_hp = target.curr_hp;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = target.first_move;
              second_move = target.second_move;
              third_move = target.third_move;
              fourth_move = target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = target.status;
              mods = target.mods;
              cost = target.cost
            } in
            Netgraphics.add_update (Item(string_of_item item,StatModified 
              (Def,1),team,target.species));
            Netgraphics.add_update (
              UpdateSteammon(target.species,target.curr_hp,target.max_hp,team))
            if target.curr_hp > 0 then new_guy::acc
            else
              Netgraphics.add_update (Message
                (target.species ^ " is fainted and can't use XDefense!"));
              x::acc
          end
        | XSpeed ->
          begin
            let new_mods = {
              attack_mod = target.mods.attack_mod;
              defense_mod = target.mods.defense_mod;
              spl_attack_mod = target.mods.spl_attack_mod;
              spl_defense_mod = target.mods.spl_defense_mod;
              speed_mod =
                if target.mods.speed_mod + 1 >= 6 then 6
                else target.mods.speed_mod + 1;
            } in
            let new_guy = {
              species = target.species;
              curr_hp = target.curr_hp;
              max_hp = target.max_hp;
              first_type = target.first_type;
              second_type = target.second_type;
              first_move = target.first_move;
              second_move = target.second_move;
              third_move = target.third_move;
              fourth_move = target.fourth_move;
              attack = target.attack;
              spl_attack  = target.spl_attack;
              defense = target.defense;
              spl_defense = target.spl_defense;
              speed = target.speed;
              status = target.status;
              mods = target.mods;
              cost = target.cost
            } in
            Netgraphics.add_update (Item(string_of_item item,StatModified
              (SpD,1),team,target.species));
            Netgraphics.add_update (
              UpdateSteammon(target.species,target.curr_hp,target.max_hp,team))
            if target.curr_hp > 0 then new_guy::acc
            else
              Netgraphics.add_update (Message
                (target.species ^ " is fainted and can't use XSpeed!"));
              x::acc
          end
      else
        Netgraphics.add_update (Message
          (target.species ^ " not found!"));
        x::acc
    ) [] player.mon_list
  if own_item then
    begin 
      player.inventory <- new_inventory;
      use_on (List.find (fun x -> x.species = s) player.mon_list)
    end
      
  else
    Netgraphics.add_update (Message("None in inventory!"))

let use_move (g:game) (c:color) (move_name: string) : unit = 
  let player = find_player c g in 
  let opponent = find_player (invert_color c) g in 

  let starter = List.hd player.mon_list in 
  let enemy = List.hd opponent.mon_list in 

  let move = 
  add_update(Message (starter.species ^ " used " ^ move_name ^ "!"));
    match get_move_from_steammon starter move_name with

    | None -> failwith "Not a valid move for this steammon" 
    | Some x -> x in 

(*   (*Check if there's any pp left. If not, use Struggle*)
  if !move.pp_remaining = 0 then   Table.find !move_table "Struggle"
  else  *)


  (*Determines if a move hits or misses.*)
  let move_hits = 
    if move.power = 0 && move.target = User then true
    else 
      Random.int 100 < move.accuracy in 


  (* Stores if the move hits, misses, or fails. pp decremented 
   * if the move hits or misses. If the move fails, pp remains the same. *)
  let hit_result : hit_result = 
    match starter.status with 
    | None -> 
(*       move <- {move with pp_remaining = move.pp_remaining - 1};
 *)      if move_hits then Hit else Miss
    | Some status -> match status with 
      | Asleep -> Failed Asleep
      | Frozen -> Failed Frozen 
      | _ -> 
(*         move <- {move with pp_remaining = move.pp_remaining - 1};
 *)        if move_hits then Hit else Miss in 


  let move_effectiveness, steamtype_multiplier = 
    match move.target with
    | User -> calculate_type_matchup move.element 
        (starter.first_type, starter.second_type) 
    | Opponent -> calculate_type_matchup move.element 
        (enemy.first_type, enemy.second_type) in
     

(*   let calc_attackers_attack (attacker:steammon) (att : move) : int =
    if is_special att.element then 
      (multiplier_of_modifier starter.mods.spl_attack_mod) * attacker.spl_attack
    else (multiplier_of_modifier starter.mods.attack_mod) * attacker.attack in
  
  let calc_defenders_defense (defender:steammon) (att : move) : int =
    if is_special att.element then 
      (multiplier_of_modifier starter.mods.spl_defense_mod) * defender.spl_defense
    else (multiplier_of_modifier starter.mods.defense_mod) * defender.defense in *)

  (*see writeup for formula*)
  let calc_multiplier (type_multiplier: float) : float =
    let random_number = 
      float_of_int (100 - Random.int (101 - cMIN_DAMAGE_RANGE)) /. 100.0 in 
    let current_multiplier = match starter.status with 
      | None -> random_number *. type_multiplier 
      | Some x -> match x with 
        | Burned -> random_number *. type_multiplier *. cBURN_WEAKNESS
        | _ -> random_number *. type_multiplier in 

    match starter.first_type , starter.second_type with
    | Some type_1, Some type_2 -> 
      if move.element = type_1 || move.element = type_2 then
        cSTAB_BONUS *. current_multiplier
      else current_multiplier
    | Some type_1, None -> 
      if move.element = type_1 then cSTAB_BONUS *. current_multiplier
      else current_multiplier
    | None, Some type_2 ->
      if move.element = type_2 then cSTAB_BONUS *. current_multiplier
      else current_multiplier
    | None, None -> failwith "Steammon should have at least one steamtype" in 

  (*Stores damage of move. If move misses or is non-damaging, returns 0*)
  let move_damage : int = 
    if move.power = 0 then 0 
    else 
      match hit_result with
       | Hit -> calculate_damage (calc_attackers_attack starter move) 
         (calc_defenders_defense enemy move) move.power 
         (calc_multiplier steamtype_multiplier)
       | _ -> 0 in
  
  let steammon_target = 
    match move.target with 
    | User -> starter 
    | Opponent -> enemy in   

  let effect_result_of_effect = function
    | InflictStatus status -> InflictedStatus status  
    | StatModifier (stat,i) -> StatModified (stat,i)
    | RecoverPercent (i) -> Recovered (i * steammon_target.curr_hp / 100)
    | Recoil i -> Recoiled (i * starter.curr_hp / 100)
    | DamagePercent i ->  Damaged (steammon_target.max_hp * i / 100)
    | HealStatus lst -> HealedStatus (List.hd lst)  
    | RestorePP i -> RestoredPP i in 

  let move_effects = 
    match move.effects with 
    | None -> []
    | Some (effect_list,target,chance) -> 
      let color = match target with 
      | User -> c 
      | Opponent -> invert_color c in 
      if Random.int 100 < chance then 
        List.map (fun effect -> 
          effect_result_of_effect effect,color) effect_list
      else [] in  

  (*information for gui*)
  let move_result = {
    name = move.name;
    element = move.element;
    from = c; 
    toward = invert_color c; 
    damage = move_damage ;
    hit = hit_result;
    effectiveness = move_effectiveness;
    effects = move_effects
  } in 

  add_update(Message (starter.species ^ " used " ^ move.name ^ "!"));
  add_update(Move(move_result))



let handle_ActionRequest (g:game) (c:color) (move_name:string) : unit =
  ()




(* let reset_mods (lst: steammon list) : steammon list = 
  match lst with
  | [] -> failwith "No steammon drafted!"
  | h::t -> 
    {h with mods = default_modifier} :: t in 
    let player = find_player c g in
      begin
        player.mon_list <- reset_mods player.mon_list;
        player.mon_list <- set_active_steammon player.mon_list mon_name;
        send_update (SetChosenSteammon mon_name)
      end *)