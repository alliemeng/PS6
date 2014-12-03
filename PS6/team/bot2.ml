(* open Team
open Definitions
open Constants

(*Bot 2 Strategy:
  Pickteam: Choose steammon with highest hp that has a type which counters that of 
  the last chosen steammon on the opponents team.
  Pickitem:
  Pickstarter:
  Pickaction: 
  Overall: This bot is more outward looking*)

let name = "BOT2" 

let _ = Random.self_init ()


let pickanalysis (opp:team data) (sp: steam_pool): steammon = 
  
(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> SendTeamName(name)

    (* Sent during the inventory phase to request that the player
       purchase an inventory. *)
   | PickInventoryRequest (gr) -> PickInventory(
          [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
           cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])

   (* Sent during the draft phase to request that the player 
       draft a Steammon. *)
    | PickRequest(col, gsd, moves, sp) ->
        let (a1,b1) = gsd in
        let my_team = if c = Red then a1 else b1 in
        let opp_team = if my_team = a1 then b1 else a1 in
        let chosen = pickanalysis opp_team sp





        let (mons, pack, credits) = in
        (match sp with
         | h::t ->
            let length = List.length sp in
            let my_pick = List.nth sp (Random.int length) in
              PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")

    Sent at the beginning of the battle phase, or when the active 
     * Steammon faints.
    | StarterRequest(gsd)->
        let (a1,b1) = gsd in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        let pick = 
          try List.find(fun x -> x.curr_hp > 0) mons 
          with _ -> (List.hd mons) in
          SelectStarter(pick.species)
     
    (* Sent during the battle phase to request that the player
    chose its next action. *)
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        (match mons with
        | h::t ->
            if (h.first_move).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_move).name)) in
                UseMove((h.first_move).name)
            else if ((h.second_move).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_move).name)) in
                UseMove((h.second_move).name)
            else if ((h.third_move).pp_remaining >0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_move).name)) in
                UseMove((h.third_move).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_move).name)) in
                UseMove((h.fourth_move).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
        
let () = run_bot handle_request
 *)