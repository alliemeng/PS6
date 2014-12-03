open Team
open Definitions
open Constants

(*Bot 1 Strategy:
  Pickteam: Choose steammon with highest hp that has a type not yet 
  represented on the team. Motive: Maximize team health and type variety
  Pickitem:
  Pickstarter:
  Pickaction: If the current Steammon has a current hp less than 25%, switch it out
  Overall: This bot is more inward looking, moderately aggressive*)

let name = "BOT" 

let _ = Random.self_init ()

let filterfortype (stp:steam_pool) (currentsteams:steammon list): steammon list =
  match currentsteams with 
    | [] -> stp
    | h::t -> List.fold_left (fun acc stelm -> 
              if (List.fold_left (fun acc cselm -> 
              (stelm.first_type <> cselm.first_type 
              && stelm.first_type <> cselm.second_type
              && stelm.second_type <> cselm.first_type
              && stelm.second_type <> cselm.second_type) && acc) 
              true currentsteams)
              then stelm::acc else acc)
              [] stp

let findmaxhp (stpool:steam_pool) (initacc: steammon): steammon =
   List.fold_left (fun acc elm ->  if elm.max_hp > acc.max_hp 
                                   then elm else acc) initacc stpool

(*Chooses the Steammon with the highest hp that is of a type not yet represented in the list*)
let pickanalysis (spool:steam_pool) (mysteams:steammon list): action = 
   match spool with
      | h::t ->
      (*Always get chansey first???*)
        let typefiltered = filterfortype spool mysteams in
        if typefiltered <> [] then 
        PickSteammon((findmaxhp typefiltered (List.hd typefiltered)).species)
        else PickSteammon((findmaxhp spool h).species)
      | [] -> failwith "no steammon to pick!"

(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> SendTeamName(name)

    (* Sent during the inventory phase to request that the player
       purchase an inventory. *)
    | PickInventoryRequest (gsd) -> 
        let (a1,b1) = gsd in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in 

          PickInventory(
          [cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
           cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XSPEED])

   (* Sent during the draft phase to request that the player 
       draft a Steammon. *)
    | PickRequest(col, gsd, moves, sp) ->
        let (a1,b1) = gsd in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack, credits) = my_team in
        pickanalysis sp mons

    (* Sent at the beginning of the battle phase, or when the active 
     * Steammon faints. *)
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
    (*Notes: Check if target is self before using attack.*)
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
