open Team
open Definitions
open Constants
open Util

(*Bot 1 Strategy:
  Pickteam: Choose steammon with highest hp that has a type not yet 
  represented on the team. Motive: Maximize team health and type variety
  Pickitem:
  Pickstarter:
  Pickaction: If the current Steammon has a current hp less than 25%, switch it out
  Overall: This bot is more inward looking, moderately aggressive*)

let name = "BOT" 
let _ = Random.self_init ()
let roleslists = ref ([],[])

let pickInventoryHelper () : int list = 
  let inventarray = Array.make 7 0 in
  let nmaxpotion = (int_of_float ((float_of_int cINITIAL_CASH) *. (5.0/.8.0)))/cCOST_MAXPOTION in
  let remainingcash = (cINITIAL_CASH - (nmaxpotion*cCOST_MAXPOTION)) in 
  let nrevive = (int_of_float ((float_of_int remainingcash)*. (3.0/.4.0)))/cCOST_REVIVE in
  let remcash = remainingcash - (nrevive* cCOST_REVIVE) in
  let nfullheal = if remcash > cCOST_FULLHEAL then remcash/cCOST_FULLHEAL
                  else 0 in
  Array.set inventarray 1 nmaxpotion;
  Array.set inventarray 3 nrevive;
  Array.set inventarray 2 nfullheal;
  Array.to_list inventarray



(* handle_request c r responds to a request r by returning an action. The color c 
 * allows the bot to know what color it is. *)
let handle_request (c : color) (r : request) : action =
  match r with
    | TeamNameRequest -> SendTeamName(name)

    (* Sent during the inventory phase to request that the player
       purchase an inventory. *)
    | PickInventoryRequest (gsd) -> 
        PickInventory(pickInventoryHelper ())

   (* Sent during the draft phase to request that the player 
       draft a Steammon. *)
    | PickRequest(col, gsd, moves, sp) ->
        PickSteammon((List.hd sp).species)

    (* Sent at the beginning ofSteamon() the battle phase, or when the active 
     * Steammon faints. *)
    | StarterRequest(gsd)->
        let (a1,b1) = gsd in
        let my_team = if c = Red then a1 else b1 in
        let op_team = if c = Red then b1 else a1 in
        let (mons, pack, credits) = my_team in
        let (opmons, oppack, opcredits) = op_team in
        let pick = 
          (* Have not yet entered battle stage *)
          if ((List.exists (fun x -> x.curr_hp != 0) mons) = true) then
            (* Pick best defender *)
            List.fold_right (fun a acc -> 
              if (acc.defense >= a.defense) then acc
              else a) (snd (!roleslists)) (List.hd (snd (!roleslists)))
          (* One of your steammon has fainted *)
          else
            (* Look for super-effective move in party steammon *)
            try (List.find (fun x ->
              (weakness (match x.first_type with 
              | Some t -> t
              | None -> Typeless) (match ((List.hd (opmons)).first_type) with
              | Some t -> t
              | None -> Typeless)) = SuperEffective) mons)
            (* Select steammon with strongest attack *)
            with _ -> List.fold_right (fun a acc -> 
              if (acc.attack >= a.attack) then acc
              else a) (fst (!roleslists)) (List.hd (fst (!roleslists))) in
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
