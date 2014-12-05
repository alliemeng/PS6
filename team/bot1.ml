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

exception NoMatchException

let findmaxhp (stpool:steam_pool) (initacc: steammon): steammon =
   List.fold_left (fun acc elm ->  if elm.max_hp > acc.max_hp 
                                   then elm else acc) initacc stpool

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

  (* let findinsp (field: string)  *)
let findcosteff (steampool: steam_pool) (maxcost:int) : steam_pool =
  List.filter (fun elm -> elm.cost < maxcost) steampool

let checkforopweaknesses (opp:steammon) (efflevel:effectiveness) (inplay: steammon) : action =
  let optype1 = (match opp.first_type with
    | Some t -> t
    | None -> Typeless) in
  let optype2 = (match opp.second_type with
    | Some t -> t
    | None -> Typeless) in
  if ((weakness optype1 inplay.first_move.element) = efflevel ||
  (weakness optype2 inplay.first_move.element) = efflevel &&
  (inplay.first_move.pp_remaining > 0)) then
    let _ = print_endline (inplay.species ^ "used " ^ ((inplay.first_move).name)) in
    UseMove(inplay.first_move.name)
  else if ((weakness optype1 inplay.first_move.element) = efflevel ||
  (weakness optype2 inplay.second_move.element) = efflevel &&
  (inplay.second_move.pp_remaining > 0)) then
    let _ = print_endline (inplay.species ^ "used " ^ ((inplay.second_move).name)) in
    UseMove(inplay.second_move.name)
  else if ((weakness optype1 inplay.first_move.element) = efflevel ||
  (weakness optype2 inplay.third_move.element) = efflevel &&
  (inplay.third_move.pp_remaining > 0)) then
    let _ = print_endline (inplay.species ^ "used " ^ ((inplay.third_move).name)) in
    UseMove(inplay.third_move.name)
  else if ((weakness optype1 inplay.first_move.element) = efflevel ||
  (weakness optype2 inplay.fourth_move.element) = efflevel &&
  (inplay.fourth_move.pp_remaining > 0)) then
      let _ = print_endline (inplay.species ^ "used " ^ ((inplay.fourth_move).name)) in
      UseMove(inplay.fourth_move.name) 
  else
    raise NoMatchException

let rec weredoomed (remlist: steammon list) : action =
  match remlist with
    | h::t ->
        if (h.first_move).pp_remaining >0 then
          let _ = print_endline (h.species ^ "used " ^ ((h.first_move).name)) in
          UseMove((h.first_move).name)
        else if (h.second_move).pp_remaining > 0 then
          let _ = print_endline (h.species ^ "used " ^ ((h.second_move).name)) in
          UseMove((h.second_move).name)
        else if (h.third_move).pp_remaining > 0 then
          let _ = print_endline (h.species ^ "used " ^ ((h.third_move).name)) in
          UseMove((h.third_move).name)
        else if (h.fourth_move).pp_remaining > 0 then
          let _ = print_endline (h.species ^ "used " ^ ((h.fourth_move).name)) in
          UseMove((h.fourth_move).name) 
        else weredoomed t
    | _ -> failwith "We done goofed" 

let rec checkteam (opp: steammon) (team: steammon list) (eff: effectiveness) : action =
  match team with
  | [] -> raise NoMatchException
  | h::t -> 
    try 
      ignore (checkforopweaknesses opp eff h);
      SwitchSteammon(h.species)
    with _ -> checkteam opp t eff

 (* allows the bot to know what color it is. *)
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
      let (a1,b1) = gsd in
      let my_team = if c = Red then a1 else b1 in
      let (mons,items,credits) = my_team in 
      let lengthmons = List.length mons in
      (*pick best defender*)
      if lengthmons = 0 then 
        let avail = findcosteff sp (int_of_float ((float_of_int cSTEAMMON_CREDITS)*. (3.0/.8.0))) in
        let picked = (List.fold_left (fun acc elm ->
        if elm.defense > acc.defense then elm else acc) (List.hd avail) avail) in 
        let (attackers,defenders) = !roleslists in 
        roleslists := (attackers, picked::defenders);
        PickSteammon(picked.species) 

      (*pick best special attacker*)
      else if lengthmons = 1 then
        let avail = findcosteff sp (int_of_float ((float_of_int cSTEAMMON_CREDITS)*. (3.0/.8.0))) in
        let picked = List.fold_left (fun acc elm ->
        if elm.spl_attack > acc.spl_attack then elm else acc) (List.hd avail) avail in
        let (attackers,defenders) = !roleslists in 
        roleslists := (picked::attackers, defenders);
        PickSteammon(picked.species) 

      (*pick best special defender*)
      else if lengthmons = 2 then
        let avail = findcosteff sp (int_of_float ((float_of_int cSTEAMMON_CREDITS)*. (1.0/.8.0))) in
        let picked = List.fold_left (fun acc elm ->
        if elm.spl_defense> acc.spl_defense then elm else acc) (List.hd avail) avail in
        let (attackers,defenders) = !roleslists in 
        roleslists := (attackers, picked::defenders);
        PickSteammon(picked.species)

      (*pick best attacker*)
      else if lengthmons = 3 then
        let avail = findcosteff sp (int_of_float ((float_of_int cSTEAMMON_CREDITS)*. (1.0/.8.0))) in
        let picked = List.fold_left (fun acc elm ->
        if elm.attack > acc.attack then elm else acc) (List.hd avail) avail in
        let (attackers,defenders) = !roleslists in 
        roleslists := (picked::attackers, defenders);
        PickSteammon(picked.species) 

      (*pick best defender*)
      else if lengthmons = 4 then
        let avail = findcosteff sp (int_of_float ((float_of_int credits)/.2.0)) in
        let picked = List.fold_left (fun acc elm ->
        if elm.defense > acc.defense then elm else acc) (List.hd avail) avail in 
        let (attackers,defenders) = !roleslists in 
        roleslists := (attackers, picked::defenders);
        PickSteammon(picked.species)

      (*pick best special attacker*)
      (*if lengthmons = 5*)
      else 
        let avail = findcosteff sp credits in
        let picked = List.fold_left (fun acc elm ->
        if elm.spl_attack> acc.spl_attack then elm else acc) (List.hd avail) avail in 
        let (attackers,defenders) = !roleslists in 
        roleslists := (picked::attackers, defenders);
        PickSteammon(picked.species) 

      (* let opp_team = if my_team = a1 then b1 else a1 in
      let (omons,oinv,ocredits) = opp_team in 
      if omons = [] then pickfirst sp *)

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
        if ((List.exists (fun x -> x.curr_hp <> 0) mons) = true) then
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
          with _ ->
            try (List.find (fun x ->
            (weakness (match x.first_type with 
            | Some t -> t
            | None -> Typeless) (match ((List.hd (opmons)).second_type) with
            | Some t -> t
            | None -> Typeless)) = SuperEffective) mons)
            with _ -> List.fold_right (fun a acc -> 
              if (acc.attack >= a.attack) then acc
              else a) (fst (!roleslists)) (List.hd (fst (!roleslists))) in
        SelectStarter(pick.species)
    
    (*if hp < 40%, use potion
    elseif party member is dead, use revive
    elseif steammon is burned, frozen, poisoned, use full heal
    elseif PP > 0, select super-effective move
    elseif PP > 0, select non-notveryeffective move
    elseif swap steammon to one with above criteria 
    else use whatever still has pp*)
    | ActionRequest (gr) ->
      let (a1, b1) = gr in 
      let my_team = if c = Red then a1 else b1 in
      let op_team = if c = Red then b1 else a1 in
      let (mons, pack, credits) = my_team in
      let (opmons, oppack, opcredits) = op_team in
      let inplay = List.hd mons in
      let ins = match inplay.status with
        | Some t -> t
        | None -> Frozen in
      let fainted = List.filter (fun elm -> elm.curr_hp = 0) mons in
      (* let optype1 = (match (List.hd opmons).first_type with
        | Some t -> t
          | None -> Typeless) in
      let optype2 = (match (List.hd opmons).second_type with
        | Some t -> t
          | None -> Typeless) in *)
      (* HP < 40% *)
      if (float_of_int (inplay.curr_hp) < (0.4 *. float_of_int (inplay.max_hp))) then
        UseItem((MaxPotion, inplay.species))
      (* Party member is dead *)
      else if fainted <> [] then
          let defender = List.fold_right (fun a acc -> 
              if (acc.defense >= a.defense) then acc
              else a) (snd (!roleslists)) (List.hd (snd (!roleslists))) in
          let spattacker = List.fold_right (fun a acc -> 
              if (acc.spl_attack >= a.spl_attack) then acc
              else a) (fst (!roleslists)) (List.hd (fst (!roleslists))) in
          if List.mem defender fainted then UseItem((Revive,defender.species))
          else if List.mem spattacker fainted then UseItem((Revive,spattacker.species))
          else UseItem((Revive, (findmaxhp fainted (List.hd (fainted))).species))
      (* Burned, frozen , poisoned *)
      else if (inplay.status <> None) && (ins = Burned || ins = Frozen || ins = Poisoned) then
        UseItem((FullHeal, inplay.species))
      (* Look for super-effective move and check PP *)
      else (
        try 
          (checkforopweaknesses (List.hd opmons) SuperEffective inplay)
        with _ -> ( 
          (* Check for other party members with super-effective move *)
          try 
            checkteam (List.hd opmons) mons SuperEffective
          with _ -> (
            (* Look for any move that that is Regular effectiveness and has PP *)
            try 
              checkforopweaknesses (List.hd opmons) Regular inplay
            with _ -> (
              (* Check for other party members with regular effectiveness and has PP *)
              try 
                checkteam (List.hd opmons) mons Regular
              with _ ->
                (* Check for any PP-available moves *)
                weredoomed mons))))


      (* else if ((weakness optype1 inplay.first_move.element) = SuperEffective ||
      (weakness optype2 inplay.first_move.element) = SuperEffective) then
        if (inplay.first_move.pp_remaining > 0) then
          UseMove(inplay.first_move.name)
      else if ((weakness optype1 inplay.first_move.element) = SuperEffective ||
      (weakness optype2 inplay.second_move.element) = SuperEffective) then
        if (inplay.second_move.pp_remaining > 0) then
          UseMove(inplay.second_move.name)
      else if ((weakness optype1 inplay.first_move.element) = SuperEffective ||
      (weakness optype2 inplay.third_move.element) = SuperEffective) then
        if (inplay.third_move.pp_remaining > 0) then
          UseMove(inplay.third_move.name)
      else if ((weakness optype1 inplay.first_move.element) = SuperEffective ||
      (weakness optype2 inplay.fourth_move.element) = SuperEffective) then
        if (inplay.fourth_move.pp_remaining > 0) then
          UseMove(inplay.fourth_move.name) *)
      
      
     (*  else if ((weakness optype1 inplay.first_move.element) = Regular ||
      (weakness optype2 inplay.first_move.element) = Regular) then
        if (inplay.first_move.pp_remaining > 0) then
          UseMove(inplay.first_move.name)
      else if ((weakness optype1 inplay.first_move.element) = Regular ||
      (weakness optype2 inplay.second_move.element) = Regular) then
        if (inplay.second_move.pp_remaining > 0) then
          UseMove(inplay.second_move.name)
      else if ((weakness optype1 inplay.first_move.element) = Regular ||
      (weakness optype2 inplay.third_move.element) = Regular) then
        if (inplay.third_move.pp_remaining > 0) then
          UseMove(inplay.third_move.name)
      else if ((weakness optype1 inplay.first_move.element) = Regular ||
      (weakness optype2 inplay.fourth_move.element) = Regular) then
        if (inplay.fourth_move.pp_remaining > 0) then
          UseMove(inplay.fourth_move.name)  *)              

     

     
let () = run_bot handle_request