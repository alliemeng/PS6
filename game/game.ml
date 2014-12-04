open Definitions
open Util
open Constants
open Netgraphics
open GameHelper

type game = State.t 
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

let handle_step (g:game) (ra:command) (ba:command) : game_output =
  match !last_request_sent with
    | TeamNameRequest -> (match ra,ba with
      | Action (SendTeamName red_name), Action (SendTeamName blue_name) -> 
          handle_SendTeamName g red_name blue_name
      | DoNothing, Action (SendTeamName blue_name) -> 
          handle_SendTeamName g "Red" blue_name 
      | Action (SendTeamName red_name), DoNothing -> 
          handle_SendTeamName g red_name "Blue" 
      | DoNothing, DoNothing -> handle_SendTeamName g "Red" "Blue"
      | _,_ -> failwith "Invalid response from bot(s)")
    | PickRequest c -> (match ra,ba with
      | Action (PickSteammon steammon), DoNothing ->
          handle_PickSteammon g Red steammon
      | DoNothing, Action (PickSteammon steammon) ->
          handle_PickSteammon g Blue steammon
      | DoNothing, DoNothing -> handle_PickSteammon g c ""
      | _,_ -> failwith "Invalid response from bot(s)")
    | PickInventoryRequest -> (match ra,ba with
      | Action (PickInventory red_inv), Action (PickInventory blue_inv) ->
          handle_PickInventory g red_inv blue_inv
      | Action (PickInventory red_inv), DoNothing ->
          handle_PickInventory g red_inv []
      | DoNothing, Action (PickInventory blue_inv) ->
          handle_PickInventory g [] blue_inv
      | DoNothing, DoNothing -> handle_PickInventory g [] []
      | _,_ -> failwith "Invalid response from bot(s)")
    | StarterRequest -> (match ra,ba with
      | Action (SelectStarter red_starter), Action (SelectStarter blue_starter)->
          handle_SelectStarter g red_starter blue_starter
      | Action (SelectStarter red_starter), DoNothing ->
          handle_SelectStarter g red_starter ""
      | DoNothing, Action (SelectStarter blue_starter)->
          handle_SelectStarter g "" blue_starter
      | DoNothing, DoNothing -> handle_SelectStarter g "" ""
      | _,_ -> failwith "Invalid response from bot(s)")
    | ActionRequest -> 
        let find_first () : unit =
          let red_speed = (List.hd g.red.mon_list).speed in
          let blue_speed = (List.hd g.blue.mon_list).speed in
          if red_speed > blue_speed then 
            begin
              first := Red;
              Netgraphics.add_update (SetFirstAttacker(Red))
            end 
          else if blue_speed > red_speed then 
            begin
              first := Blue;
              Netgraphics.add_update (SetFirstAttacker(Blue))
            end   
          (*Pick random team to start*)       
          else 
            begin
              if Random.bool () then
                begin 
                  first := Red;
                  Netgraphics.add_update (SetFirstAttacker(Red))
                end
              else
                begin 
                  first := Blue;
                  Netgraphics.add_update (SetFirstAttacker(Blue))  
                end 
            end
                
             in 

        let active_fainted (c:color) : bool = 
          let active_mon = List.hd (find_player c g).mon_list in 
          active_mon.curr_hp = 0 in 

   (*      let remove_status_effects (c:color) : unit = 
          let lst = (find_player c g).mon_list in 
          match  lst with
          | h::t -> lst <- {h with status = None} :: t
          | [] -> failwith "No steammon in team!!" in  *)


        let act (c:color) (action:command) : command option =
          match action with
          | Action (SwitchSteammon s) ->
              switch_steammon g c s;
              let game_data = game_datafication g in
              Some(Request(ActionRequest(game_data)))

          | Action (UseItem (item,target)) ->
              use_item g c item target;
              let game_data = game_datafication g in
              if active_fainted c then
                begin 
                  (* remove_status_effects c; *)
                  last_request_sent := StarterRequest;
                  Some(Request(StarterRequest(game_data)))
                end
              else
                Some(Request(ActionRequest(game_data)))

          | Action (UseMove m) ->
              use_move g c m;
              let game_data = game_datafication g in
              if active_fainted c then
                begin 
                  (* remove_status_effects c; *)
                  last_request_sent := StarterRequest;
                  Some(Request(StarterRequest(game_data)))
                end
              else
                Some(Request(ActionRequest(game_data)))

          | DoNothing ->
            let game_data = game_datafication g in 
            Some(Request(ActionRequest(game_data))) 
            
            
          | _ -> failwith "Invalid bot response" in 
        send_update (Message ((string_of_color !first)^" goes first"));
        find_first (); 
        (*Placeholder, fix later*)
        if !first = Red then
            let r = act Red ra in
            let b = act Blue ba in 
            (None, game_datafication g, act Red ra, act Blue ra)

        else let b = act Blue ba in let r = act Red ra in 
        (None, game_datafication g, act Red ra, act Blue ra)





let init_game () : game * request * request * move list * steammon list =
  let s = State.create () in 

    Initialization.init_pool "moves.csv" "steammon.csv";
    
    move_table := Initialization.move_table;
    mon_table := Initialization.mon_table;
    if Random.bool () then first := Red else first := Blue;
    last_request_sent := TeamNameRequest;
    (s, TeamNameRequest,TeamNameRequest,hash_to_list (!move_table), hash_to_list (!mon_table))
