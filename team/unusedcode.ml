 (* let itemcosts = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_FULLHEAL; 
    cCOST_REVIVE; cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED]
  let cheapitem = List.fold_left (fun acc elm -> if elm < acc then elm
                                else acc) maxint itemcosts *)


(*let filterfortype (stp:steam_pool) (currentsteams:steammon list): steammon list =
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
      | [] -> failwith "no steammon to pick!" *)