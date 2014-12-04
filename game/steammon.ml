open Util
open Definitions

module Steammon : OrderedType = struct
type t = steammon
let compare x y = 
  if x.species < y.species then -1 
  else if x.species = y.species then 0
  else 1  
end

module SteammonSet = Set.Make(Steammon)

let set_of_list (lst: steammon list) : SteammonSet.t = 
  List.fold_left (fun acc elem -> 
    if SteammonSet.mem elem acc then acc else SteammonSet.add elem acc) 
    SteammonSet.empty lst 
