(*
   PPFA
   Boukari Idir 
   Berkennou Brahim
   Université Paris-Saclay
*)

open Tools
open Game
open Network





let action_trans_netk_send=ref "";;
    
let rec choose map player = 
  Printf.printf "\nmake a choice  :         \n";
  Printf.printf "\n|1| ===> Atack             ";
  Printf.printf "\n|2| ===> Move              ";
  Printf.printf "\n|3| ===> pass your turn  \n";
  Printf.printf "\n Your Choice :: ";
  let ch = read_int () in  
  match ch with 
  | 1 -> begin
          let (x,y) = choose_coords (!(fst player.position),!(snd player.position))  4  map in
          action_trans_netk_send:= !action_trans_netk_send^string_of_int 1 ^ "|"^ string_of_int x ^"|"^string_of_int y^"~"; 
          Atack(x,y)
          end
  | 2 -> begin
          let (x,y) = choose_coords (!(fst player.position),!(snd player.position)) 1000  map in
          action_trans_netk_send:= !action_trans_netk_send^string_of_int 2 ^ "|"^ string_of_int x ^"|"^ string_of_int y^"~";
          Move(x,y);
        end   
  | 3 ->begin 
         action_trans_netk_send := !action_trans_netk_send^string_of_int 3 ^"~" ;
         DoNothing;
        end 
  | _ -> begin
          clear 0;
          print_map map;
          choose map player
        end
;;
  




  
let rec game_loop map players_list id =
  Printf.printf "player N° %d\n" id;
  try 
      let player =  List.assoc id !players_list in
      let choice = choose map player in
      match choice with
      | Atack(x,y) ->begin
                      attack player map players_list x y; 
                      clear 0; 
                      print_map map;
                      game_loop map players_list ((id+1) mod 6)
                     end
      | Move(x,y) -> begin
                      move id player map  x y;  
                      clear 0;
                      print_map map;
                      game_loop map players_list ((id+1) mod 6)
                     end
      | DoNothing -> begin
                          clear 0;
                          print_map map;
                          game_loop map players_list ((id+1) mod 6)
                          
                     end
    
  with _ -> game_loop map players_list ((id+1) mod 6)
  
  
;;

  
let players_list = ref [];;
let m = init_map 20 30;;
init_players m players_list;;
init_obstacle m;;
print_map m;;
game_loop m players_list 0;;
(*
let s = map_translation_send m;;
Printf.printf "\n map -> CC  : \n %s  \n" s;;
let pls = player_translation_send invincible;;
Printf.printf"\n the invincible translation -> :%s \n" pls;;
Printf.printf"\n La liste des actions traduites est ->: %s \n" (String.sub !action_trans_netk_send 0 ((String.length !action_trans_netk_send)-1));;

*)


