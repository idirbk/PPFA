(*
   PPFA
   Boukari Idir 
   Berkennou Brahim
   Université Paris-Saclay
*)

open Tools
open Game
open Network
  
let action_trans_netk_send=ref ""

let rec choose map player = 
  Printf.printf "\nmake a choice  :         \n";
  Printf.printf "\n|1| ===> Atack             ";
  Printf.printf "\n|2| ===> Move              ";
  Printf.printf "\n|3| ===> Send Action  \n";
  Printf.printf "\n Your Choice :: ";
  let ch = read_int () in  
  match ch with 
  | 1 -> begin
          let (x,y) = choose_coords (!(fst player.position),!(snd player.position))  4  map in
          action_trans_netk_send:= !action_trans_netk_send^string_of_int 1 ^ "|"^ string_of_int x ^"|"^string_of_int y^";"; 
          Atack(x,y)
          end
  | 2 -> begin
          let (x,y) = choose_coords (!(fst player.position),!(snd player.position)) 1000  map in
          action_trans_netk_send:= !action_trans_netk_send^string_of_int 2 ^ "|"^ string_of_int x ^"|"^ string_of_int y^";";
          Move(x,y);
        end   
  | 3 ->begin 
         action_trans_netk_send := !action_trans_netk_send^string_of_int 3 ^";" ;
         DoNothing;
        end 
  | _ -> begin
          clear;
          print_map map;
          choose map player
        end

  
  
let rec game_loop ic oc =
  let id = read_num ic in
  let map = read_map ic in
  let players_list = ref (read_perso ic) in
  Printf.printf "player N° %d\n" id;
  print_player !players_list;
  print_map map;

  let player =  List.nth !players_list id in

  let choice = choose map player in
  match choice with
  | Atack(x,y) ->begin
                  attack player map players_list x y; 
                  clear; 
                  print_map map;
                  end
  | Move(x,y) -> begin
                  move id player map  x y;  
                  clear;
                  print_map map;
                  end
  | DoNothing -> begin
                      clear;
                      print_map map;
                      send_action oc action_trans_netk_send;
                      action_trans_netk_send := ""

                  end
    

  
  
let perso_list = [invincible;invincible;invincible];;
let ic,oc = install_client "92.89.116.186";;
send_pseudo oc "idir";;
send_perso oc perso_list;;
game_loop ic oc;;

  
