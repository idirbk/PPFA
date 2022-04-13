(*
   PPFA
   Boukari Idir 
   Berkennou Brahim
   Université Paris-Saclay
*)

open Tools
open Game
open Network
open Graphics
open Draw

let actions = ref "";;

let init () =
  init_window ();
  let perso_list = [invincible;invincible;invincible] in
  let ic,oc = install_client "92.89.116.186" in
  send_pseudo oc "idir";
  send_perso oc perso_list;
  ic,oc

let rec main_loop ic oc= 
    let id = read_num ic in
    let map = read_map ic in
    let players_list = ref (read_perso ic ) in 
    draw_map map;
    play_moves map !(players_list) id actions;
    main_loop ic oc;
    read_key(); 
    close_graph();
    ()

let ic,oc = init ();;
main_loop ic oc;;

(*


let rec choose map player = 
  Printf.printf "\nPlayer pm == %d \n" !(player.pm);
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
  | 2 when !(player.pm) > 0 -> begin
          let (x,y) = choose_coords (!(fst player.position),!(snd player.position)) !(player.pm)  map in
          action_trans_netk_send:= !action_trans_netk_send^string_of_int 2 ^ "|"^ string_of_int x ^"|"^ string_of_int y^";";
          Move(x,y);
        end   
  | 3 ->begin 
         action_trans_netk_send := String.sub !action_trans_netk_send 0 ((String.length !action_trans_netk_send ) -1);
         DoNothing;
        end 
  | _ -> begin
          clear;
          print_map map;
          choose map player
        end

  


let rec choice map players_list player id  = 
  let  choix = choose map player in
  match choix with
  | Atack(x,y) ->begin
                  attack player map players_list x y; 
                  clear; 
                  print_map map;
                  choice map players_list player id
                  end
  | Move(x,y) -> begin
                  move id player map  x y;  
                  clear;
                  print_map map;
                  choice map players_list player id 
                  end
  | DoNothing -> begin
                      clear;
                      print_map map;

                  end


let rec game_loop ic oc =
  let id = read_num ic in
  let map = read_map ic in
  let players_list = ref (read_perso ic ) in 
  Printf.printf "player N° %d\n" id;
  print_player !players_list;
  print_map map;

  let player =  List.nth !players_list id in
  choice map players_list player id;
  send_action oc action_trans_netk_send;
  Printf.printf "%s\n%!" !action_trans_netk_send;
  action_trans_netk_send := "";
  game_loop ic oc

  
  
let perso_list = [invincible;invincible;invincible];;
let ic,oc = install_client "92.89.116.186";;
send_pseudo oc "idir";;
send_perso oc perso_list;;
game_loop ic oc;;

*)