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

let bootMode = ref false;;
let actions = ref "";;
let color = ref white;;
let players = ref [invincible];;

let init () =
  let argc_length = Array.length Sys.argv in
  if argc_length != 2 then (exit (-1));
  if Sys.argv.(1) = "0" then bootMode := false  else bootMode := true;

  init_window ();
  let perso_list = [invincible;invincible;invincible] in
  let ic,oc = install_client "127.0.0.1" in
  send_pseudo oc "idir";
  send_perso oc perso_list;
  ic,oc

let rec main_loop ic oc= 
    let id = read_num ic in
    if id < 6   then
      begin
        let map = read_map ic in
        let players_list = ref (read_perso ic ) in 
        players := !players_list;
        draw_map map !players_list id;
        play_moves map !(players_list) id actions !bootMode;
        send_action oc actions;
        actions := "";
        color := if (id mod 2) = 0 then red else blue;
        main_loop ic oc;
      end
    else
        if id = 6 && not(game_over !players) then draw_Loose !color else draw_Win !color;
    let _ = wait_next_event [Key_pressed] in
    close_graph();
    ()

let ic,oc = init ();;
main_loop ic oc;;

