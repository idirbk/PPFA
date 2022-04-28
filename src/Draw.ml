open Graphics
open Game
open Network
open Tools
open Bot

let size = 30
let gray = (rgb 100 100 100)



(* fill a list of rectangles *)
let fill_rects list color h =
  set_color color;
  List.iter (fun x -> fill_rect ((snd x)*size) ( (h-(fst x))*size) size size) list;
  set_color white;
  List.iter (fun x -> draw_rect ((snd x)*size) ( (h-(fst x))*size) size size) list

(* draw a list of rectagles *)
let draw_rects list color h =
  set_color color;
  List.iter (fun x -> draw_rect ((snd x)*size) ( (h-(fst x))*size) size size) list

let draw_cadriage i j var=
  set_color cyan;
  for k = 1 to 3
  do 
    moveto (j*size) ((var-i)*(size)+((k*size)/4));
    lineto (j*(size)+size) ((var-i)*size+((k*size)/4));
    moveto (j*size+(k*size)/4) ((var-i)*size);
    lineto (j*(size)+(k*size)/4) ((var-i+1)*size);
  done
  

(* display the map *)
let draw_map map players_list num=
set_color (rgb 100 100 100);
fill_rect 0 0 (size*map.width) (size*map.height);
let var = map.height-1 in
  for i = 0 to map.height-1
  do
    for j = 0 to map.width-1
    do
      match map.grid.(i).(j) with
      |Empty     -> begin
                      set_color white;
                      draw_rect (j*size) ((var-i)*size) size size;
                    end
      |Wall      -> begin
                      set_color black;
                      fill_rect (j*size) ((var-i)*size) size size;
                      set_color white;
                      draw_rect (j*size) ((var-i)*size) size size;
                      draw_cadriage i j var;
                    end
      |Player(id)-> begin
                      if id mod 2 = 0 then set_color red else  set_color blue;
                      fill_circle (j*size + size/2) ((var-i)*size + size/2) (size/3);
                      let player = List.nth players_list id in
                      set_color white;
                      moveto (j*size + size/3) ((var-i)*size + size/3);
                      draw_string (string_of_int !(player.life));
                    end
    done
  done


(* display a move step by step *)
let display_move l c players id map = 
  let player = List.nth players id in
  let way =  List.rev (pcc !(fst (player.position)) !(snd (player.position)) l c map) in
  let player_col = if id mod 2 == 0 then red else blue in
  map.grid.(!(fst (player.position))).(!(snd (player.position))) <- Empty;
  map.grid.(l).(c) <- Player(id);
  List.iter (fun e -> let nc = (snd e)in
                      let nl = (fst e)in
                      wait 100.0;
                      fill_rects [(!(fst (player.position)),!(snd (player.position)))] gray (map.height-1);
                      set_color player_col;
                      fill_circle (nc*size + size/2) (((map.height-1)-nl)*size + size/2) (size/2);
                      (fst (player.position)) := nl;
                      (snd (player.position)) := nc;
                      (player.pm) := !(player.pm)-1;
      ) way;
      (player.pm) := !(player.pm)+1


(*init the window *)
let init_window ()= 
  open_graph " 600x600" ;
  set_window_title "Street Fighter" ;
  set_color (rgb 100 100 100);
  fill_rect 0 0 (size*20) (size*20)



(*choose move in the key board
  m  ====> Move 
  a  ====> Attack
  s  ====> send the actions
  *)
let rec choose_move () = 
  let e = wait_next_event [Key_pressed] in
  match e.key with
    |'a'-> Atack(0,0)
    |'m'-> Move(0,0)
    |'s'-> DoNothing
    | _ -> choose_move()
  

(* choose a coordinate by clicking with the mouse *)
let rec get_coords h list= 
  
  let e = wait_next_event [Button_down] in
  let x =(e.mouse_x/size) in
  let y =(h-e.mouse_y/size) in
  if (List.exists (fun e -> (snd e) = x && (fst e) = y) list) then
    y,x
  else
    get_coords h list

  

let rec  play_moves map player_list id actions bootMode=
  if (not bootMode) then
    let player = (List.nth player_list id) in
    match choose_move() with
      |Atack(_,_) -> if !(player.pa) > 0 then
                      begin
                        let list =get_possible_attacks map player (5) in 
                        draw_rects list magenta (map.height-1);
                        let (l,c) = get_coords (map.height-1) list in
                        attack map player_list id l c;
                        draw_rects list white (map.height-1);
                        add_shot actions (Atack(l,c));
                        draw_map map player_list id;
                      end;
                      (play_moves map player_list id actions bootMode)
      |Move(_,_)  -> if !(player.pm) > 0 then
                      begin
                        let list =get_possible_moves map player (!(player.pm)) in 
                        fill_rects list magenta (map.height-1);
                        draw_rects list black (map.height-1);
                        let (l,c) = get_coords (map.height-1) list in
                        fill_rects list (rgb 100 100 100) (map.height-1);
                        add_shot actions (Move(l,c));
                        display_move l c player_list id map;
                        draw_map map player_list id;
                      end;
                      (play_moves map player_list id actions bootMode)
      |DoNothing  -> if (String.length (!actions)) > 0 
                    then 
                        actions :=( String.sub !actions 0 ((String.length !actions) -1))
                    else 
                        (play_moves map player_list id actions bootMode)
  else
    let shot = bestshot map player_list id in
    match shot with 
    |Atack(l,c) ->begin
                    attack map player_list id l c;
                    add_shot actions (Atack(l,c));
                    draw_map map player_list id;
                  end;
                  (play_moves map player_list id actions bootMode)
    |Move(l,c)  ->begin
                    add_shot actions (Move(l,c));
                    display_move l c player_list id map;
                    draw_map map player_list id;
                  end;
                  (play_moves map player_list id actions bootMode)
    |DoNothing  ->if (String.length (!actions)) > 0 
                  then 
                      actions :=( String.sub !actions 0 ((String.length !actions) -1))
                  else 
                      (play_moves map player_list id actions bootMode)
                




let draw_Loose color = 
  let list = [(7,0);(8,0);(9,0);(10,0);(11,0);
              (11,1);(11,2);
              (7,4);(8,4);(9,4);(10,4);(11,4);
              (7,5);(11,5);
              (7,6);(8,6);(9,6);(10,6);(11,6);
              (7,8);(8,8);(9,8);(10,8);(11,8);
              (7,9);(11,9);
              (7,10);(8,10);(9,10);(10,10);(11,10);
              (7,12);(8,12);(9,12);(11,12); 
              (7,13);(9,13);(11,13);
              (7,14);(9,14);(10,14);(11,14);
              (7,16);(8,16);(9,16);(10,16);(11,16);
              (7,17);(7,18);(9,17);(9,18);(11,17);(11,18)] in
  
  fill_rects list color 19;
  (draw_rects list black 19)


let draw_Win color = 
  let list = [(7,3);(8,3);(9,3);(10,3);
              (11,4);
              (7,5);(8,5);(9,5);(10,5);
              (11,6);
              (7,7);(8,7);(9,7);(10,7);
              (7,9);(9,9);(10,9);(11,9);
              (7,11);(8,11);(9,11);(10,11);(11,11);
              (7,12);(8,12);(9,13);(10,14);(11,14);
              (7,15);(8,15);(9,15);(10,15);(11,15)
              ] in
  fill_rects list color 19;
  (draw_rects list black 19)