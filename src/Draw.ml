open Graphics
open Game
open Network
open Tools


let size = 20
let gray = (rgb 100 100 100)

let fill_rects list color h =
  set_color color;
  List.iter (fun x -> fill_rect ((snd x)*size) ( (h-(fst x))*size) size size) list;
  set_color white;
  List.iter (fun x -> draw_rect ((snd x)*size) ( (h-(fst x))*size) size size) list



let draw_map map=
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
                    end
      |Player(id)-> begin
                      if id mod 2 = 0 then set_color red else  set_color blue;
                      fill_circle (j*size + size/2) ((var-i)*size + size/2) (size/2);

                    end
    done
  done

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

let init_window ()= 
  open_graph " 400x400" ;
  set_window_title "Street Fighter" ;
  set_color (rgb 100 100 100);
  fill_rect 0 0 400 400




let rec choose_move () = 
  let e = wait_next_event [Key_pressed] in
  match e.key with
    |'a'-> Atack(0,0)
    |'m'-> Move(0,0)
    |'s'-> DoNothing
    | _ -> choose_move()
  

let rec get_coords h list= 
  let e = wait_next_event [Button_down] in
  let x =(e.mouse_x/size) in
  let y =(h-e.mouse_y/size) in
  if (List.exists (fun e -> (fst e) = x && (snd e) = y) list) then
    y,x
  else
    get_coords h list

  
  
let rec  play_moves map player_list id actions=
  Printf.printf "%s\n%!" (!actions);
  let player = (List.nth player_list id) in
  match choose_move() with
    |Atack(_,_) -> if !(player.pm) > 0 then
                    begin
                      let list =get_possible_moves map player (!(player.pa)) in 
                      fill_rects list magenta (map.height-1);
                      let (l,c) = get_coords (map.height-1) list in
                      fill_rects list (rgb 100 100 100) (map.height-1);
                      add_shot actions (Atack(l,c));
                      
                      draw_map map;
                    end;
    |Move(_,_)  -> if !(player.pm) > 0 then
                    begin
                      let list =get_possible_moves map player (!(player.pm)) in 
                      fill_rects list blue (map.height-1);
                      let (l,c) = get_coords (map.height-1) list in
                      fill_rects list (rgb 100 100 100) (map.height-1);
                      add_shot actions (Move(l,c));
                      display_move l c player_list id map;
                      draw_map map;
                    end;
                    (play_moves map player_list id actions)
    |DoNothing  -> ()

