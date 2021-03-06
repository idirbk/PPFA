

open Tools
(* Types definitions *)
type cell = Empty | Wall | Player of int
type map = {height : int ; width : int ; grid : cell array array}
type atck = {dmg : int; range : int; pa : int}
type player={position:int ref*int ref;strength: int; life:int ref ; pa:int ref; pm:int ref; attack: atck }
type action = Atack of int * int | Move of int * int | DoNothing



(*Players definitions*)
let invincible  = {position=(ref 5,ref 5);strength=50;life= ref 50;pa=ref 10;pm=ref 5;attack={dmg=10;range=5;pa=5}}
let predactor   = {position=(ref 0,ref 0);strength=30;life= ref 70;pa=ref 10;pm=ref 5;attack={dmg=2;range=5;pa=2}}
let predator    = {position=(ref 0,ref 0);strength=60;life=ref 40;pa=ref 6;pm=ref 11;attack={dmg=3;range=2;pa=2}}
let speeder     = {position=(ref 0,ref 0);strength=40;life=ref 60;pa=ref 6;pm=ref 11;attack={dmg=3;range=1;pa=2}}
let equil1      = {position=(ref 0,ref 0);strength=50;life=ref 50;pa=ref 6;pm=ref 9;attack={dmg=2;range=2;pa=2}}
let equil2      = {position=(ref 0,ref 0);strength=50;life=ref 50;pa=ref 9;pm=ref 6;attack={dmg=3;range=1;pa=3}}


(* INIT THE BOARD *)
let init_map h w = 
  let gr = Array.make_matrix h w Empty in 
  for i = 0 to w-1
    do 
      gr.(0).(i) <- Wall;
      gr.(h-1).(i) <- Wall;
    done;
    for i = 0 to h-1
    do 
      gr.(i).(0) <- Wall;
      gr.(i).(w-1) <- Wall;
    done;
    {height = h ; width = w ; grid = gr}




(* place obstacle in random places  *)
let place_obstacle m = 
  let y =  (Random.int (m.width -2))+1 in 
  let x =  (Random.int (m.height -2))+1 in 
   m.grid.(x).(y)<- Wall
   

 let init_obstacle m = 
  for i=0 to ((m.height*m.width)/10)
   do
    place_obstacle m
   done
   
(* Place the player in the game board *)
let rec place_player m id= 
  let y =  (Random.int (m.width -2))+1 in 
  let x =  (Random.int (m.height -2))+1 in 
   match m.grid.(x).(y) with 
     Empty->begin 
          m.grid.(x).(y)<- Player(id);
           (x,y)
          end
    |_-> place_player m id



let init_players m  players_list= 
  Random.init (int_of_float (Unix.time()));
  for i= 0 to 5 do 
      let (x,y) = place_player m i in
        players_list := (i,{position=(ref x,ref y);strength=18;life= ref 22;pa=ref 12;pm=ref 9;attack={dmg=22;range=4;pa=12}})::(!players_list)
  done



  (* Printing the game Board in Console*)
let print_cell c =
  match c with
  | Empty -> Printf.printf " "
  | Wall  -> Printf.printf "#"
  | Player(p)-> print_int p


let print_map map = 
    for i = 0 to map.height -1
    do 
      for j = 0  to map.width -1
      do
        print_cell map.grid.(i).(j);
      done;
      Printf.printf "\n%!";
    done

(* Print a list of players in console *)
let print_player player_list =
  List.iter (fun e -> Printf.printf "{position=(ref %d,ref %d);strength=%d;life= ref %d;pa=ref %d;pm=ref %d;attack={dmg=%d;range=%d;pa=%d}}\n"
                                    !(fst e.position) !(snd e.position) e.strength !(e.life) !(e.pa) !(e.pm) (e.attack.dmg) (e.attack.range) (e.attack.pa) ) player_list





(* choose the coords from terminal *)

let rec choose_coords pos range map=
    Printf.printf "\nline   ->";
    let l = read_int () in 
    Printf.printf "\ncolomn ->";
    let c = read_int () in 
    if (l > 0) && (c > 0) && (l < map.height -1) && (c < map.width -1) && distance pos (l,c) < range 
      then (l,c)
    else
      (choose_coords pos range map)
  



let copie_map map =
  let cp = Array.make_matrix map.height map.width 0 in
  for i = 0 to map.height-1
    do 
      for j = 0 to map.width-1
        do
          cp.(i).(j) <-
                      match map.grid.(i).(j) with
                      |Empty -> 0
                      |Wall | Player(_) -> -1 
        done;
    done;
  cp
  
  let copie_map2 map =
    let cp = Array.make_matrix map.height map.width 0 in
    for i = 0 to map.height-1
      do 
        for j = 0 to map.width-1
          do
            cp.(i).(j) <-
                        match map.grid.(i).(j) with
                        | Empty | Player(_) -> 0
                        | Wall -> -1
          done;
      done;
    cp


(* A* algorithme *)
  
let rec pcc_aux cmap v x0 y0 x1 y1 =
cmap.(x0).(y0) <- (v+1);
if   (x1 != x0) || (y1 != y0) then
  begin
    List.iter (function e  -> let nx = x0 + (fst e)in
                              let ny = y0 + (snd e)in
                              if cmap.(nx).(ny) = 0 || cmap.(nx).(ny) > (v+2)
                                then 
                                  pcc_aux cmap (v+1) nx ny x1 y1
                              ) [(1,0);(0,1);(-1,0);(0,-1)]

  end


let pcc  x0 y0 x1 y1 map=
  let cpy = copie_map map in
  pcc_aux cpy 1 x0 y0 x1 y1;
  let rec get_way x0 y0 x1 y1 cpy=
    if x1 = x0 && y1 = y0 then
      []
    else
        begin
          List.fold_left (fun acc e -> match acc with 
                                      |[] -> begin
                                                let nx = x1 + (fst e)in
                                                let ny = y1 + (snd e)in
                                                if cpy.(nx).(ny) = (cpy.(x1).(y1) -1) then 
                                                  (nx,ny)::(get_way x0 y0 nx ny cpy)
                                                else
                                                  []
                                              end 
                                      | _ -> acc
                                      ) [] [(1,0);(0,1);(-1,0);(0,-1)]
        end
  in 
  let lresult = (get_way x0 y0 x1 y1 cpy) in 
  match lresult with 
    []-> lresult
  |e::llres->(x1,y1)::lresult

(* change player's position from x0 y0 to x1 y1 *)
let position_change x0 y0 x1 y1 map player id = 
  print_map map;
  map.grid.(x0).(y0)<- Empty;
  map.grid.(x1).(y1)<- Player(id);
  (fst (player.position )):= x1;
  (snd (player.position )):= y1;
  Unix.sleepf 0.1;
  clear


(* player move to the coords x y *)
let move id player map x y =
  match map.grid.(x).(y) with
     Empty->begin
              let way_list= (pcc (!(fst (player.position))) (!(snd (player.position))) x y map)  in
              List.iter(fun e -> position_change (!(fst (player.position))) (!(snd (player.position))) (fst e) (snd e) map player id ; player.pm:= !(player.pm) - 1) (List.rev way_list);
              
            end   
    |Wall | Player(_)-> Printf.printf"Vous ne pouvez vous d??placer dans une case occup??e !!!\n"
    
(* player attack the coords l c *)
let attack map player_list id l c =
  let player = List.nth player_list id in
  (player.pa) := !(player.pa) - player.attack.pa ;
  match map.grid.(l).(c) with
  |Empty|Wall -> ()
  |Player(x) -> let target = List.nth player_list x in
                target.life := max (!(target.life) - player.attack.dmg) 0;
                if!(target.life ) = 0 then map.grid.(l).(c) <- Empty;
                ()

let f strength dmg : int = 
  dmg+(strength/10)

let g dmg pa range : bool = 
  pa-(range+dmg) == 0


(* returns true if the game is over and false else *)
let game_over player_list = 
  if List.length player_list != 6 then 
    false
  else
    let (m1,m2) = ( [(List.nth player_list 1); (List.nth player_list 3); (List.nth player_list 5)] ,[(List.nth player_list 0); (List.nth player_list 2); (List.nth player_list 4)]) in
    match (List.filter (fun e -> (!(e.life) > 0)) m1),(List.filter (fun e -> !(e.life) > 0) m2) with
    |[],_ | _,[] -> true
    |_,_ -> false







let rec process map x y w h v range =
  if(v > range) || x*y <= 0 || x >= w || y >= h || (map.(y).(x) != 0 && map.(y).(x) != -2)then
    []
  else
    let var = if(map.(y).(x) == -2) then [] else  [(y,x)] in
    if (map.(y).(x) != -2) then (if(map.(y).(x) > v) then map.(y).(x) <- v) else  map.(y).(x) <- -3;
    let res = var@(List.fold_left (fun acc e -> 
                                    let nx = x + (fst e)in
                                    let ny = y + (snd e)in
                                    acc@process map nx ny w h (v+1) range
                          ) [] [(1,0);(0,1);(-1,0);(0,-1)]) in
    res


(* get the possible moves for a player *)
let get_possible_moves map player range =
  let m = copie_map map in 
  let l = !(fst (player.position)) in
  let c = !(snd (player.position)) in
  m.(l).(c) <- -2;
  (process m c l (map.width) (map.height) 0 range)

(* get the possible attacks for a player *)
let get_possible_attacks map player range =
  let m = copie_map2 map in 
  let l = !(fst (player.position)) in
  let c = !(snd (player.position)) in
  m.(l).(c) <- -2;
  (process m c l (map.width) (map.height) 0 range)


