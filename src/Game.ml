

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





let place_obstacle m = 
  let y =  (Random.int (m.width -2))+1 in 
  let x =  (Random.int (m.height -2))+1 in 
   m.grid.(x).(y)<- Wall
   

 let init_obstacle m = 
  for i=0 to ((m.height*m.width)/10)
   do
    place_obstacle m
   done
   





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



  (* Printing the Board game in Console*)
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


(* Delete player from Players list *)
let rec rm_id l id = 
  match l with
  | [] -> []
  | e::ll when id == 0-> ll
  | e::ll when id != 0-> e::(rm_id ll (id-1))



let attack player map players_list x y =
  player.pa := !(player.pa) -1;
  match map.grid.(x).(y) with
  | Empty | Wall-> Printf.printf("BAD SHOT !!! :( ")
  | Player(id)  ->(  
                      
                        let target =  List.nth !players_list id in
                        target.life :=  !(target.life) - player.attack.dmg;
                        
                        if !(target.life) <= 0 then 
                            (players_list := (rm_id !players_list id);
                            map.grid.(x).(y) <- Empty)   
                  ) 



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


let position_change x0 y0 x1 y1 map player id = 
  print_map map;
  map.grid.(x0).(y0)<- Empty;
  map.grid.(x1).(y1)<- Player(id);
  (fst (player.position )):= x1;
  (snd (player.position )):= y1;
  Unix.sleepf 0.1;
  clear


let move id player map x y =
  match map.grid.(x).(y) with
     Empty->begin
              let way_list= (pcc (!(fst (player.position))) (!(snd (player.position))) x y map)  in
              List.iter(fun e -> position_change (!(fst (player.position))) (!(snd (player.position))) (fst e) (snd e) map player id ; player.pm:= !(player.pm) - 1) (List.rev way_list);
              
            end   
    |Wall | Player(_)-> Printf.printf"Vous ne pouvez vous déplacer dans une case occupée !!!\n"
    


let died players_list id : bool =
  try 
    List.assoc id !players_list; 
    false
  with _ -> true

let win team players_list : bool =
  if  team = 0 then 
    died players_list 1 && died players_list 3 && died players_list 5
  else
    died players_list 0 && died players_list 2 && died players_list 4



let print_player player_list =
  List.iter (fun e -> Printf.printf "{position=(ref %d,ref %d);strength=%d;life= ref %d;pa=ref %d;pm=ref %d;attack={dmg=%d;range=%d;pa=%d}}\n"
                                    !(fst e.position) !(snd e.position) e.strength !(e.life) !(e.pa) !(e.pm) (e.attack.dmg) (e.attack.range) (e.attack.pa) ) player_list