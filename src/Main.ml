(*
   PPFA
   Boukari Idir 
   Berkennou Brahim
   Université Paris-Saclay
*)

(* Types definitions *)
type cell = Empty | Wall | Player of int;;
type map = {height : int ; width : int ; grid : cell array array};;
type atck = {dmg : int; range : int; pa : int};;
type player={position:int ref*int ref;strength: int; life:int ref ; pa:int ref; pm:int; attack: atck };;
type action = Atack of int * int | Move of int * int | DoNothing;;

let clear = function x -> Sys.command("clear")+x;; 

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
;;

let print_cell c =
  match c with
  | Empty -> Printf.printf " "
  | Wall  -> Printf.printf "#"
  | Player(p)-> print_int p
;;

let print_map map = 
    for i = 0 to map.height -1
    do 
      for j = 0  to map.width -1
      do
        print_cell map.grid.(i).(j);
      done;
      Printf.printf "\n";
    done;
;;


let rec place_player m id= 
  let y =  (Random.int (m.width -2))+1 in 
  let x =  (Random.int (m.height -2))+1 in 
   match m.grid.(x).(y) with 
     Empty->begin 
          m.grid.(x).(y)<- Player(id);
           (x,y)
          end
    |_-> place_player m id
  ;;



let init_players m  players_list= 
  Random.init (int_of_float (Unix.time()));
  for i= 0 to 5 do 
      let (x,y) = place_player m i in
        players_list := (i,{position=(ref x,ref y);strength=18;life= ref 22;pa=ref 12;pm=9;attack={dmg=22;range=4;pa=12}})::(!players_list)
  done;;


let rec rm_id l id = 
  match l with
  | [] -> []
  | (x,p)::ll when x == id-> ll
  | (x,p)::ll when x != id-> (x,p)::(rm_id ll id)
;;

 
    
let attack player map players_list x y =
  player.pa := !(player.pa) -1;
  match map.grid.(x).(y) with
  | Empty | Wall-> Printf.printf("BAD SHOT !!! :( ")
  | Player(id)  ->    
                      try
                        let target =  List.assoc id !players_list in
                        target.life :=  !(target.life) - player.attack.dmg;
                        
                        if !(target.life) <= 0 then 
                            (players_list := (rm_id !players_list id);
                            map.grid.(x).(y) <- Empty)
                        
                    with _ -> Printf.printf "ERROR!!!!!!";     
             
  ;;

let distance p1 p2 = 
      (abs  ((fst p1) - (fst p2))) + (abs  ((snd p1 )- (snd p2)))


let rec choose_coords pos range map=
    Printf.printf "\nline   ->";
    let l = read_int () in 
    Printf.printf "\ncolomn ->";
    let c = read_int () in 
    if (l > 0) && (c > 0) && (l < map.height -1) && (c < map.width -1) && distance pos (l,c) <= range
      then (l,c)
    else
      choose_coords pos range map
  ;;
let rec choose map  player = 
  Printf.printf "\nmake a choice  :         \n";
  Printf.printf "\n|1| ===> Move              ";
  Printf.printf "\n|2| ===> Atack             ";
  Printf.printf "\n|3| ===> pass your turn  \n";
  Printf.printf "\n Your Choice :: ";
  let ch = read_int () in  
  match ch with
  | 1 -> DoNothing
  | 2 -> begin
          let (x,y) = choose_coords (!(fst player.position),!(snd player.position))  4  map in
          Atack(x,y);
         end
  | 3 -> DoNothing
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
      | Move(x,y) -> Printf.printf "MOVE IT MOVE IT";
      | DoNothing -> Printf.printf "SKIP";
      
    
  with _ -> game_loop map players_list ((id+1) mod 6)
  
  
;;

  
let players_list = ref [];;
let m = init_map  5 5;;
init_players m players_list;;
print_map m;;

game_loop m players_list 0;;
     


