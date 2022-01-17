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
type player={position:int ref*int ref;strength: int; life:int ; pa:int ref; pm:int; attack: atck };;

(* Global variables *)
let players_list = ref [];;




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


let init_players m = 
  Random.init (int_of_float (Unix.time()));
  for i= 0 to 5 do 
      let (x,y) = place_player m i in
        players_list := (i,{position=(ref x,ref y);strength=18;life=22;pa=ref 12;pm=9;attack={dmg=2;range=4;pa=12}})::(!players_list)
  done;;


(* Main (test) *)
let m = init_map  20 20;;
init_players m;;
print_map m;;
     


