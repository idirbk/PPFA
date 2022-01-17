
type cell = Empty | Wall | Player of int;;
type map = {height : int ; width : int ; grid : cell array array};;
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
  | Player(x)-> print_int x
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

let m = init_map  20 30;;
print_map m;;
     


