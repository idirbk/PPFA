open Game
open Tools



(* Get the possible targets for  the player in line l and column c *)
let scan_for_targets player_list id dmax l c =
    let enemies = match (id mod 2) with
                  |0 ->[(List.nth player_list 1); (List.nth player_list 3); (List.nth player_list 5)]
                  |_ ->[(List.nth player_list 0); (List.nth player_list 2); (List.nth player_list 4)]
              in
    List.fold_left (fun acc e -> if (distance (l,c) ((!(fst (e.position))),(!(snd (e.position)))) ) <= dmax  && (!(e.life) > 0)
                                  then e::acc else acc) [] enemies


(* Sum two matrices *)
let sum_matrice m1 m2 h w =
  for l=0 to h
  do
    for c=0 to w 
    do
      m1.(l).(c) <- m1.(l).(c) + m2.(l).(c);
    done 
  done 


(* return an estimate of the danger present on the map in one turn *)
let risk_matrix map player_list player id=
    let enemies = match (id mod 2) with
                  |0 ->[(List.nth player_list 1); (List.nth player_list 3); (List.nth player_list 5)]
                  |_ ->[(List.nth player_list 0); (List.nth player_list 2); (List.nth player_list 4)]
                in
    
    let maps = List.map (fun p -> let m  = copie_map2 map in
                                  let atk = get_possible_attacks map p (min (p.attack.range + !(p.pm)) 9 ) in
                                  List.iter (fun e -> m.(fst e).(snd e) <- p.attack.dmg;) atk;
                                  m
    ) enemies in
    (List.fold_left (fun acc e ->sum_matrice acc e (map.height-1) (map.width-1); acc) (copie_map2 map) maps)



(*   
  returns the best move to play with the following strategy:
    - if i can attack then i attack else i move 
    - if i have attack point i move to targets else i move to safe zone
*)


let a_fuit = ref false;; 

let sum_lifes_enemies player_list id =
  let enemies = match (id mod 2) with
    |0 ->[(List.nth player_list 1); (List.nth player_list 3); (List.nth player_list 5)]
    |_ ->[(List.nth player_list 0); (List.nth player_list 2); (List.nth player_list 4)]
  in
  (List.fold_left (fun acc e -> acc + !(e.life)) 0 enemies)
  

let bestshot map player_list id= 
  if (sum_lifes_enemies player_list id) > 0 && not(!a_fuit) then 
    let player = List.nth player_list id in
    let psa = scan_for_targets player_list id !(player.pa) !(fst player.position) !(snd player.position) in
    if !(player.pa) > 0 && (List.length psa)  > 0 then
      let atk  = (List.hd  psa).position in
      Atack(!(fst atk),!(snd atk))
    else
      if !(player.pm) > 0 then 
        if !(player.pa) > 0 then
          let lmv = get_possible_moves map player !(player.pm) in
          let listMoves = List.filter (fun e -> (List.length (scan_for_targets player_list id !(player.pa) (fst e) (snd e)))> 0) lmv in
          let mv = if (List.length listMoves) > 0 then 
                          let index = Random.int (List.length listMoves) in
                          (List.nth listMoves index)
                        else
                          let index = Random.int (List.length lmv) in
                          (List.nth lmv index) 
                        in
          Move((fst mv), (snd mv))
      else
          let () = a_fuit := true in
          let m = risk_matrix map player_list player id in
          let lmv = get_possible_moves map player !(player.pm) in
          let listMoves = List.sort (fun e1 e2  -> m.(fst e2).(snd e2) - m.(fst e1).(snd e1)) lmv in
          let mv = List.hd listMoves in
          Move((fst mv), (snd mv))
      else
        DoNothing
  else 
      let () = a_fuit := false in
      DoNothing