open Game

(* first sense network translation *)

let map_translation_send map = 
  let s_res =ref "" in 
  for i = 0 to map.height -1
    do 
      for j = 0  to map.width -1
        do
          s_res := !s_res ^ match map.grid.(i).(j) with 
                                   Empty -> " "
                                  |Wall -> "#"
                                  |Player(id) ->string_of_int id 
        done;
        s_res := !s_res ^ "|";
    done;
    String.sub !s_res 0 ((String.length !s_res)-1) 
  

let player_translation_send player =
  string_of_int player.strength ^"|"^ string_of_int !(player.life) ^ "|"^ string_of_int !(player.pa) ^ "|"^ string_of_int !(player.pm) ^ "|"^string_of_int(!(fst player.position))^"*"^string_of_int(!(snd player.position)) 
                            ^ "|"^ string_of_int player.attack.dmg ^ "|" ^ string_of_int player.attack.range ^ "|"^ string_of_int player.attack.pa 



(* second sense network translation *)


let player_translation_receive player_string = 
  let player_string_ref = ref player_string in 
  let val_list =ref [] in 
  let i= ref 0 in 
  let nb=ref "" in 
  while !i <((String.length player_string)) do
    begin
      while ((!i <(String.length player_string)) && (String.get (!player_string_ref) !i) != '|'&& (String.get (!player_string_ref) !i) != '*')  do
      
        begin
          nb := !nb ^ (String.sub !player_string_ref !i 1);
          i := !i+1;
        end
      
      done;
      val_list:= !val_list@[int_of_string !nb];
      nb:="";
      i:=!i+1;   
    end
  done;
  
  {position=(ref (List.nth !val_list 4),ref (List.nth !val_list 5));strength=List.nth !val_list 0 ;life=ref (List.nth !val_list 1);pa=ref (List.nth !val_list 2) ;pm=ref(List.nth !val_list 3);attack={dmg=List.nth !val_list 6;range=List.nth !val_list 7;pa=List.nth !val_list 8}}



let height_count map_string  = 
  let cpt= ref 0 in 
  for i = 0 to (String.length map_string)-1 do 
    if(map_string.[i]='|') then
      cpt:=!cpt+1;
  done;
  !cpt



let width_count map_string = 
  let cpt = ref 0 in 
  let i = ref 0 in
  while map_string.[!i]!='|' do
    cpt:=!cpt + 1;
    i := !i +1;
  done;
  !cpt





let map_translation_receive map_string  = 
  let h = height_count map_string in 
  let w = width_count map_string in 
  let gr = Array.make_matrix (h+1) w Empty in
  let j = ref 0 in 
  let k = ref 0 in
  for i=0 to (String.length map_string)-1 do 
    if(map_string.[i]='|') then 
      begin
        j := !j +1;
        k:=0
      end
    else
      begin
        match map_string.[i] with 
            '#'->(gr.(!j).(!k) <- Wall);k := !k +1;
            |' '->(gr.(!j).(!k) <- Empty);k := !k +1;
            | _ ->gr.(!j).(!k) <-Player(int_of_string (String.sub map_string i 1));k := !k +1;

      end
  done; 
  {height = h+1 ; width = w ; grid = gr}

    

let install_client ip  =
     let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string ip,6566) in 
     let domain = Unix.domain_of_sockaddr sockaddr in
     let sock = Unix.socket domain Unix.SOCK_STREAM 0 in 
     Unix.connect sock sockaddr;
     (Unix.in_channel_of_descr sock,Unix.out_channel_of_descr sock)
     
let send_pseudo oc pseudo = 
  output_string oc (pseudo ^"\n")

let send_perso oc liste_perso =
  List.iter (fun e -> let str = player_translation_send e in  
                      output_string oc (str^"\n") ;flush oc) liste_perso

let send_map oc map =
  let str= map_translation_send map in
  output_string oc (str^"n")

let send_action oc action =
  output_string oc ((!action)^"\n")

let read_num ic=
  int_of_string (input_line ic)

let read_perso ic =
    List.map (fun e ->player_translation_receive e) (String.split_on_char ';' (input_line ic))  

let read_map ic = 
  (map_translation_receive (input_line ic))


  




