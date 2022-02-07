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
  
;;

let player_translation_send player =
  string_of_int player.strength ^  "|"^ string_of_int !(player.life) ^ "|"^ string_of_int !(player.pa) ^ "|"^ string_of_int !(player.pm) 
                            ^ "|"^ string_of_int player.attack.dmg ^ "|"^ string_of_int player.attack.range ^ "|"^ string_of_int player.attack.pa 
;;
