

let clear  = fun x -> Sys.command("clear");;

(*  *)
let distance p1 p2 = 
  (abs  ((fst p1) - (fst p2))) + (abs  ((snd p1 )- (snd p2)))
;;