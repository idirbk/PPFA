

let clear  = let _ =Sys.command("clear")in ()

(*  *)
let distance p1 p2 = 
  (abs  ((fst p1) - (fst p2))) + (abs  ((snd p1 )- (snd p2)))
