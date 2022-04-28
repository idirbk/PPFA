

let clear  = let _ =Sys.command("clear")in ()

(*  *)
let distance p1 p2 = 
  (abs  ((fst p1) - (fst p2))) + (abs  ((snd p1 )- (snd p2)))

let wait milli =
  let sec = milli /. 1000. in
  let tm1 = Unix.gettimeofday () in
  while Unix.gettimeofday () -. tm1 < sec do () done