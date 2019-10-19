open Ast

let a = "a"
let b = "b"
let c = "c"

let g =
  (a --> b) "msg" finish


let rec nring n last from to_ =
  let (!!) n = "r" ^ string_of_int n in
  if n = 0 then
    (!!from --> !!last) "msg" finish
  else
    (!!from --> !!to_) "msg" (nring (n-1) last to_ (to_+1))
  
let () =
  print_global scribble g;
  print_global ocamlmpst (nring 10 0 0 1);
  ()
