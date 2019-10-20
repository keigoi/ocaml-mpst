open Codegen
open Bench_util

let a = "a"
let b = "b"
let c = "c"


let rec nring n last from to_ =
  let (!!) n = "r" ^ string_of_int n in
  if n = 0 then
    (!!from --> !!last) "msg" finish
  else
    (!!from --> !!to_) "msg" (nring (n-1) last to_ (to_+1))


let rec countingactor from to_ base n =
  let act = (from --> to_) (base^string_of_int n) finish in
  if n = 0 then
    act
  else
    choice_at from act (countingactor from to_ base (n-1))
    
let num step cnt =
  List.init cnt (fun i -> (i+1) * step)
  
let () =
  (* print_global (new ocamlmpst) (nring 10 0 0 1);
   * print_global (new ocamlmpst) (countingactor "a" "b" "msg" 10); *)
  let param = num 1 1 @ num 10 1 @ num 50 1 @ num 100 1 in
  print_endline (Gentest.gentests param (fun n -> nring n 0 0 1));
  print_endline (Gentest.gentests param (fun n -> countingactor "a" "b" "msg" n));
  ()
