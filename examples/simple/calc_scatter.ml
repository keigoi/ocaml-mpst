open Mpst
open Calc_util

type op = Add | Sub | Mul | Div

let calc =
  fix (fun t ->
      (choice_at cli (to_srv compute_or_result)
         (cli, (scatter cli srv) compute @@
               t)
         (cli, (scatter cli srv) result @@
               (gather srv cli) answer @@
               finish)))

let const x _ = x
let tCli ec =
  let ec = sendmany ec#role_Srv#compute (fun i -> (Add, 20+i)) in
  let ec = sendmany ec#role_Srv#compute (const (Sub, 45)) in
  let ec = sendmany ec#role_Srv#compute (const (Mul, 10)) in
  let ec = sendmany ec#role_Srv#result (const ()) in
  let `answer(ans, ec) = receive ec#role_Srv in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  List.iter (fun ans -> Printf.printf "Answer: %d\n" ans) ans

let tSrv ew =
  let rec loop acc ew =
    match receive ew#role_Cli with
    | `compute((sym,num), ew) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) ew
    | `result((), ew) ->
      let ew = send (ew#role_Cli#answer) acc in
      close ew
  in loop 0 ew

let () =
  let g = gen_mult [1;100] calc in
  let ec = get_ep cli g
  and ess = get_ep_list srv g
  in List.iter Thread.join
       ([Thread.create tCli ec] @ (List.map (fun es -> Thread.create tSrv es) ess))
