open Concur_shims
open Mpst
open Calc_util

let (let*) = IO.bind

let finish = finish_with_multirole ~at:srv

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

let tCli (ec : 't) =
  let (_ : 't ty) = get_ty cli calc in
  let* ec = send_many ec#role_Srv#compute (fun i -> (Add, 20+i)) in
  let* ec = send_many ec#role_Srv#compute (const (Sub, 45)) in
  let* ec = send_many ec#role_Srv#compute (const (Mul, 10)) in
  let* ec = send_many ec#role_Srv#result (const ()) in
  let* `answer(ans, ec) = receive_many ec#role_Srv in
  let* () = close ec in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  List.iter (fun ans -> Printf.printf "Answer: %d\n" ans) ans;
  IO.return ()

let tSrv ew =
  let rec loop acc ew =
    let* var = receive ew#role_Cli in
    match var with
    | `compute((sym,num), ew) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) ew
    | `result((), ew) ->
      let* ew = send ew#role_Cli#answer acc in
      close ew
  in loop 0 ew

let (_ : unit IO.io) =
  let g = gen_mult [1;100] calc in
  let ec = get_ch cli g
  and ess = get_ch_list srv g
  in
  let ts = List.map (Thread.create tSrv) ess in
  ignore (Thread.create tCli ec);
  IO_list.iter Thread.join ts

