open Mpst
module Util = Calc_util.Dyn
open Util

let mst = {role_index=Zero;
           role_label={make_obj=(fun v->object method role_Mst=v end);
                       call_obj=(fun o -> o#role_Mst)}}


type op = Add | Sub | Mul | Div
let calc =
  fix (fun t ->
      choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let worker =
  (srv --> mst) msg @@
  (mst --> srv) (msg >: prot srv calc) @@
  finish

let tCli s =
  print_endline "client start";
  let s = send s#role_Srv#compute (Add, 20) in
  let s = send s#role_Srv#compute (Sub, 45) in
  let s = send s#role_Srv#compute (Mul, 10) in
  let s = send s#role_Srv#result () in
  let `answer(ans, s) = receive s#role_Srv in
  close s;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  flush stdout

let tSrv s =
  let rec loop acc s =
    match receive s#role_Cli with
    | `compute((sym,num), s) ->
       let op = match sym with
         | Add -> (+)   | Sub -> (-)
         | Mul -> ( * ) | Div -> (/)
       in loop (op acc num) s
    | `result(_, s) ->
       let s = send s#role_Cli#answer acc in
       close s
  in loop 0 s

let calc_sh = create_shared ~kinds:[`IPCProcess;`IPCProcess] calc
let work_sh = create_shared ~kinds:[`Local;`Local] worker

let tSrvWorker i =
  let rec loop () =
    let t = accept work_sh srv in
    let t = send t#role_Mst#msg i in
    let `msg(s, t) = receive t#role_Mst in
    close t;
    tSrv s;
    loop ()
  in loop ()

let tMaster () =
  let rec loop () =
    print_endline "master loop";
    let s = accept calc_sh srv in
    print_endline "master: client comes";
    let t = connect work_sh mst in
    let `msg(i, t) = receive t#role_Srv in
    Printf.printf "master: connecrted to a worker %d\n" i;
    let t = send t#role_Srv#msg s in
    close t;
    loop ()
  in loop ()

let repeat cnt f =
  let rec loop cnt acc =
    if cnt > 0 then begin
        let v = f cnt in
        loop (cnt-1) (v::acc)
      end else
      acc
  in
  loop cnt []

let _ : Thread.t list =
  repeat 10 (fun i ->
      Thread.create (fun () -> tSrvWorker i) ())

let _ : Thread.t =
  Thread.create tMaster ()

let () =
  List.iter Thread.join @@
    repeat 10 (fun i -> Thread.create (fun () ->
                                let s = connect calc_sh cli in
                                tCli s) ())
