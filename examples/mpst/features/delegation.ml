open Concur_shims
open Mpst
open Mpst.Types
open Mpst.Util
open Calc_util

let (let*) = IO.bind

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
  (mst --> srv) (msg >: get_ty srv calc) @@
  finish

let tCli (s:'t) =
  let (_ : 't ty) = get_ty cli calc in
  print_endline "client start";
  let* s = send s#role_Srv#compute (Add, 20) in
  let* s = send s#role_Srv#compute (Sub, 45) in
  let* s = send s#role_Srv#compute (Mul, 10) in
  let* s = send s#role_Srv#result () in
  let* `answer(ans, s) = receive s#role_Srv in
  let* () = close s in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  flush stdout;
  IO.return ()

let tSrv s =
  let rec loop acc s =
    let* var = receive s#role_Cli in
    match var with
    | `compute((sym,num), s) ->
       let op = match sym with
         | Add -> (+)   | Sub -> (-)
         | Mul -> ( * ) | Div -> (/)
       in loop (op acc num) s
    | `result(_, s) ->
       let* s = send s#role_Cli#answer acc in
       close s
  in loop 0 s

let calc_sh = create_shared ~kinds:[`Local,0;`Local,0] calc
let work_sh = create_shared ~kinds:[`Local,0;`Local,0] worker

let tSrvWorker i =
  let rec loop () =
    let* t = accept work_sh srv in
    let* t = send t#role_Mst#msg i in
    let* `msg(s, t) = receive t#role_Mst in
    let* () = close t in
    let* () = tSrv s in
    loop ()
  in loop ()

let tMaster () =
  let rec loop () =
    print_endline "master loop";
    let* s = accept calc_sh srv in
    print_endline "master: client comes";
    let* t = connect work_sh mst in
    let* `msg(i, t) = receive t#role_Srv in
    Printf.printf "master: connecrted to a worker %d\n" i;
    let* t = send t#role_Srv#msg s in
    let* () = close t in
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
  IO.main_run @@ IO_list.iter Thread.join @@
    repeat 10 (fun _ -> Thread.create (fun () ->
                                let* s = connect calc_sh cli in
                                tCli s) ())
