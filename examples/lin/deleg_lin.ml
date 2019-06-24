open Mpst_monad
open Linocaml
module Util = Calc_util.Make(Mpst_monad.Nocheck.Nodyncheck)
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
  (mst --> srv) msg @@
  finish

let s = Linocaml.Zero
let t = Linocaml.(Succ Zero)

let tCli_monad () =
  print_endline "client start";
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Add, 20) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Sub, 45) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Mul, 10) in
  let%lin #s = s <@ send (fun x->x#role_Srv#result) () in
  let%lin `answer(ans, #s) = s <@ receive (fun x->x#role_Srv) in
  s <@ close >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv_monad () =
  let rec loop acc =
    match%lin s <@ receive (fun x->x#role_Cli) with
    | `compute({data=(sym,num)}, #s) ->
       let op = match sym with
         | Add -> (+)   | Sub -> (-)
         | Mul -> ( * ) | Div -> (/)
       in loop (op acc num)
    | `result(_, #s) ->
       let%lin #s = s <@ send (fun x->x#role_Cli#answer) acc in
       s <@ close
  in loop 0

let calc_sh = create_shared ~kinds:[`IPCProcess;`IPCProcess] calc
let work_sh = create_shared ~kinds:[`Untyped;`Untyped] worker

let tSrvWorker i =
  let rec loop () =
    print_endline "worker loop";
    let%lin #t = accept work_sh srv in
    let%lin #t = t <@ send (fun x->x#role_Mst#msg) i in
    let%lin `msg(#s, #t) = t <@ receive (fun x->x#role_Mst) in
    t <@ close >>= fun () ->
    tSrv_monad () >>= fun () ->
    loop ()
  in loop ()

let tMaster () =
  let rec loop () =
    print_endline "master loop";
    let%lin #s = accept calc_sh srv in
    print_endline "master: client comes";
    let%lin #t = connect work_sh mst in
    let%lin `msg(i, #t) = t <@ receive (fun x->x#role_Srv) in
    Printf.printf "master: connecrted to a worker %d\n" i;
    let%lin #t = (t @* s) <@ deleg_send (fun x->x#role_Srv#msg) in
    t <@ close >>= loop
  in loop ()

let run f x : unit =
  Linocaml.run' f x

let repeat cnt f =
  let rec loop cnt acc =
    if cnt > 0 then begin
        let v = f cnt in
        loop (cnt-1) (v::acc)
      end else
      acc
  in
  loop cnt []

let rec tSrv_monad () =
  let%lin #s = accept calc_sh srv in
  let rec loop acc =
    match%lin s <@ receive (fun x->x#role_Cli) with
    | `compute({data=(sym,num)}, #s) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `result({data=()}, #s) ->
      let%lin #s = s <@ send (fun x->x#role_Cli#answer) acc in
      s <@ close >>= fun () ->
      tSrv_monad ()
  in loop 0


let _ : Thread.t list =
  repeat 10 (fun i ->
      Thread.create (run (fun () -> tSrvWorker i)) ())

let _ : Thread.t =
  Thread.create (run tMaster) ()

let () =
  List.iter Thread.join @@
    repeat 10 (fun i -> Thread.create (fun () ->
                            run (fun () ->
                                let%lin #s = connect calc_sh cli in
                                tCli_monad ()) ()) ())
