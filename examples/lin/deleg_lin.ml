open Concur_shims
open Mpst_lin
open Mpst_lin.Util
open Mpst_lin.LinocamlStyle

open Linocaml
open Calc_util

let (let*) = IO.bind
let (let/) = Linocaml.bind

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

let s = Linocaml.Zero
let t = Linocaml.(Succ Zero)

let tCli_monad () =
  print_endline "client start";
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Add, 20) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Sub, 45) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Mul, 10) in
  let%lin #s = s <@ send (fun x->x#role_Srv#result) () in
  let%lin `answer(ans, #s) = s <@ receive (fun x->x#role_Srv) in
  let/ () = s <@ close in
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

let calc_sh = create_shared ~kinds:[`Local,0;`Local,0] calc
let work_sh = create_shared ~kinds:[`Local,0;`Local,0] worker

let tSrvWorker i =
  let rec loop () =
    print_endline "worker loop";
    let%lin #t = accept work_sh srv in
    let%lin #t = t <@ send (fun x->x#role_Mst#msg) i in
    let%lin `msg(#s, #t) = t <@ receive (fun x->x#role_Mst) in
    let/ () = t <@ close in
    let/ () = tSrv_monad () in
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
    let/ () = t <@ close in
    loop ()
  in loop ()

let run f x : unit IO.io =
  Linocaml.run f x

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
      Thread.create (run (fun () -> tSrvWorker i)) ())

let _ : Thread.t =
  Thread.create (run tMaster) ()

let (_:unit IO.io) =
  IO_list.iter Thread.join @@
    repeat 10 (fun _ -> Thread.create (fun () ->
                            run (fun () ->
                                let%lin #s = connect calc_sh cli in
                                tCli_monad ()) ()) ())
