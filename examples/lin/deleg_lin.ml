open Mpst_monad
open Linocaml
open Calc_util

let mst = {role_index=Zero;
           role_label={make_obj=(fun v->object method role_Mst=v end);
                       call_obj=(fun o -> o#role_Mst)}}

(* let to_srv m =
 *   {obj_merge=(fun l r -> object method role_Srv=m.obj_merge l#role_Srv r#role_Srv end);
 *   obj_splitL=(fun lr -> (lr :> <>))} *)


type op = Add | Sub | Mul | Div
let calc () =
  fix (fun t ->
      choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let worker () =
  (srv --> mst) msg @@
  (mst --> srv) msg @@
  finish

let s = Linocaml.Zero
let t = Linocaml.Succ Linocaml.Zero

let tCli_monad () =
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

let calc_sh = create_shared calc [`role_Cli(); `role_Srv()]
let work_sh = create_shared worker [`role_Mst(); `role_Srv()]

let tSrvWorker i =
  print_endline "worker started";
  let rec loop () =
    let%lin #t = connect work_sh srv in
    let%lin #t = t <@ send (fun x->x#role_Mst#msg) i in
    let%lin `msg(#s, #t) = t <@ receive (fun x->x#role_Mst) in
    t <@ close >>= fun () ->
    tSrv_monad () >>= fun () ->
    loop ()
  in loop ()

let tMaster () =
  print_endline "master started";
  let rec loop () =
    let%lin #s = connect calc_sh srv in
    print_endline "master: client comes";
    let%lin #t = connect work_sh mst in
    let%lin `msg(i, #t) = t <@ receive (fun x->x#role_Srv) in
    Printf.printf "master: connecrted to a worker %d\n" i;
    let%lin #t = (s @* t) <@ deleg_send (fun x->x#role_Srv#msg) in
    t <@ close >>= loop
  in loop ()

let run f x =
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


let _ : Thread.t list =
  repeat 10 (fun i ->
      Thread.create (fun () -> run (fun () -> tSrvWorker i)) ())

let _ : Thread.t =
  Thread.create (fun () -> run tMaster) ()

let () =
  List.iter Thread.join @@
                    repeat 10 (fun i -> print_endline"here"; Thread.create (run (fun () ->
                                            Printf.printf "%d\n" i;
                                            let%lin #s = connect calc_sh cli in
                                            tCli_monad ())) ())
