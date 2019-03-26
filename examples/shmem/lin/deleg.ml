open Mpst_shmem.Lin
open Util
open Global
open Session
open LinMonad
open LinMonad.Op

let cli : ([`Cli],_,_,_,_) role = {role=`Cli; lens=Fst}
let srv : ([`Srv],_,_,_,_) role = {role=`Srv; lens=Next Fst}
let mst : ([`Mst],_,_,_,_) role = {role=`Mst; lens=Fst}

let compute = {select_label=(fun f-> object method compute v=f v end); offer_label=(fun (v,c) -> `compute(v,c))}
let result = {select_label=(fun f-> object method result v=f v end); offer_label=(fun (v,c) -> `result(v,c))}
let answer = {select_label=(fun f-> object method answer v=f v end); offer_label=(fun (v,c) -> `answer(v,c))}
let deleg = {select_label=(fun f-> object method deleg v=f v end); offer_label=(fun (v,c) -> `deleg(v,c))}
let compute_or_result =
     {label_merge=(fun l r -> object method compute=l#compute method result=r#result end)}

let finish = one @@ one @@ nil

type op = Add | Sub | Mul | Div
let calc () =
  let rec g =
    lazy (choice_at cli compute_or_result
           (cli, (cli --> srv) compute @@
                 loop g)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))
  in Lazy.force g

let worker () =
  (srv --> mst) msg @@
  (mst --> srv) deleg @@
  finish

let _0 = Mpst_base.LinMonad.Fst
let _1 = Mpst_base.LinMonad.Next Mpst_base.LinMonad.Fst

let tCli () =
  let%lin #_0 = send `Srv (fun x->x#compute) (Add, 20) _0 in
  let%lin #_0 = send `Srv (fun x->x#compute) (Sub, 45) _0 in
  let%lin #_0 = send `Srv (fun x->x#compute) (Mul, 10) _0 in
  let%lin #_0 = send `Srv (fun x->x#result) () _0 in
  let%lin `answer(ans, #_0) = receive `Srv _0 in
  close _0 >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv () =
  let rec loop acc =
    match%lin receive `Cli _0 with
    | `compute({data=(sym,num)}, #_0) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `result({data=()}, #_0) ->
      let%lin #_0 = send `Cli (fun x->x#answer) acc _0 in
      close _0
  in loop 0

let calc_sh = create_global calc [`Cli; `Srv]
let work_sh = create_global worker [`Mst; `Srv]

let tSrvWorker i =
  print_endline "worker started";
  let rec loop () =
    let%lin #_1 = connect work_sh srv in
    let%lin #_1 = unone _1 in
    let%lin #_1 = send `Mst (fun x->x#msg) i _1 in
    let%lin `deleg(#_0, #_1) = receive `Mst _1 in
    close _1 >>= fun () ->
    tSrv () >>= fun () ->
    loop ()
  in loop ()

let tMaster () =
  print_endline "master started";
  let rec loop () =
    let%lin #_0 = connect calc_sh srv in
    let%lin #_0 = unone _0 in
    print_endline "master: client comes";
    let%lin #_1 = connect work_sh mst in
    let%lin #_1 = unone _1 in
    let%lin `msg({data=i}, #_1) = receive `Srv _1 in
    Printf.printf "master: connecrted to a worker %d\n" i;
    let%lin #_1 = deleg_send `Srv (fun x->x#deleg) _0 _1 in
    close _1 >>= loop
  in loop ()
   
let run f =
  LinMonad.run begin
      LinMonad.expand >>= fun () ->
      LinMonad.expand >>= fun () ->
      f () >>= fun () ->
      LinMonad.shrink >>= fun () ->
      LinMonad.shrink
  end

let rec repeat cnt f =
  if cnt > 0 then begin
      let open Lwt in
      f cnt >>= fun () ->
      repeat (cnt-1) f
    end else
    Lwt.return ()

let _ : unit Lwt.t =
  repeat 10 (fun i ->
      Lwt.async (fun () -> run (fun () -> tSrvWorker i));
      Lwt.return ())

let _ : unit =
  Lwt.async (fun () -> run tMaster)
  
let () =
  Lwt_main.run (Lwt.join [
                    repeat 10 (fun i -> run (fun () ->
                                            Printf.printf "%d\n" i;
                                            let%lin #_0 = connect calc_sh cli in
                                            let%lin #_0 = unone _0 in
                                            tCli ()))
                  ])

