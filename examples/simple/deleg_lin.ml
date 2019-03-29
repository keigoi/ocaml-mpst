open Mpst_simple
open Mpst_simple.Lin
open Mpst_simple.LinMonad
open Mpst_simple.LinMonad.Op

let cli = {lens=Fst;
           label={make_obj=(fun v->object method role_Cli=v end);
                 make_var=(fun v->(`role_Cli(v):[`role_Cli of _]))}}
let srv = {lens=Next Fst;
           label={make_obj=(fun v->object method role_Srv=v end);
                 make_var=(fun v->(`role_Srv(v):[`role_Srv of _]))}}
let mst = {lens=Next (Next Fst);
           label={make_obj=(fun v->object method role_Mst=v end);
                 make_var=(fun v->(`role_Mst(v):[`role_Mst of _]))}}


let compute = {make_obj=(fun v-> object method compute=v end); make_var=(fun v -> `compute(v))}
let result = {make_obj=(fun v-> object method result=v end); make_var=(fun v -> `result(v))}
let answer = {make_obj=(fun v-> object method answer=v end); make_var=(fun v -> `answer(v))}
let compute_or_result =
     {obj_merge=(fun l r -> object method compute=l#compute method result=r#result end)}
let to_srv m =
  {obj_merge=(fun l r -> object method role_Srv=m.obj_merge l#role_Srv r#role_Srv end)}

let finish = one @@ one @@ one @@ nil

type op = Add | Sub | Mul | Div
let calc () =
  let rec g =
    lazy (choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 goto g)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))
  in Lazy.force g

let worker () =
  (srv --> mst) msg @@
  (mst --> srv) msg @@
  finish

let _0 = Mpst_simple.LinMonad.Fst
let _1 = Mpst_simple.LinMonad.Next Mpst_simple.LinMonad.Fst

let tCli_monad () =
  let%lin #_0 = send (fun x->x#role_Srv#compute) (Add, 20) _0 in
  let%lin #_0 = send (fun x->x#role_Srv#compute) (Sub, 45) _0 in
  let%lin #_0 = send (fun x->x#role_Srv#compute) (Mul, 10) _0 in
  let%lin #_0 = send (fun x->x#role_Srv#result) () _0 in
  let%lin `role_Srv(`answer(ans, #_0)) = receive _0 in
  close _0 >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv_monad () =
  let rec loop acc =
    match%lin receive _0 with
    | `role_Cli(`compute({data=(sym,num)}, #_0)) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `role_Cli(`result(_, #_0)) ->
      let%lin #_0 = send (fun x->x#role_Cli#answer) acc _0 in
      close _0
  in loop 0

let calc_sh = create_shared calc [`role_Cli(); `role_Srv()]
let work_sh = create_shared worker [`role_Mst(); `role_Srv()]

let tSrvWorker i =
  print_endline "worker started";
  let rec loop () =
    let%lin #_1 = connect work_sh srv in
    let%lin #_1 = send (fun x->x#role_Mst#msg) i _1 in
    let%lin `role_Mst(`msg(#_0, #_1)) = receive _1 in
    close _1 >>= fun () ->
    tSrv_monad () >>= fun () ->
    loop ()
  in loop ()

let tMaster () =
  print_endline "master started";
  let rec loop () =
    let%lin #_0 = connect calc_sh srv in
    print_endline "master: client comes";
    let%lin #_1 = connect work_sh mst in
    let%lin `role_Srv(`msg({data=i}, #_1)) = receive _1 in
    Printf.printf "master: connecrted to a worker %d\n" i;
    let%lin #_1 = deleg_send (fun x->x#role_Srv#msg) _0 _1 in
    close _1 >>= loop
  in loop ()

let run f x =
  LinMonad.run begin
      LinMonad.expand >>= fun () ->
      LinMonad.expand >>= fun () ->
      f x >>= fun () ->
      LinMonad.shrink >>= fun () ->
      LinMonad.shrink
  end

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
                                            let%lin #_0 = connect calc_sh cli in
                                            tCli_monad ())) ())
