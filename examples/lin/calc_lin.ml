open Mpst_monad
open Linocaml
module Util = Calc_util.Make(Linocaml_lin.EP)
open Util

type op = Add | Sub | Mul | Div
let calc =
    fix (fun t -> choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let sh = create_shared ~kinds:[`Untyped;`Untyped] calc

let s = Linocaml.Zero

let tCli_monad () =
  let%lin #s = connect sh cli in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Add, 20) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Sub, 45) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Mul, 10) in
  let%lin #s = s <@ send (fun x->x#role_Srv#result) () in
  let%lin `answer(ans, #s) = s <@ receive (fun x -> x#role_Srv) in
  s <@ close >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let rec tSrv_monad () =
  let%lin #s = accept sh srv in
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


let () =
  ignore (Thread.create (run' tSrv_monad) ());
  ignore (Thread.create (run' tCli_monad) ());
  ignore (Thread.create (run' tCli_monad) ());
  ignore (Thread.create (run' tCli_monad) ());
  ignore (Thread.create (run' tCli_monad) ());
  ignore (Thread.create (run' tCli_monad) ());
  ignore (Thread.create (run' tCli_monad) ());
  run' tCli_monad ();
  run' tCli_monad ();
  run' tCli_monad ();
  ()
