open Concur_shims
open Mpst_lin
open Mpst_lin.LinocamlStyle
open Linocaml
open Calc_util

let (let/) = Linocaml.bind
let (let*) = IO.bind

let s = Linocaml.Zero

type op = Add | Sub | Mul | Div
let calc =
    fix (fun t -> choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let sh = create_shared ~kinds:[`Local,0;`Local,0] calc

let tCli_monad () =
  let/ ans =
    let%lin #s = connect sh cli in
    let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Add, 20) in
    let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Sub, 45) in
    let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Mul, 10) in
    let%lin #s = s <@ send (fun x->x#role_Srv#result) () in
    let%lin `answer(ans, #s) = s <@ receive (fun x -> x#role_Srv) in
    let/ () = s <@ close in
    return ans
  in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let rec tSrv_monad () =
  let%lin #s = accept sh srv in
  let rec loop acc =
    let/ () =
      match%lin s <@ receive (fun x->x#role_Cli) with
      | `compute({data=(sym,num)}, #s) ->
        let op = match sym with
          | Add -> (+)   | Sub -> (-)
          | Mul -> ( * ) | Div -> (/)
        in loop (op acc num)
      | `result({data=()}, #s) ->
        let%lin #s = s <@ send (fun x->x#role_Cli#answer) acc in
        s <@ close
    in
    tSrv_monad ()
  in loop 0


let (_ : unit IO.io) =
  ignore (Thread.create (run tSrv_monad) ());
  ignore (Thread.create (run tCli_monad) ());
  ignore (Thread.create (run tCli_monad) ());
  ignore (Thread.create (run tCli_monad) ());
  ignore (Thread.create (run tCli_monad) ());
  ignore (Thread.create (run tCli_monad) ());
  ignore (Thread.create (run tCli_monad) ());
  let* () = run tCli_monad () in
  let* () = run tCli_monad () in
  let* () = run tCli_monad () in
  IO.return ()
