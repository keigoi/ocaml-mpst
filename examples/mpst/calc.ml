open Mpst
open Calc_util
open Concur_shims
let (let*) = IO.bind


type op = Add | Sub | Mul | Div
let calc =
  Fix.(fix [cli;srv]) (fun t -> (* equivalent to fix (fun t -> ...) *)
    choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let tCli (ec: 'ec) =
  (* let _ : 'ec ty = get_ty cli calc in *)
  let* ec = send ec#role_Srv#compute (Add, 20) in
  let* ec = send ec#role_Srv#compute (Sub, 45) in
  let* ec = send ec#role_Srv#compute (Mul, 10) in
  let* ec = send ec#role_Srv#result () in
  let* `answer(ans, ec) = receive ec#role_Srv in
  let* () = close ec in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  IO.return ()

let tSrv es =
  (* let _ : 'es ty = get_ty srv calc in *)
  let rec loop acc (es : 'es) =
    let* ret = receive es#role_Cli in
    match ret with
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) es
    | `result((), es) ->
      let* es = send (es#role_Cli#answer) acc in
      close es
  in loop 0 es

let () =
  let sh = create_shared ~kinds:[`Local,0;`Local,0] calc in
  ignore (Thread.create (fun () -> let* es = accept sh srv in tSrv es) ()); (*FIXME*)
  IO.main_run @@
    let* ec = connect sh cli in
    tCli ec
