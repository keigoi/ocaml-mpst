open Mpst
open Calc_util.Dyn

type op = Add | Sub | Mul | Div
let calc =
  fix (fun t ->
    choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let tCli (ec: 'ec) =
  let _ : 'ec ty = get_ty cli calc
  in  let ec = send ec#role_Srv#compute (Add, 20) in
  let ec = send ec#role_Srv#compute (Sub, 45) in
  let ec = send ec#role_Srv#compute (Mul, 10) in
  let ec = send ec#role_Srv#result () in
  let `answer(ans, ec) = receive ec#role_Srv in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans

let tSrv es =
  let _ : 'es ty = get_ty srv calc in
  let rec loop acc (es : 'es) =
    match receive es#role_Cli with
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) es
    | `result((), es) ->
      let es = send (es#role_Cli#answer) acc in
      close es
  in loop 0 es

let () =
  let sh = create_shared ~kinds:[`Local;`Local] calc in
  ignore (Thread.create (fun () -> accept_and_start sh srv tSrv) ()); (*FIXME*)
  connect_and_start sh cli tCli
  ;
  ()
  (* let g = gen @@ calc in
   * let ec = get_ch cli g in
   * let es = get_ch srv g in
   * List.iter Thread.join [Thread.create tCli ec; Thread.create tSrv es] *)
