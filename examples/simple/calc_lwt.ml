open Mpst_lwt
open Calc_util.Dyn
let (let/) = Lwt.bind

type op = Add | Sub | Mul | Div
(* let calc =
 *   gen @@ fix (fun t ->
 *     choice_at cli (to_srv compute_or_result)
 *            (cli, (cli --> srv) compute @@
 *                  t)
 *            (cli, (cli --> srv) result @@
 *                  (srv --> cli) answer @@
 *                  finish)) *)

let tCli ec =
  let/ ec = send ec#role_Srv#compute (Add, 20) in
  let/ ec = send ec#role_Srv#compute (Sub, 45) in
  let/ ec = send ec#role_Srv#compute (Mul, 10) in
  let/ ec = send ec#role_Srv#result () in
  print_endline "client waiting";
  let/ `answer(ans, ec) = receive ec#role_Srv in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  Lwt.return_unit

let tSrv es =
  let rec loop acc es =
    let/ var = receive es#role_Cli in
    match var with
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) es
    | `result((), es) ->
      let/ es = send (es#role_Cli#answer) acc in
      close es;
      Lwt.return_unit
  in loop 0 es

(* let () =
 *   let es = get_ch srv calc in
 *   let ec = get_ch cli calc in
 *   let t = tSrv es in
 *   let u = tCli ec in
 *   Lwt_main.run (Lwt.join [t;u])
 *   ;
 *   () *)

(* custom label declaration *)
let current =
  {obj={make_obj=(fun v-> object method current=v end);
        call_obj=(fun o->o#current)};
   var=(fun v -> `current(v))}

(* merger *)
let compute_result_or_current =
  {disj_merge=(fun l r ->
    object method compute=l#compute method result=l#result
      method current=r#current end);
   disj_splitL=(fun lr-> (lr :> <compute:_; result:_>));
   disj_splitR=(fun lr-> (lr :> <current:_>));
  }

let calc2 () =
  fix (fun t ->
    choice_at cli (to_srv compute_result_or_current)
       (cli, choice_at cli (to_srv compute_or_result)
             (cli, (cli --> srv) compute @@
                   t)
             (cli, (cli --> srv) result @@
                   (srv --> cli) answer @@
                   finish))
       (cli, (cli --> srv) current @@
             (srv --> cli) answer @@
             t))

let tSrv2 es =
  let rec loop acc es =
    print_endline "server waiting";
    let/ var = receive es#role_Cli in
    print_endline "received";
    match var with
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) es
    | `result((), es) ->
      let/ es = send (es#role_Cli#answer) acc in
      close es;
      Lwt.return_unit
    | `current((), es) ->
      let/ es = send (es#role_Cli#answer) acc in
      loop acc es
  in loop 0 es

let () =
  let calc2 = gen @@ calc2 () in
  let ec = get_ch cli calc2 and es = get_ch srv calc2 in
  Lwt_main.run @@ Lwt.join [tSrv2 es; tCli ec]
