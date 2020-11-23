(* code reuse through polymorphic variants *)
open Concur_shims
open Mpst
open Calc_util
let (let*) = IO.bind

type op = Add | Sub | Mul | Div

(* open recursion for later reuse *)
let calcbody self =
    choice_at cli (to_srv compute_or_result)
    (cli, (cli --> srv) compute @@
          self)
    (cli, (cli --> srv) result @@
          (srv --> cli) answer @@
          finish)

let calc' () =
  fix (fun t -> calcbody t)

(* same as before *)
let tCli ec =
  let* ec = send (ec#role_Srv#compute) (Add, 20) in
  let* ec = send (ec#role_Srv#compute) (Sub, 45) in
  let* ec = send (ec#role_Srv#compute) (Mul, 10) in
  let* ec = send (ec#role_Srv#result) () in
  let* `answer(ans, ec) = receive ec#role_Srv in
  let* () = close ec in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  IO.return ()

(* calculator server using open recursion *)
let srvbody self acc =
    function
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in self (op acc num) es
    | `result((), es) ->
      let* es = send (es#role_Cli#answer) acc in
      close es

let tSrv' es =
  let rec loop acc es =
    let* lab = receive es#role_Cli in
    srvbody loop acc lab
  in loop 0 es

let (_:unit IO.io) =
  let g = gen @@ calc' () in
  let ec = get_ch cli g
  and es = get_ch srv g
  in
  IO_list.iter Thread.join [Thread.create tCli ec; Thread.create tSrv' es]


(*
 * extending server with label "current"
 *)

open Mpst.Types

(* custom label declaration *)
let current =
  {obj={make_obj=(fun v-> object method current=v end);
        call_obj=(fun o->o#current)};
   var={make_var=(fun v -> `current(v));
        match_var=(function `current(v) -> Some v | _ -> None)}}

(* merger *)
let compute_result_or_current =
  {disj_concat=(fun l r ->
    object method compute=l#compute method result=l#result
      method current=r#current end);
   disj_splitL=(fun lr-> (lr :> <compute:_; result:_>));
   disj_splitR=(fun lr-> (lr :> <current:_>));
  }

let calc2' () =
  fix (fun t ->
    choice_at cli (to_srv compute_result_or_current)
       (cli, calcbody t)
       (cli, (cli --> srv) current @@
             (srv --> cli) answer @@
             t))

let tSrv2' es =
  let rec loop acc es =
    let* label = receive es#role_Cli in
    match label with
    | (`compute(_) | `result(_) as v) ->
       srvbody loop acc v
    | `current((), es) ->
      let* es = send (es#role_Cli#answer) acc in
      loop acc es
  in loop 0 es

let (_ : unit IO.io) =
  let g = gen @@ calc2' () in
  let ec = get_ch cli g and es = get_ch srv g
  in
  IO_list.iter Thread.join [Thread.create tCli ec; Thread.create tSrv2' es]

let tCli2 ec =
  let* ec = send (ec#role_Srv#compute) (Add,100) in
  let* ec = send (ec#role_Srv#current) () in
  let* `answer(num,ec) = receive ec#role_Srv in
  assert (num = 100);
  let* ec = send (ec#role_Srv#compute) (Sub,1) in
  let* ec = send (ec#role_Srv#result) () in
  let* `answer(num,ec) = receive ec#role_Srv in
  assert (num = 99);
  let* () = close ec in
  Printf.printf "Answer: %d\n" num;
  IO.return ()


let () =
  let g = gen @@ calc2' () in
  let ec = get_ch cli g and es = get_ch srv g
  in
  IO.main_run @@ IO_list.iter Thread.join [Thread.create tCli2 ec; Thread.create tSrv2' es]
