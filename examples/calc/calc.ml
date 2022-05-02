open Mpst.BasicCombinators
open Mpst.Unicast

type op = Add | Sub | Mul | Div

[%%declare_roles_prefixed cli, srv]
[%%declare_labels compute, result, answer]

let calc =
  loop_with [ cli; srv ] (fun t ->
      [%choice_at
        cli
          ( (cli --> srv) compute @@ (cli ==> srv) t,
            (cli --> srv) result @@ (srv ==> cli) finish )])

let (`cons (sc, `cons (ss, _))) = extract calc

let tCli () =
  let sc = select sc#role_Srv#compute in
  let sc = send sc#role_Srv (Add, 20) in
  let sc = select sc#role_Srv#compute in
  let sc = send sc#role_Srv (Sub, 45) in
  let sc = select sc#role_Srv#compute in
  let sc = send sc#role_Srv (Mul, 10) in
  let sc = select sc#role_Srv#result in
  let ans, sc = receive sc#role_Srv in
  let () = close sc in
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  ()

let tSrv () =
  let (_ : 'ss) = ss in
  (* let _ : 'es ty = get_ty srv calc in *)
  let rec loop acc (ss : 'ss) =
    match branch ss#role_Cli with
    | `compute ss ->
        let (sym, num), ss = receive ss#role_Cli in
        let op =
          match sym with
          | Add -> ( + )
          | Sub -> ( - )
          | Mul -> ( * )
          | Div -> ( / )
        in
        loop (op acc num) ss
    | `result ss ->
        let ss = send ss#role_Cli acc in
        close ss
  in
  loop 0 ss

let () =
  let ts = List.map (fun f -> Thread.create f ()) [ tCli; tSrv ] in
  List.iter Thread.join ts
