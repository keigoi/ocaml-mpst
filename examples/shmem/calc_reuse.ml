(* code reuse through polymorphic variants *)
open Mpst_shmem.Session
open Mpst_shmem.Global
open Mpst_shmem.Util

let (>>=) = Lwt.(>>=)

let cli : ([`Cli],_,_,_,_) role = {role=`Cli; lens=Fst}
let srv : ([`Srv],_,_,_,_) role = {role=`Srv; lens=Next Fst}

let compute = {select_label=(fun f-> object method compute v=f v end); offer_label=(fun (v,c) -> `compute(v,c))}
let result = {select_label=(fun f-> object method result v=f v end); offer_label=(fun (v,c) -> `result(v,c))}
let answer = {select_label=(fun f-> object method answer v=f v end); offer_label=(fun (v,c) -> `answer(v,c))}
let compute_or_result =
     {label_merge=(fun l r -> object method compute=l#compute method result=r#result end)}

let finish = one @@ one @@ nil

type op = Add | Sub | Mul | Div

(* open recursion for later reuse *)
let calcbody body =
    choice_at cli compute_or_result
    (cli, (cli --> srv) compute @@
          loop body)
    (cli, (cli --> srv) result @@
          (srv --> cli) answer @@
          finish)

let calc () =
  let rec g = lazy (calcbody g)
  in Lazy.force g

(* same as before *)
let tCli ec =
  let ec = send `Srv (fun x->x#compute) (Add, 20) ec in
  let ec = send `Srv (fun x->x#compute) (Sub, 45) ec in
  let ec = send `Srv (fun x->x#compute) (Mul, 10) ec in
  let ec = send `Srv (fun x->x#result) () ec in
  let%lwt `answer(ans, ec) = receive `Srv ec in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  Lwt.return ()

(* calculator server using open recursion *)
let srvloop body acc es =
    function
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in body (op acc num) es
    | `result((), es) ->
      let es = send `Cli (fun x->x#answer) acc es in
      close es; Lwt.return ()

let tSrv es =
  let rec loop acc es =
    receive `Cli es >>=
    srvloop loop acc es
  in loop 0 es

let () =
  let g = calc () in
  let ec = get_sess cli g
  and es = get_sess srv g
  in
  Lwt_main.run (Lwt.join [tCli ec; tSrv es])


(*
 * extending server with label "current"
 *)

(* custom label declaration *)
let current = {select_label=(fun f-> object method current v=f v end); offer_label=(fun (v,c) -> `current(v,c))}

(* merger *)
let compute_result_or_current =
  {label_merge=(fun l r ->
    object method compute=l#compute method result=l#result
           method current=r#current end)}

let calc2 () =
  let rec g =
    lazy (choice_at cli compute_result_or_current
       (cli, calcbody g)
       (cli, (cli --> srv) current @@
             (srv --> cli) answer @@
             loop g))
  in Lazy.force g

type ('a,'b) t = [`compute of 'a | `result of 'b]
let tSrv2 es =
  let rec loop acc es =
    receive `Cli es >>= function
    | (#t as v) ->
       srvloop loop acc es v
    | `current((), es) ->
      let es = send `Cli (fun x->x#answer) acc es in
      loop acc es
  in loop 0 es

let () =
  let calc2 = calc2 () in
  let ec = get_sess cli calc2 and es = get_sess srv calc2
  in Lwt_main.run (Lwt.join [tCli ec; tSrv2 es])

let tCli2 ec =
  let ec = send `Srv (fun x->x#compute) (Add,100) ec in
  let ec = send `Srv (fun x->x#current) () ec in
  let%lwt `answer(num,ec) = receive `Srv ec in
  assert (num = 100);
  let ec = send `Srv (fun x->x#compute) (Sub,1) ec in
  let ec = send `Srv (fun x->x#result) () ec in
  let%lwt `answer(num,ec) = receive `Srv ec in
  assert (num = 99);
  close ec;
  Printf.printf "Answer: %d\n" num;
  Lwt.return ()

let () =
  let calc2 = calc2 () in
  let ec = get_sess cli calc2 and es = get_sess srv calc2
  in Lwt_main.run (Lwt.join [tCli2 ec; tSrv2 es])
