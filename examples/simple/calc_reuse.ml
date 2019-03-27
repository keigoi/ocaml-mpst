(* code reuse through polymorphic variants *)
open Mpst_simple


let cli = {lens=Fst;
           role={make_obj=(fun v->object method role_cli=v end);
                 make_var=(fun v->(`role_cli(v):[`role_cli of _]))}}
let srv = {lens=Next Fst;
           role={make_obj=(fun v->object method role_srv=v end);
                 make_var=(fun v->(`role_srv(v):[`role_srv of _]))}}

let compute = {make_obj=(fun v-> object method compute=v end); make_var=(fun v -> `compute(v))}
let result = {make_obj=(fun v-> object method result=v end); make_var=(fun v -> `result(v))}
let answer = {make_obj=(fun v-> object method answer=v end); make_var=(fun v -> `answer(v))}
let compute_or_result =
     {obj_merge=(fun l r -> object method compute=l#compute method result=r#result end)}
let to_srv m =
  {obj_merge=(fun l r -> object method role_srv=m.obj_merge l#role_srv r#role_srv end)}

let finish = one @@ one @@ nil

type op = Add | Sub | Mul | Div

(* open recursion for later reuse *)
let calcbody self =
    choice_at cli (to_srv compute_or_result)
    (cli, (cli --> srv) compute @@
          goto self)
    (cli, (cli --> srv) result @@
          (srv --> cli) answer @@
          finish)

let calc' () =
  let rec g = lazy (calcbody g)
  in Lazy.force g

(* same as before *)
let tCli ec =
  let ec = ec#role_srv#compute (Add, 20) in
  let ec = ec#role_srv#compute (Sub, 45) in
  let ec = ec#role_srv#compute (Mul, 10) in
  let ec = ec#role_srv#result () in
  let `role_srv(`answer(ans, ec)) = Event.sync ec in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans

(* calculator server using open recursion *)
let srvbody self acc es =
    function
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in self (op acc num) es
    | `result((), es) ->
      let es = es#role_cli#answer acc in
      close es

let tSrv' es =
  let rec loop acc es =
    let `role_cli(lab) = Event.sync es in
    srvbody loop acc es lab
  in loop 0 es

let () =
  let g = calc' () in
  let ec = get_sess cli g
  and es = get_sess srv g
  in
  List.iter Thread.join [Thread.create tCli ec; Thread.create tSrv' es]


(*
 * extending server with label "current"
 *)

(* custom label declaration *)
let current = {make_obj=(fun v-> object method current=v end); make_var=(fun v -> `current(v))}

(* merger *)
let compute_result_or_current =
  {obj_merge=(fun l r ->
    object method compute=l#compute method result=l#result
           method current=r#current end)}

let calc2' () =
  let rec g =
    lazy (choice_at cli (to_srv compute_result_or_current)
       (cli, calcbody g)
       (cli, (cli --> srv) current @@
             (srv --> cli) answer @@
             goto g))
  in Lazy.force g

let tSrv2' es =
  let rec loop acc es =
    let `role_cli(label) = Event.sync es in
    match label with
    | (`compute(_) | `result(_) as v) ->
       srvbody loop acc es v
    | `current((), es) ->
      let es = es#role_cli#answer acc in
      loop acc es
  in loop 0 es

let () =
  let g = calc2' () in
  let ec = get_sess cli g and es = get_sess srv g
  in List.iter Thread.join [Thread.create tCli ec; Thread.create tSrv2' es]

let tCli2 ec =
  let ec = ec#role_srv#compute (Add,100) in
  let ec = ec#role_srv#current () in
  let `role_srv(`answer(num,ec)) = Event.sync ec in
  assert (num = 100);
  let ec = ec#role_srv#compute (Sub,1) in
  let ec = ec#role_srv#result () in
  let `role_srv(`answer(num,ec)) = Event.sync ec in
  assert (num = 99);
  close ec;
  Printf.printf "Answer: %d\n" num

let () =
  let g = calc2' () in
  let ec = get_sess cli g and es = get_sess srv g
  in List.iter Thread.join [Thread.create tCli2 ec; Thread.create tSrv2' es]
