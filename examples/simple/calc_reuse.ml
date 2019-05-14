(* code reuse through polymorphic variants *)
open Mpst_simple

let cli = {lens=Zero;
           label={make_obj=(fun v->object method role_Cli=v end);
                  call_obj=(fun o->o#role_Cli);
                 make_var=(fun v->(`role_Cli(v):[`role_Cli of _]))}}
let srv = {lens=Succ Zero;
           label={make_obj=(fun v->object method role_Srv=v end);
                  call_obj=(fun o->o#role_Srv);
                 make_var=(fun v->(`role_Srv(v):[`role_Srv of _]))}}

let compute = {make_obj=(fun v-> object method compute=v end); call_obj=(fun o->o#compute); make_var=(fun v -> `compute(v))}
let result = {make_obj=(fun v-> object method result=v end); call_obj=(fun o->o#result); make_var=(fun v -> `result(v))}
let answer = {make_obj=(fun v-> object method answer=v end); call_obj=(fun o->o#answer); make_var=(fun v -> `answer(v))}
let compute_or_result =
  {obj_merge=(fun l r -> object method compute=l#compute method result=r#result end);
   obj_splitL=(fun lr -> (lr :> <compute : _>));
   obj_splitR=(fun lr -> (lr :> <result : _>));
  }
let to_srv m =
  {obj_merge=(fun l r -> object method role_Srv=m.obj_merge l#role_Srv r#role_Srv end);
   obj_splitL=(fun lr -> object method role_Srv=m.obj_splitL lr#role_Srv end);
   obj_splitR=(fun lr -> object method role_Srv=m.obj_splitR lr#role_Srv end);
  }

let finish = one @@ one @@ nil

type op = Add | Sub | Mul | Div

(* open recursion for later reuse *)
let calcbody self =
    choice_at cli (to_srv compute_or_result)
    (cli, (cli --> srv) compute @@
          goto2 self)
    (cli, (cli --> srv) result @@
          (srv --> cli) answer @@
          finish2)

let calc' () =
  let rec g = lazy (calcbody g)
  in Lazy.force g

(* same as before *)
let tCli ec =
  let ec = send (ec#role_Srv#compute) (Add, 20) in
  let ec = send (ec#role_Srv#compute) (Sub, 45) in
  let ec = send (ec#role_Srv#compute) (Mul, 10) in
  let ec = send (ec#role_Srv#result) () in
  let `answer(ans, ec) = Event.sync (ec#role_Srv) in
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
      let es = send (es#role_Cli#answer) acc in
      close es

let tSrv' es =
  let rec loop acc es =
    let lab = Event.sync (es#role_Cli) in
    srvbody loop acc es lab
  in loop 0 es

let () =
  let g = calc' () in
  let ec = get_ep cli g
  and es = get_ep srv g
  in
  List.iter Thread.join [Thread.create tCli ec; Thread.create tSrv' es]


(*
 * extending server with label "current"
 *)

(* custom label declaration *)
let current = {make_obj=(fun v-> object method current=v end); call_obj=(fun o->o#current); make_var=(fun v -> `current(v))}

(* merger *)
let compute_result_or_current =
  {obj_merge=(fun l r ->
    object method compute=l#compute method result=l#result
      method current=r#current end);
   obj_splitL=(fun lr-> (lr :> <compute:_; result:_>));
   obj_splitR=(fun lr-> (lr :> <current:_>));
  }

let calc2' () =
  let rec g =
    lazy (choice_at cli (to_srv compute_result_or_current)
       (cli, calcbody g)
       (cli, (cli --> srv) current @@
             (srv --> cli) answer @@
             goto2 g))
  in Lazy.force g

let tSrv2' es =
  let rec loop acc es =
    let label = Event.sync (es#role_Cli) in
    match label with
    | (`compute(_) | `result(_) as v) ->
       srvbody loop acc es v
    | `current((), es) ->
      let es = send (es#role_Cli#answer) acc in
      loop acc es
  in loop 0 es

let () =
  let g = calc2' () in
  let ec = get_ep cli g and es = get_ep srv g
  in List.iter Thread.join [Thread.create tCli ec; Thread.create tSrv2' es]

let tCli2 ec =
  let ec = send (ec#role_Srv#compute) (Add,100) in
  let ec = send (ec#role_Srv#current) () in
  let `answer(num,ec) = Event.sync (ec#role_Srv) in
  assert (num = 100);
  let ec = send (ec#role_Srv#compute) (Sub,1) in
  let ec = send (ec#role_Srv#result) () in
  let `answer(num,ec) = Event.sync (ec#role_Srv) in
  assert (num = 99);
  close ec;
  Printf.printf "Answer: %d\n" num

let () =
  let g = calc2' () in
  let ec = get_ep cli g and es = get_ep srv g
  in List.iter Thread.join [Thread.create tCli2 ec; Thread.create tSrv2' es]
