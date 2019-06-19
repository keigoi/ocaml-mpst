open Mpst.Base
open Mpst.Local
open Mpst.Global

let cli = {role_index=Zero;
           role_label={make_obj=(fun v->object method role_Cli=v end);
                       call_obj=(fun o->o#role_Cli)}}
let srv = {role_index=Succ Zero;
           role_label={make_obj=(fun v->object method role_Srv=v end);
                       call_obj=(fun o->o#role_Srv)}}

let compute = {obj={make_obj=(fun v-> object method compute=v end);
                    call_obj=(fun o->o#compute)};
               var=(fun v -> `compute(v))}
let result = {obj={make_obj=(fun v-> object method result=v end);
                   call_obj=(fun o->o#result)};
              var=(fun v -> `result(v))}
let answer = {obj={make_obj=(fun v-> object method answer=v end);
                   call_obj=(fun o->o#answer)};
              var=(fun v -> `answer(v))}
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

type op = Add | Sub | Mul | Div
let calc () =
  fix (fun t ->
    choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let tCli ec =
  let ec = send (ec#role_Srv#compute) (Add, 20) in
  let ec = send (ec#role_Srv#compute) (Sub, 45) in
  let ec = send (ec#role_Srv#compute) (Mul, 10) in
  let ec = send (ec#role_Srv#result) () in
  let `answer(ans, ec) = Event.sync (ec#role_Srv) in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans

let tSrv es =
  let rec loop acc es =
    match Event.sync (es#role_Cli) with
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
  let g = unseq @@ calc () in
  let ec = get_ep cli g in
  let es = get_ep srv g in
  List.iter Thread.join [Thread.create tCli ec; Thread.create tSrv es]

(* custom label declaration *)
let current =
  {obj={make_obj=(fun v-> object method current=v end);
        call_obj=(fun o->o#current)};
   var=(fun v -> `current(v))}

(* merger *)
let compute_result_or_current =
  {obj_merge=(fun l r ->
    object method compute=l#compute method result=l#result
      method current=r#current end);
   obj_splitL=(fun lr-> (lr :> <compute:_; result:_>));
   obj_splitR=(fun lr-> (lr :> <current:_>));
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
    match Event.sync (es#role_Cli) with
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) es
    | `result((), es) ->
      let es = send (es#role_Cli#answer) acc in
      close es
    | `current((), es) ->
      let es = send (es#role_Cli#answer) acc in
      loop acc es
  in loop 0 es

let () =
  let calc2 = unseq @@ calc2 () in
  let ec = get_ep cli calc2 and es = get_ep srv calc2 in
  ignore @@ Thread.create tSrv2 es;
  tCli ec
