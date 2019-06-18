open Mpst_simple
type op = Add | Sub | Mul | Div

let cli = {role_index=Zero;
           role_label={make_obj=(fun v->object method role_Cli=v end);
                       call_obj=(fun o->o#role_Cli)}}
let wrk = {role_index=Succ Zero;
           role_label={make_obj=(fun v->object method role_Wrk=v end);
                       call_obj=(fun o->o#role_Wrk)}}

let compute = {obj={make_obj=(fun v-> object method compute=v end); call_obj=(fun o->o#compute)}; var=(fun v -> `compute(v))}
let result = {obj={make_obj=(fun v-> object method result=v end); call_obj=(fun o->o#result)}; var=(fun v -> `result(v))}
let answer = {obj={make_obj=(fun v-> object method answer=v end); call_obj=(fun o->o#answer)}; var=(fun v -> `answer(v))}
let compute_or_result =
  {obj_merge=(fun l r -> object method compute=l#compute method result=r#result end);
   obj_splitL=(fun lr -> (lr :> <compute : _>));
   obj_splitR=(fun lr -> (lr :> <result : _>));
  }
let to_wrk m =
  {obj_merge=(fun l r -> object method role_Wrk=m.obj_merge l#role_Wrk r#role_Wrk end);
   obj_splitL=(fun lr -> object method role_Wrk=m.obj_splitL lr#role_Wrk end);
   obj_splitR=(fun lr -> object method role_Wrk=m.obj_splitR lr#role_Wrk end);
  }

let calc =
  fix (fun t ->
      (choice_at cli (to_wrk compute_or_result)
         (cli, (scatter cli wrk) compute @@
               t)
         (cli, (scatter cli wrk) result @@
               (gather wrk cli) answer @@
               finish)))

let const x _ = x
let tCli ec =
  let ec = sendmany ec#role_Wrk#compute (fun i -> (Add, 20+i)) in
  let ec = sendmany ec#role_Wrk#compute (const (Sub, 45)) in
  let ec = sendmany ec#role_Wrk#compute (const (Mul, 10)) in
  let ec = sendmany ec#role_Wrk#result (const ()) in
  let `answer(ans, ec) = receive ec#role_Wrk in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  List.iter (fun ans -> Printf.printf "Answer: %d\n" ans) ans

let tWrk ew =
  let rec loop acc ew =
    match receive ew#role_Cli with
    | `compute((sym,num), ew) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) ew
    | `result((), ew) ->
      let ew = send (ew#role_Cli#answer) acc in
      close ew
  in loop 0 ew

let () =
  let g = unseq_param calc [1;100] in
  let ec = get_ep cli g
  and ews = get_ep_list wrk g
  in List.iter Thread.join
       ([Thread.create tCli ec] @ (List.map (fun ew -> Thread.create tWrk ew) ews))
