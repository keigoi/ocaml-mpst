
open Mpst_mult
type op = Add | Sub | Mul | Div

let rec make cont lens n =
  if n = 0 then
    []
  else
    WrapGuard(
        lazy
          begin
            let xs = unlist @@ get lens (eps @@ Lazy.force cont) in
            List.nth xs (n-1)
          end) :: make cont lens (n-1)
  

let rec goto2_ n =fun xs ->
  {global=Guard((lazy(Lazy.force xs).global));
   loop=(`Loop:[`Loop]);
   endpoints=
     Cons(EOne(WrapGuard(lazy (unone @@ get ZeroO (eps @@ Lazy.force xs)))),
     Cons(EList(List.init n (fun i ->
       WrapGuard(lazy begin 
         let xs = unlist @@ get (Succ ZeroM) (eps @@ Lazy.force xs) in
         List.nth xs i
       end))),
     Nil))}

let loop2 = loop_ (goto2_ 10)
let finish2 = {global=Finish; loop=(`Finish:[`Finish]); endpoints=one @@ list 10 @@ nil}

let cli = {lens=ZeroO;
           label={name="Master";
                  make_obj=(fun v->object method role_Mst=v end);
                  call_obj=(fun o->o#role_Mst);
                 make_var=(fun v->(`role_Mst(v):[`role_Mst of _]))}}
let wrk = {lens=Succ ZeroM;
           label={name="Worker";
                  make_obj=(fun v->object method role_Wrk=v end);
                  call_obj=(fun o->o#role_Wrk);
                 make_var=(fun v->(`role_Wrk(v):[`role_Wrk of _]))}}

let compute = {name="compute"; make_obj=(fun v-> object method compute=v end); call_obj=(fun o->o#compute); make_var=(fun v -> `compute(v))}
let result = {name="result"; make_obj=(fun v-> object method result=v end); call_obj=(fun o->o#result); make_var=(fun v -> `result(v))}
let answer = {name="answer"; make_obj=(fun v-> object method answer=v end); call_obj=(fun o->o#answer); make_var=(fun v -> `answer(v))}
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

let calc () =
  loop2 (fun self ->
      (choice_at cli (to_wrk compute_or_result)
         (cli, (select_one cli wrk) compute >>
               self)
         (cli, (scatter cli wrk) result >>
               (gather wrk cli) answer)))

let calc () = run_arr_finish (calc ()) finish2
let () = print_global (calc ())

let const x _ = x
let tMst ec =
  let ec = sendone (ec#role_Wrk#compute) 1 (Add, 20) in
  let ec = sendone (ec#role_Wrk#compute) 2 (Sub, 45) in
  let ec = sendone (ec#role_Wrk#compute) 2 (Mul, 10) in
  let ec = sendmany (ec#role_Wrk#result) (const ()) in
  let `role_Wrk(`answer(ans, ec)) = Event.sync ec in
  close ec;
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  List.iter (fun ans -> Printf.printf "Answer: %d\n" ans) ans

let tWrk i es =
  let rec loop acc es =
    let `role_Mst(label) = Event.sync es in
    match label with
    | `compute((sym,num), es) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num) es
    | `result((), es) ->
      let es = send (es#role_Mst#answer) acc in
      close es
  in loop 0 es

let () =
  (* let g = ChVecExample.calc () in *)
  let g = calc () in
  let ec = get_ep cli g
  and ess = get_ep_list wrk g
  in List.iter Thread.join
       ([Thread.create tMst ec] @ (List.mapi (fun i es -> Thread.create (tWrk i) es) ess))

