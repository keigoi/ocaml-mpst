open Mpst_simple
open Mpst_simple.Lin
open Mpst_simple.LinMonad
open Mpst_simple.LinMonad.Op

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
let calc () =
  let rec g =
    lazy (choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 goto g)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))
  in Lazy.force g

let _0 = Mpst_simple.LinMonad.Fst

let tCli () =
  let%lin #_0 = send (fun x->x#role_srv#compute) (Add, 20) _0 in
  let%lin #_0 = send (fun x->x#role_srv#compute) (Sub, 45) _0 in
  let%lin #_0 = send (fun x->x#role_srv#compute) (Mul, 10) _0 in
  let%lin #_0 = send (fun x->x#role_srv#result) () _0 in
  let%lin `role_srv(`answer(ans, #_0)) = receive _0 in
  close _0 >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv () =
  let rec loop acc =
    match%lin receive _0 with
    | `role_cli(`compute({data=(sym,num)}, #_0)) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `role_cli(`result(_, #_0)) ->
      let%lin #_0 = send (fun x->x#role_cli#answer) acc _0 in
      close _0
  in loop 0

let g = create_global calc [`role_cli(); `role_srv()]

let run f g r =
  LinMonad.run begin
      LinMonad.expand >>= fun () ->
      let%lin #_0 = connect g r in
      f () >>= fun () ->
      LinMonad.shrink
  end

let () =
   List.iter Thread.join [Thread.create (run tCli g) cli; Thread.create (run tSrv g) srv]
