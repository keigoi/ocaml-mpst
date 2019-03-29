open Mpst_simple
open Mpst_simple.Lin
open Mpst_simple.LinMonad
open Mpst_simple.LinMonad.Op

let cli = {lens=Zero;
           label={make_obj=(fun v->object method role_Cli=v end);
                 make_var=(fun v->(`role_Cli(v):[`role_Cli of _]))}}
let srv = {lens=Succ Zero;
           label={make_obj=(fun v->object method role_Srv=v end);
                 make_var=(fun v->(`role_Srv(v):[`role_Srv of _]))}}

let compute = {make_obj=(fun v-> object method compute=v end); make_var=(fun v -> `compute(v))}
let result = {make_obj=(fun v-> object method result=v end); make_var=(fun v -> `result(v))}
let answer = {make_obj=(fun v-> object method answer=v end); make_var=(fun v -> `answer(v))}
let compute_or_result =
     {obj_merge=(fun l r -> object method compute=l#compute method result=r#result end)}
let to_srv m =
  {obj_merge=(fun l r -> object method role_Srv=m.obj_merge l#role_Srv r#role_Srv end)}

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

let tCli_monad () =
  let%lin #_0 = send (fun x->x#role_Srv#compute) (Add, 20) _0 in
  let%lin #_0 = send (fun x->x#role_Srv#compute) (Sub, 45) _0 in
  let%lin #_0 = send (fun x->x#role_Srv#compute) (Mul, 10) _0 in
  let%lin #_0 = send (fun x->x#role_Srv#result) () _0 in
  let%lin `role_Srv(`answer(ans, #_0)) = receive _0 in
  close _0 >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv_monad () =
  let rec loop acc =
    match%lin receive _0 with
    | `role_Cli(`compute({data=(sym,num)}, #_0)) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `role_Cli(`result(_, #_0)) ->
      let%lin #_0 = send (fun x->x#role_Cli#answer) acc _0 in
      close _0
  in loop 0

let g = create_shared calc [`role_Cli(); `role_Srv()]

let run f g r =
  LinMonad.run begin
      LinMonad.expand >>= fun () ->
      let%lin #_0 = connect g r in
      f () >>= fun () ->
      LinMonad.shrink
  end

let () =
   List.iter Thread.join [Thread.create (run tCli_monad g) cli; Thread.create (run tSrv_monad g) srv]
