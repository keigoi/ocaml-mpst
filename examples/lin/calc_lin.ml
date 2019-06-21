open Mpst_monad
open Linocaml
open Calc_util

type op = Add | Sub | Mul | Div
let calc () =
  fix (fun t -> choice_at cli (to_srv compute_or_result)
           (cli, (cli --> srv) compute @@
                 t)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))

let s = Linocaml.Zero

let tCli_monad () =
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Add, 20) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Sub, 45) in
  let%lin #s = s <@ send (fun x->x#role_Srv#compute) (Mul, 10) in
  let%lin #s = s <@ send (fun x->x#role_Srv#result) () in
  let%lin `answer(ans, #s) = s <@ receive (fun x -> x#role_Srv) in
  s <@ close >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv_monad () =
  let rec loop acc =
    match%lin s <@ receive (fun x->x#role_Cli) with
    | `compute({data=(sym,num)}, #s) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `result(_, #_0) ->
      let%lin #s = s <@ send (fun x->x#role_Cli#answer) acc in
      s <@ close
  in loop 0

let g = create_shared calc [`role_Cli(); `role_Srv()]

let run f g r =
  Linocaml.run' begin
      let%lin #s = connect g r in
      f ()
  end

let () =
   List.iter Thread.join [Thread.create (run tCli_monad g) cli; Thread.create (run tSrv_monad g) srv]
