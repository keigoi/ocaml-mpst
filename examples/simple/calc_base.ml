open Mpst
open Calc_util.Dyn

type op = Add | Sub | Mul | Div

(* check type of the following with Ctrl-C Ctrl-T *)
let calc =
  gen @@
    fix (fun t ->
        choice_at cli (to_srv compute_or_result)
          (cli, (cli --> srv) compute @@ t)
          (cli, (cli --> srv) result @@
                  (srv --> cli) answer @@ finish))

let () =
  let sch = get_ch srv calc in
  let cch = get_ch cli calc in
  let (_:Thread.t) =
    Thread.create (fun () ->
        let rec loop sch accum =
          match receive sch#role_Cli with
          | `compute((op,x), sch) ->
             let accum' =
               match op with
               | Add -> accum + x
               | Sub -> accum - x
               | Mul -> accum * x
               | Div -> accum / x
             in
             loop sch accum'
          | `result((), sch) ->
             let sch = send sch#role_Cli#answer accum in
             close sch
        in
        loop sch 0
      ) ()
  in
  let cch = send cch#role_Srv#compute (Add, 100) in
  let cch = send cch#role_Srv#compute (Sub, 10) in
  let cch = send cch#role_Srv#compute (Mul, 3) in
  let cch = send cch#role_Srv#compute (Div, 2) in
  let cch = send cch#role_Srv#result () in
  let `answer(ans,cch) = receive cch#role_Srv in
  close cch;
  Printf.printf "answer is: %d\n" ans;
  ()
