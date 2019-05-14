open Mpst_shmem.Lin
open Util
open Global
open Session
open LinMonad
open LinMonad.Op

let cli : ([`Cli],_,_,_,_) role = {role=`Cli; lens=Fst}
let srv : ([`Srv],_,_,_,_) role = {role=`Srv; lens=Next Fst}

let compute = {select_label=(fun f-> object method compute v=f v end); offer_label=(fun (v,c) -> `compute(v,c))}
let result = {select_label=(fun f-> object method result v=f v end); offer_label=(fun (v,c) -> `result(v,c))}
let answer = {select_label=(fun f-> object method answer v=f v end); offer_label=(fun (v,c) -> `answer(v,c))}
let compute_or_result =
     {label_merge=(fun l r -> object method compute=l#compute method result=r#result end)}

let finish = one @@ one @@ nil

type op = Add | Sub | Mul | Div
let calc () =
  let rec g =
    lazy (choice_at cli compute_or_result
           (cli, (cli --> srv) compute @@
                 loop g)
           (cli, (cli --> srv) result @@
                 (srv --> cli) answer @@
                 finish))
  in Lazy.force g

let _0 = Mpst_base.LinMonad.Fst

let tCli () =
  let%lin #_0 = send `Srv (fun x->x#compute) (Add, 20) _0 in
  let%lin #_0 = send `Srv (fun x->x#compute) (Sub, 45) _0 in
  let%lin #_0 = send `Srv (fun x->x#compute) (Mul, 10) _0 in
  let%lin #_0 = send `Srv (fun x->x#result) () _0 in
  let%lin `answer(ans, #_0) = receive `Srv _0 in
  close _0 >>= fun () ->
  (* outputs "Answer: -250" (= (20 - 45) * 10) *)
  Printf.printf "Answer: %d\n" ans;
  return ()

let tSrv () =
  let rec loop acc =
    match%lin receive `Cli _0 with
    | `compute({data=(sym,num)}, #_0) ->
      let op = match sym with
        | Add -> (+)   | Sub -> (-)
        | Mul -> ( * ) | Div -> (/)
      in loop (op acc num)
    | `result(_, #_0) ->
      let%lin #_0 = send `Cli (fun x->x#answer) acc _0 in
      close _0
  in loop 0

let g = create_global calc [`Cli; `Srv]
   
let run f g r =
  LinMonad.run begin
      LinMonad.expand >>= fun () ->
      let%lin #_0 = connect g r in
      let%lin #_0 = unone _0 in
      f () >>= fun () ->
      LinMonad.shrink
  end

let () =
  Lwt_main.run (Lwt.join [run tCli g cli; run tSrv g srv])

(* (\* custom label declaration *\)
 * let current = {select_label=(fun f-> object method current v=f v end); offer_label=(fun (v,c) -> `current(v,c))}
 * 
 * (\* merger *\)
 * let compute_result_or_current =
 *   {label_merge=(fun l r ->
 *     object method compute=l#compute method result=l#result
 *            method current=r#current end)}
 * 
 * let calc2 () =
 *   let rec g =
 *     lazy (choice_at cli compute_result_or_current
 *        (cli, choice_at cli compute_or_result
 *              (cli, (cli --> srv) compute @@
 *                    loop g)
 *              (cli, (cli --> srv) result @@
 *                    (srv --> cli) answer @@
 *                    finish))
 *        (cli, (cli --> srv) current @@
 *              (srv --> cli) answer @@
 *              loop g))
 *   in Lazy.force g
 * 
 * let tSrv2 es =
 *   let rec loop acc es =
 *     match%lwt receive `Cli es with
 *     | `compute((sym,num), es) ->
 *       let op = match sym with
 *         | Add -> (+)   | Sub -> (-)
 *         | Mul -> ( * ) | Div -> (/)
 *       in loop (op acc num) es
 *     | `result((), es) ->
 *       let es = send `Cli (fun x->x#answer) acc es in
 *       close es; Lwt.return ()
 *     | `current((), es) ->
 *       let es = send `Cli (fun x->x#answer) acc es in
 *       loop acc es
 *   in loop 0 es
 * 
 * let () =
 *   let calc2 = calc2 () in
 *   let ec = get_sess cli calc2 and es = get_sess srv calc2
 *   in Lwt_main.run (Lwt.join [tCli ec; tSrv2 es]) *)
