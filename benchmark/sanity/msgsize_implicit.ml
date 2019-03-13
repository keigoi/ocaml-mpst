open Mpst_implicit.Util.Global
open Mpst_implicit.Util.Session
open Mpst_implicit.Util

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}

let finish = one @@ one @@ nil

let (>>=) = Lwt.(>>=)

let mkglobal () =
  let rec g =
    lazy begin
        (b --> a) msg @@
          choice_at a left_or_right
            (a, (a --> b) left @@
                  finish)
            (a, (a --> b) right @@
                  loop g)
      end
  in
  Lazy.force g

let a = `A
let b = `B
let c = `C
  
let tA cnt s =
  let rec loop i s =
    receive b s >>= fun (`msg((),s)) ->
    if i > 0 then begin
        let s = send b (fun x->x#left) () s in
        close s;
        Lwt.return ()
      end else begin
        let s = send b (fun x->x#right) () s in
        loop (i-1) s
      end
  in
  loop cnt s

let tB s =
  let rec loop s =
    let s = send a (fun x->x#msg) () s in
    receive a s >>= function
    | `left((),s) ->
       close s;
       Lwt.return ()
    | `right((),s) ->
       loop s
  in
  loop s

(* let count = int_of_string Sys.argv.(1) *)
let count = 10000

let run () =
  let g = mkglobal () in
  let sa = get_sess a g in
  let sb = get_sess b g in
  Lwt_main.run (Lwt.join [tA count sa; tB sb])
