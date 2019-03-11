(* loop with branching *)
open Mpst_shmem.Global
open Mpst_shmem.Session
open Mpst_shmem.Util
let (>>=) = Lwt.(>>=)

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil

let mk_g () =
  let rec g =
    lazy
      begin
        (b --> a) msg @@
        choice_at a left_or_right
            (a, (a --> b) left @@
                (b --> c) left @@
                (c --> a) msg @@
                finish)
            (a, (a --> b) right @@
                (b --> c) right @@
                (c --> a) msg @@
                loop g)
    end
    in Lazy.force g

let rec tA s : unit Lwt.t =
  let open Lwt in
  print_endline "tA";
  receive `B s >>= fun (`msg(v,s)) ->
  print_endline "tA cont";
  if v > 100 then begin
      print_endline ">100";
      let s = send `B (fun x -> x#left) "> 100" s in
      receive `C s >>= fun (`msg(v,s)) ->
      close s;
      Lwt.return ()
    end else begin
      print_endline "<=100";
      let s = send `B (fun x -> x#right) false s in
      receive `C s >>= fun (`msg(v,s)) ->
      print_endline "tA received from c";
      tA s
    end

let rec tB i s : unit Lwt.t =
  let open Lwt in
  Printf.printf "tB %d\n" i;
  let s = send `A (fun x -> x#msg) i s in
  receive `A s >>= function
  | `left(v,s) ->
     let s = send `C (fun x->x#left) () s in
     close s;
     Lwt.return ()
  | `right(v,s) ->
     let s = send `C (fun x->x#right) () s in
     tB (i+1) s

let rec tC s : unit Lwt.t =
  print_endline "tC";
  let open Lwt in
  receive `B s >>= function
  | `left(v,s) ->
     let s = send `A (fun x->x#msg) 100 s in
     close s;
     Lwt.return ()
  | `right(w,s) ->
     let s = send `A (fun x->x#msg) "abc" s in
     tC s


let () =
  let g = mk_g () in
  Lwt_main.run (Lwt.join [tA (get_sess a g); tB 0 (get_sess b g); tC (get_sess c g)])


let mk_g2 () =
  let rec g =
    lazy
      begin
        (b --> a) msg @@
        choice_at a left_or_right
          (a, (a --> b) left @@
                choice_at a left_or_right
                    (a, (a --> b) left @@
                        (b --> c) left @@
                        (c --> a) msg @@
                        finish)
                    (a, (a --> b) right @@
                        (b --> c) right @@
                        (c --> a) msg @@
                        loop g))
          (a, (a --> b) right @@
              (b --> c) right @@
              (c --> a) msg @@
              loop g)
      end
  in
  Lazy.force g
