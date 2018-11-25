(* loop with branching *)
open Mpst.ThreeParty
open Mpst.ThreeParty.Shmem
let (>>=) = Lwt.(>>=)

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
  receive b s >>= fun (`msg(v,s)) ->
  print_endline "tA cont";
  if v > 100 then begin
      print_endline ">100";
      let s = send b (fun x -> x#left) "> 100" s in
      receive c s >>= fun (`msg(v,s)) ->
      close s;
      Lwt.return ()
    end else begin
      print_endline "<=100";
      let s = send b (fun x -> x#right) false s in
      receive c s >>= fun (`msg(v,s)) ->
      print_endline "tA received from c";
      tA s
    end

let rec tB i s : unit Lwt.t =
  let open Lwt in
  Printf.printf "tB %d\n" i;
  let s = send a (fun x -> x#msg) i s in
  receive a s >>= function
  | `left(v,s) ->
     let s = send c (fun x->x#left) () s in
     close s;
     Lwt.return ()
  | `right(v,s) ->
     let s = send c (fun x->x#right) () s in
     tB (i+1) s

let rec tC s : unit Lwt.t =
  print_endline "tC";
  let open Lwt in
  receive b s >>= function
  | `left(v,s) ->
     let s = send a (fun x->x#msg) 100 s in
     close s;
     Lwt.return ()
  | `right(w,s) ->
     let s = send a (fun x->x#msg) "abc" s in
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
