open Mpst.Scribble.MPST
open Lwt

let rec g =
  lazy begin
  (a -%%-> b)
      ~left:((a,b),
             (b --> c) msg @@
             finish)
      ~right:((a,b),
             dummy_receive c @@
             loop_ g)
    end

let g = Lazy.force g

let a_ = get_sess a g
let b_ = get_sess b g
let c_ = get_sess c g
let () = print_endline "endpoints generated"


let rec t1 i s =
  Lwt_unix.sleep 1.0 >>= fun () ->
  if i >= 10 then begin
    let s = send B (fun x -> x#left) () s in
    close s;
    print_endline "t1 finished";
    Lwt.return ()
  end else begin
      Printf.printf "t1 %d\n" i;
      let s = send B (fun x -> x#right) i s in
      Lwt.return () >>= fun () ->
      t1 (i+1) s
    end


let rec t2 s =
  print_endline "t2";
  receive A s >>= function
  | `left(i, s) ->
     let s = send C (fun x -> x#msg) () s in
     close s;
     print_endline "t2 finished";
     Lwt.return ()
  | `right(i, s) ->
     Printf.printf "t2 %d\n" i;
     t2 s

let rec t3 s =
  print_endline "t3\n";
  receive B s >>= fun (`msg((),s)) ->
  close s;
  print_endline "t3 finished";
  Lwt.return ()

let () =
  Lwt_main.run @@ Lwt.join [t2 (get_sess b g); t1 0 (get_sess a g); t3 (get_sess c g)]
