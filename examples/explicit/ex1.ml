open Mpst.M
open Mpst_explicit
open HList

let ($$) f m =
  Lwt.bind m (fun x -> f x)

type generic_pipe_channel =
  {inp: Lwt_io.input_channel;
   out: Lwt_io.output_channel;
   mutable tagbuf: Base.tag option
  }

let new_channel () =
  let pipe () =
    let inp,out = Lwt_unix.pipe () in
    Lwt_io.of_fd Lwt_io.input inp, Lwt_io.of_fd Lwt_io.output out
  in
  let my_inp, otr_out = pipe () in
  let otr_inp, my_out = pipe () in
  {inp=my_inp; out=my_out; tagbuf=None}, {inp=otr_inp; out=otr_out; tagbuf=None}

let generic_handler label =
  let open Lwt in
  let mytag = Mpst.M.Base.make_tag label.var in
  {write=(fun h v ->
     Lwt_io.write_value h.out (mytag, v) >>= fun () ->
     Lwt_io.flush h.out);
   read=(fun h -> Lwt_io.read_value h.inp);
   try_parse=(fun _ (tag,v) ->
     if tag=mytag then
       Some (Obj.obj v)
     else
       None)
  }

let (!%) label =
  label %% generic_handler label

open Lwt

let g =
  (a -!-> b) !%msg @@
    choice_at a (to_b left_or_right)
      (a, (a --> b) !%left @@ disconnect a b @@ finish)
      (a, (a --> b) !%right @@ disconnect a b @@ finish)

let ea, eb = get_ep a g vec_all_empty, get_ep b g vec_all_empty

let pa, pb = new_channel ()

let () =
  Random.self_init ()

(* role B *)
let ta =
  begin
    receive (eb pa)#role_A >>= fun (`msg(_,eb)) ->
    receive eb#role_A >>= function
    | `left(str, eb) ->
       print_endline @@ "received: " ^ str;
       close $$ eb#disconnect
    | `right(x, eb) ->
       Printf.printf "received: %d\n" x;
       close $$ eb#disconnect
  end
  >>= fun () ->
  print_endline "finished";
  Lwt.return_unit

(* role A *)
let tb =
  (ea pb)#role_B#msg () >>= fun ea ->
  if Random.bool () then begin
      ea#role_B#left "Hello" >>= fun ea ->
      close $$ ea#disconnect
    end else begin
      ea#role_B#right 49 >>= fun ea ->
      close $$ ea#disconnect
    end

let () =
  Lwt_main.run @@ Lwt.join [ta; tb]
