(* simple ring protocol *)
open Mpst_simple

let ring = (a --> b) msg @@ (b --> c) msg @@ (c --> a) msg finish

(* the above is equivalent to following: *)
module ChVecExample = struct
  let force = Lazy.force

  let ring () =
    let ch0 = Event.new_channel ()
    and ch1 = Event.new_channel ()
    and ch2 = Event.new_channel ()
    in
    let rec ea0 =
      lazy (object method role_B =
                object method msg v =
                    Event.sync (Event.send ch0 v);
                    force ea1
                end
            end)
    and ea1 =
      lazy (Event.wrap (Event.receive ch2)
              (fun v -> `role_C(`msg(v, Close))))
    in
    let rec eb0 =
      lazy (Event.wrap (Event.receive ch0)
              (fun v -> `role_A(`msg(v, force eb1))))
    and eb1 =
      lazy (object method role_C =
                object method msg v =
                    Event.sync (Event.send ch1 v);
                    Close
                end
            end)
    in
    let rec ec0 =
      lazy (Event.wrap (Event.receive ch1)
              (fun v -> `role_B(`msg(v, force ec1))))
    and ec1 =
      lazy (object method role_A =
                object method msg v =
                    Event.sync (Event.send ch2 v);
                    Close
                end
            end)
    in
    let ea = force ea0 and eb = force eb0 and ec = force ec0
    in
    lazy (Cons(WrapSend(ea), lazy(Cons(WrapRecv(eb), lazy(Cons(WrapRecv(ec), lazy Nil))))))

end
(* let ring = ChVecExample.ring () *)

let ea = get_ep a ring
and eb = get_ep b ring
and ec = get_ep c ring

let tA = Thread.create (fun () ->
  let ea = ea#role_B#msg () in
  let `role_C(`msg((), ea)) = Event.sync ea in
  print_endline "A done";
  close ea) ()

(* let tA_bad (_:Obj.t) = Thread.create (fun () ->
 *   let `role_C(`msg((), ea)) = Event.sync ea in
 *   let ea = ea#role_B#msg () in
 *   print_endline "A done";
 *   close ea) () *)

let tB = Thread.create (fun () ->
  let `role_A(`msg((), eb)) = Event.sync eb in
  let eb = eb#role_C#msg () in
  print_endline "B done";
  close eb) ()

let tC = Thread.create (fun () ->
  let `role_B(`msg((), ec)) = Event.sync ec in
  let ec = ec#role_A#msg () in
  print_endline "C done";
  close ec) ()

let () = List.iter Thread.join [tA; tB; tC]
