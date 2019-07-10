open Util
open Testbase

module Base =  Mpst.M.Base
module Common = Mpst.M.Common

module BEvent : TEST = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()

  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch_arr) in
          Event.sync (Event.send ch_unit ());
          loop ()
        in loop ()) ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        Event.sync (Event.send ch_arr payload);
        Event.sync (Event.receive ch_unit))
end

module BEventUntyped : TEST = struct
  let ch = Event.new_channel ()
  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch) in
          Event.sync (Event.send ch (Obj.repr ()));
          loop ()
        in loop ()) ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        Event.sync (Event.send ch (Obj.repr payload));
        let _:Obj.t = Event.sync (Event.receive ch) in
        ()
      )
end

module BEventCont : TEST = struct
  let init_ch = Event.new_channel ()

  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop ch =
          let arr_, `Cont(ch) = Event.sync (Event.receive ch) in
          let next = Event.new_channel () in
          Event.sync (Event.send ch ((),`Cont(next)));
          loop next
        in loop init_ch) ()

  let stored = ref init_ch
  let runtest param =
    let payload = List.assoc param big_arrays in 
    Core.Staged.stage @@
      fun () ->
      let ch = !stored in
      let next = Event.new_channel () in
      Event.sync (Event.send ch (payload, `Cont(next)));
      let ((),`Cont(ch)) = Event.sync (Event.receive next) in
      stored := ch
end

module type LWT_CHAN = sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit Lwt.t
  val receive : 'a t -> 'a Lwt.t
end
module LwtBoundedStream : LWT_CHAN = struct
  type 'a t = 'a Lwt_stream.t * 'a Lwt_stream.bounded_push
  let create () = Lwt_stream.create_bounded 1
  let send (_,wr) v = wr#push v
  let receive (st,_) = Lwt_stream.next st
end
module LwtStream : LWT_CHAN = struct
  type 'a t = 'a Lwt_stream.t * ('a option -> unit)
  let create () = Lwt_stream.create ()
  let send (_,wr) v = wr (Some v); Lwt.return_unit
  let receive (st,_) = Lwt_stream.next st
end
module LwtMVar : LWT_CHAN = struct
  type 'a t = 'a Lwt_mvar.t
  let create () = Lwt_mvar.create_empty ()
  let send m v = Lwt_mvar.put m v
  let receive m = Lwt_mvar.take m
end
module LwtWait : LWT_CHAN = struct
  type 'a t = 'a Lwt.t * 'a Lwt.u
  let create () = Lwt.wait ()
  let send (t_,u) v = Lwt.wakeup_later u v; Lwt.return_unit
  let receive (t,u_) = t
end

module BLwtTwoChan(Chan:LWT_CHAN)() : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Chan.create ()
  let ch2 = Chan.create ()
  (* let receive (st,_) = Lwt_stream.next st
   * let send (_,push) v = push (Some v); Lwt.return_unit *)
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let server_step () =
    let/ arr_ = Chan.receive ch1 in
    Chan.send ch2 ()

  let server_iter cnt =
    let rec loop cnt =
      if cnt = 0 then
        Lwt.return_unit
      else begin
          let/ arr_ = Chan.receive ch1 in
          let/ () = Chan.send ch2 () in
          loop (cnt-1)
        end
    in
    loop cnt

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter 1);
        Lwt_main.run begin
            let/ () = Chan.send ch1 payload in
            Chan.receive ch2
          end)
end

module BLwtCont(Chan:LWT_CHAN)() : TEST = struct
  open Chan
  let (let/) = Lwt.bind

  (* let init = create () *)

  let server_loop init =
    let stored = ref init in
    fun cnt ->
    let rec loop ch cnt =
      if cnt=0 then
        Lwt.return ch
      else
        let/ arr_,ch = receive ch in
        let next = create () in
        let/ () = send ch (`Next((),next)) in
        loop next (cnt-1)
    in
    let/ ch = loop !stored cnt in
    stored := ch;
    Lwt.return_unit

  let iteration_body init =
    let stored = ref init in
    fun param ->
    let ch = !stored in
    let payload = List.assoc param big_arrays in
    let next = create () in
    let/ () = send ch (payload,next) in
    let/ `Next((),ch) = receive next in
    stored := ch;
    Lwt.return_unit

  let runtest =
    fun param ->
    let init = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_loop init 1);
        Lwt_main.run begin
            iteration_body init param
          end)


  let init_st, init_push = Lwt_stream.create ()

  let server_step =
    let stored = ref (init_st, init_push) in
    fun () ->
    let st, _ = !stored in
    let/ (arr_,(_,push)) = Lwt_stream.next st in
    let next = Lwt_stream.create () in
    stored := next;
    push (Some((),`Cont(next)));
    Lwt.return_unit

end

module Make_IPC(M:PERIPHERAL)() : TEST = struct
  module Dpipe = Common.Make_dpipe(M.Serial)
  module C = M.Serial

  let ch = Dpipe.new_dpipe ()

  let (let/) = M.bind

  let () =
    fork (fun () ->
        let ch = Dpipe.flip_dpipe ch in
        let rec loop () =
          let/ _ = C.input_value ch.Dpipe.me.inp in
          let/ _ = C.output_value ch.Dpipe.me.out () in
          let/ () = C.flush ch.Dpipe.me.out in
          loop ()
        in M.run (loop ())) ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage @@
      fun () ->
      M.run begin
          let/ () = C.output_value ch.Dpipe.me.out payload in
          let/ () = C.flush ch.Dpipe.me.out in
          let/ () = C.input_value ch.Dpipe.me.inp in
          M.return_unit
        end

end


module BLwtChannelVectorManual : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Lwt_stream.create ()
  let ch2 = Lwt_stream.create ()
  let raw_receive (st,_) = Lwt_stream.next st
  let raw_send (_,push) v = push (Some v); Lwt.return_unit
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec epa =
      lazy begin
          object method role_B =
              object method ping =
                  (ch1, Lazy.force epa1)
              end
          end
        end
    and epa1 =
      lazy begin
          object method role_B =
              fun () -> Lwt.map (fun v -> `pong(v, Lazy.force epa)) (raw_receive ch2)
          end
        end
    in
    let rec epb =
      lazy begin
          object method role_A =
              fun () -> Lwt.map (fun v -> `ping(v, Lazy.force epb1)) (raw_receive ch1)
          end
        end
    and epb1 =
      lazy begin
          object method role_A =
              object method pong =
                  (ch2, Lazy.force epb)
              end
          end
        end
    in
    Lazy.force epa, Lazy.force epb

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont

  let receive ep = ep ()



  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          let/ `ping(_arr, epb) = receive epb#role_A in
          let/ epb = send epb#role_A#pong () in
          loop epb (cnt-1)
        end
    in
    loop epb cnt

  let runtest =
    fun param ->
    let payload = List.assoc param big_arrays in
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb 1);
        Lwt_main.run begin
            let/ epa = send epa#role_B#ping payload in
            let/ `pong((), epa) = receive epa#role_B in
            Lwt.return_unit
          end)
end

module BLwtChannelVectorManualDyncheckClosure : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Lwt_stream.create ()
  let ch2 = Lwt_stream.create ()
  let raw_receive (st,_) = Lwt_stream.next st
  let raw_send (_,push) v = push (Some v); Lwt.return_unit
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec epa () =
          object method role_B =
              object method ping =
                  (ch1, epa1 ())
              end
          end
    and epa1 () =
          object method role_B =
              fun () -> Lwt.map (fun v -> `pong(v, epa ())) (raw_receive ch2)
          end
    in
    let rec epb () =
          object method role_A =
              fun () -> Lwt.map (fun v -> `ping(v, epb1 ())) (raw_receive ch1)
          end
    and epb1 () =
          object method role_A =
              object method pong =
                  (ch2, epb ())
              end
          end
    in
    epa (), epb ()

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont

  let receive ep = ep ()



  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          let/ `ping(_arr, epb) = receive epb#role_A in
          let/ epb = send epb#role_A#pong () in
          loop epb (cnt-1)
        end
    in
    loop epb cnt

  let runtest =
    fun param ->
    let payload = List.assoc param big_arrays in
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb 1);
        Lwt_main.run begin
            let/ epa = send epa#role_B#ping payload in
            let/ `pong((), epa) = receive epa#role_B in
            Lwt.return_unit
          end)
end

module BLwtChannelVectorManualLessWrap : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Lwt_stream.create ()
  let ch2 = Lwt_stream.create ()
  let raw_receive (st,_) = Lwt_stream.next st
  let raw_send (_,push) v = push (Some v); Lwt.return_unit
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec epa =
      lazy begin
          object method role_B =
              object method ping =
                  (ch1, Lazy.force epa1)
              end
          end
        end
    and epa1 =
      lazy begin
          object method role_B =
              (* fun () -> Lwt.map (fun v -> `pong(v, Lazy.force epa)) (raw_receive ch2) *)
              fun () -> raw_receive ch2, Lazy.force epa
              (* raw_receive ch2, Lazy.force epa *)
          end
        end
    in
    let rec epb =
      lazy begin
          object method role_A =
              (* fun () -> Lwt.map (fun v -> `ping(v, Lazy.force epb1)) (raw_receive ch1) *)
              fun () -> raw_receive ch1, Lazy.force epb1
              (* raw_receive ch1, Lazy.force epb1 *)
          end
        end
    and epb1 =
      lazy begin
          object method role_A =
              object method pong =
                  (ch2, Lazy.force epb)
              end
          end
        end
    in
    (* let _: unit -> _ = Lazy.force epa1 in
     * ignore (Lazy.force epb1); *)
    Lazy.force epa, Lazy.force epb

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont

  let receive ep = ep ()

  (* let receive ep = ep *)

  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          let inp, epb =  receive epb#role_A in
          (* let inp, epb =  receive epb in *)
          let/ arr_ =  inp in
          (* let/ `ping(_arr, epb) = receive epb#role_A in *)
          (* let/ epb = send epb () in *)
          let/ epb = send epb#role_A#pong () in
          loop epb (cnt-1)
        end
    in
    loop epb cnt

  let runtest =
    fun param ->
    let payload = List.assoc param big_arrays in
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb 1);
        Lwt_main.run begin
            let/ epa = send epa#role_B#ping payload in
            (* let/ epa = send epa default_payload in *)
            (* let/ `pong((), epa) = receive epa#role_B in *)
            (* let inp, epa = receive epa in *)
            let inp, epa = receive epa#role_B in
            let/ () = inp in
            Lwt.return_unit
          end)
end


module BLwtChannelVectorManualLessWrap1_5 : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Lwt_stream.create ()
  let ch2 = Lwt_stream.create ()
  let raw_receive (st,_) = Lwt_stream.next st
  let raw_send (_,push) v = push (Some v); Lwt.return_unit
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec epa =
      lazy begin
          object method role_B =
              object method ping =
                  (ch1, Lazy.force epa1)
              end
          end
        end
    and epa1 =
      lazy begin
          object method role_B =
              (* fun () -> Lwt.map (fun v -> `pong(v, Lazy.force epa)) (raw_receive ch2) *)
              (* fun () -> raw_receive ch2, Lazy.force epa *)
              raw_receive ch2, Lazy.force epa
          end
        end
    in
    let rec epb =
      lazy begin
          object method role_A =
              (* fun () -> Lwt.map (fun v -> `ping(v, Lazy.force epb1)) (raw_receive ch1) *)
              (* fun () -> raw_receive ch1, Lazy.force epb1 *)
              raw_receive ch1, Lazy.force epb1
          end
        end
    and epb1 =
      lazy begin
          object method role_A =
              object method pong =
                  (ch2, Lazy.force epb)
              end
          end
        end
    in
    (* let _: unit -> _ = Lazy.force epa1 in
     * ignore (Lazy.force epb1); *)
    Lazy.force epa, Lazy.force epb

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont

  (* let receive ep = ep () *)

  let receive ep = ep

  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          let inp, epb =  receive epb#role_A in
          (* let inp, epb =  receive epb in *)
          let/ arr_ =  inp in
          (* let/ `ping(_arr, epb) = receive epb#role_A in *)
          (* let/ epb = send epb () in *)
          let/ epb = send epb#role_A#pong () in
          loop epb (cnt-1)
        end
    in
    loop epb cnt

  let runtest =
    fun param ->
    let payload = List.assoc param big_arrays in
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb 1);
        Lwt_main.run begin
            let/ epa = send epa#role_B#ping payload in
            (* let/ epa = send epa default_payload in *)
            (* let/ `pong((), epa) = receive epa#role_B in *)
            (* let inp, epa = receive epa in *)
            let inp, epa = receive epa#role_B in
            inp
          end)
end

module BLwtChannelVectorManualLessWrap2 : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Lwt_stream.create ()
  let ch2 = Lwt_stream.create ()
  let raw_receive (st,_) = Lwt_stream.next st
  let raw_send (_,push) v = push (Some v); Lwt.return_unit
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec epa =
      lazy begin
                  (ch1, Lazy.force epa1)
        end
    and epa1 =
      lazy begin
              fun () -> Lwt.map (fun v -> `pong(v, Lazy.force epa)) (raw_receive ch2)
              (* fun () -> raw_receive ch2, Lazy.force epa *)
              (* raw_receive ch2, Lazy.force epa *)
        end
    in
    let rec epb =
      lazy begin
              fun () -> Lwt.map (fun v -> `ping(v, Lazy.force epb1)) (raw_receive ch1)
              (* fun () -> raw_receive ch1, Lazy.force epb1 *)
              (* raw_receive ch1, Lazy.force epb1 *)
        end
    and epb1 =
      lazy begin
                  (ch2, Lazy.force epb)
        end
    in
    (* let _: unit -> _ = Lazy.force epa1 in
     * ignore (Lazy.force epb1); *)
    Lazy.force epa, Lazy.force epb

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont

  let receive ep = ep ()

  (* let receive ep = ep *)

  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          (* let inp, epb =  receive epb#role_A in *)
          (* let inp, epb =  receive epb in *)
          (* let/ arr_ =  inp in *)
          (* let/ `ping(_arr, epb) = receive epb#role_A in *)
          let/ `ping(_arr, epb) = receive epb in
          let/ epb = send epb () in
          (* let/ epb = send epb#role_A#pong () in *)
          loop epb (cnt-1)
        end
    in
    loop epb cnt

  let runtest =
    fun param ->
    let payload = List.assoc param big_arrays in
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb 1);
        Lwt_main.run begin
            (* let/ epa = send epa#role_B#ping default_payload in *)
            let/ epa = send epa payload in
            let/ `pong((), epa) = receive epa in
            (* let/ `pong((), epa) = receive epa#role_B in *)
            (* let inp, epa = receive epa in *)
            (* let inp, epa = receive epa#role_B in *)
            (* let/ () = inp in *)
            Lwt.return_unit
          end)
end

module BLwtChannelVectorManualLessWrap3 : TEST = struct
  let (let/) = Lwt.bind

  let ch1 = Lwt_stream.create ()
  let ch2 = Lwt_stream.create ()
  let raw_receive (st,_) = Lwt_stream.next st
  let raw_send (_,push) v = push (Some v); Lwt.return_unit
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec epa =
      lazy begin
                  (ch1, Lazy.force epa1)
        end
    and epa1 =
      lazy begin
              (* fun () -> Lwt.map (fun v -> `pong(v, Lazy.force epa)) (raw_receive ch2) *)
              fun () -> raw_receive ch2, Lazy.force epa
              (* raw_receive ch2, Lazy.force epa *)
        end
    in
    let rec epb =
      lazy begin
              (* fun () -> Lwt.map (fun v -> `ping(v, Lazy.force epb1)) (raw_receive ch1) *)
              fun () -> raw_receive ch1, Lazy.force epb1
              (* raw_receive ch1, Lazy.force epb1 *)
        end
    and epb1 =
      lazy begin
                  (ch2, Lazy.force epb)
        end
    in
    (* let _: unit -> _ = Lazy.force epa1 in
     * ignore (Lazy.force epb1); *)
    Lazy.force epa, Lazy.force epb

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont

  let receive ep = ep ()

  (* let receive ep = ep *)

  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          (* let inp, epb =  receive epb#role_A in *)
          let inp, epb =  receive epb in
          let/ arr_ =  inp in
          (* let/ `ping(_arr, epb) = receive epb#role_A in *)
          (* let/ `ping(_arr, epb) = receive epb in *)
          let/ epb = send epb () in
          (* let/ epb = send epb#role_A#pong () in *)
          loop epb (cnt-1)
        end
    in
    loop epb cnt

  let runtest =
    fun param ->
    let payload = List.assoc param big_arrays in
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb 1);
        Lwt_main.run begin
            (* let/ epa = send epa#role_B#ping default_payload in *)
            let/ epa = send epa payload in
            (* let/ `pong((), epa) = receive epa in *)
            (* let/ `pong((), epa) = receive epa#role_B in *)
            let inp, epa = receive epa in
            (* let inp, epa = receive epa#role_B in *)
            inp
          end)
end
