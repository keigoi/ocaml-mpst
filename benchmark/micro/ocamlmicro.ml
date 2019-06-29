open Util
module Base =  Mpst.M.Base
module Common = Mpst.M.Common

module BEvent = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()
  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch_arr) in
          Event.sync (Event.send ch_unit ());
          loop ()
        in loop ()) ()

  let test_msgsize =
    fun i ->
    Core.Staged.stage (fun () ->
        let arr = List.assoc i big_arrays in
        Event.sync (Event.send ch_arr arr);
        Event.sync (Event.receive ch_unit))

  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        let rec loop cnt =
          if cnt = 0 then
            ()
          else begin
              Event.sync (Event.send ch_arr default_payload);
              Event.sync (Event.receive ch_unit);
              loop (cnt-1)
            end
        in loop cnt)
end

module BEventUntyped = struct
  let ch = Event.new_channel ()
  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let _ = Event.sync (Event.receive ch) in
          Event.sync (Event.send ch (Obj.repr ()));
          loop ()
        in loop ()) ()

  let test_msgsize i =
    Core.Staged.stage (fun () ->
        let arr = List.assoc i big_arrays in
        Event.sync (Event.send ch (Obj.repr arr));
        Event.sync (Event.receive ch))

  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        let rec loop cnt =
          if cnt=0 then
            ()
          else begin
              Event.sync (Event.send ch (Obj.repr default_payload));
              let _:Obj.t = Event.sync (Event.receive ch) in
              loop (cnt-1)
            end
        in loop cnt
      )
end

module BEventCont = struct
  let init_ch = Event.new_channel ()

  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop ch =
          let arr_, `Cont(ch) = Event.sync (Event.receive ch) in
          let next = Event.new_channel () in
          Event.sync (Event.send ch ((),`Cont(next)));
          loop next
        in loop init_ch) ()

  let test_msgsize =
    let stored = ref init_ch in
    fun i ->
    Core.Staged.stage (fun () ->
        let ch = !stored in
        let arr = List.assoc i big_arrays in
        let next = Event.new_channel () in
        Event.sync (Event.send ch (arr, `Cont(next)));
        let ((),`Cont(ch)) = Event.sync (Event.receive next) in
        stored := ch;
        ())

  let stored = ref init_ch
  let test_iteration cnt =
    Core.Staged.stage @@
      fun () ->
      let rec loop ch cnt =
        if cnt=0 then
          ch
        else begin
            let next = Event.new_channel () in
            Event.sync (Event.send ch (default_payload, `Cont(next)));
            let ((),`Cont(ch)) = Event.sync (Event.receive next) in
            loop ch (cnt-1)
          end
      in
      let ch = !stored in
      let ch = loop ch cnt in
      stored := ch;
      ()
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

module BLwtTwoChan(Chan:LWT_CHAN)() = struct
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

  let test_msgsize =
    fun i ->
    Core.Staged.stage (fun () ->
        Lwt.async server_step;
        Lwt_main.run begin
            let arr = List.assoc i big_arrays in
            let/ () = Chan.send ch1 arr in
            Chan.receive ch2
          end)


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

  let test_iteration =
    fun cnt ->
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter cnt);
        Lwt_main.run begin
            let rec loop cnt =
              if cnt=0 then
                Lwt.return_unit
              else
                let/ () = Chan.send ch1 default_payload in
                let/ () = Chan.receive ch2 in
                loop (cnt-1)
            in
            loop cnt
          end)
end

module BLwtCont(Chan:LWT_CHAN)() = struct
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
    fun cnt ->
    let rec loop ch cnt =
      if cnt=0 then
        Lwt.return ch
      else begin
          let next = create () in
          let/ () = send ch (default_payload,next) in
          let/ `Next((),ch) = receive next in
          loop ch (cnt-1)
        end
    in
    let/ ch = loop !stored cnt in
    stored := ch;
    Lwt.return_unit

  let test_iteration =
    fun cnt ->
    let init = create () in
    let server_loop = server_loop init in
    let iteration_body = iteration_body init in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_loop cnt);
        Lwt_main.run begin
            iteration_body cnt
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

  let test_msgsize =
    let stored = ref (init_st, init_push) in
    fun i ->
    Core.Staged.stage @@
      fun () ->
      let _, push = !stored in
      Lwt.async server_step;
      Lwt_main.run begin
          let arr = List.assoc i big_arrays in
          let (st,_) as next = Lwt_stream.create () in
          push (Some (arr, next));
          let/ ((),`Cont(next)) = Lwt_stream.next st in
          stored := next;
          Lwt.return_unit
        end
end

module Make_IPC(M:PERIPHERAL)() = struct
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

  let test_msgsize i =
    Core.Staged.stage @@
      fun () ->
      M.run begin
          let arr = List.assoc i big_arrays in
          let/ () = C.output_value ch.Dpipe.me.out arr in
          let/ () = C.flush ch.Dpipe.me.out in
          C.input_value ch.Dpipe.me.inp
      end

  let test_iteration cnt =
    Core.Staged.stage @@
      fun () ->
      M.run begin
          let rec loop cnt =
            if cnt=0 then
              M.return_unit
            else
              let/ () = C.output_value ch.Dpipe.me.out default_payload in
              let/ () = C.flush ch.Dpipe.me.out in
              let/ () = C.input_value ch.Dpipe.me.inp in
              loop (cnt-1)
          in
          loop cnt
        end

end


module BLwtChannelVectorManual = struct
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

  let test_iteration =
    fun cnt ->
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb cnt);
        Lwt_main.run begin
            let rec loop epa cnt =
              if cnt=0 then
                Lwt.return epa
              else
                let/ epa = send epa#role_B#ping default_payload in
                let/ `pong((), epa) = receive epa#role_B in
                loop epa (cnt-1)
            in
            loop epa cnt
          end)
end

module BLwtChannelVectorManualLessWrap = struct
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

  let test_iteration =
    fun cnt ->
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb cnt);
        Lwt_main.run begin
            let rec loop epa cnt =
              if cnt=0 then
                Lwt.return epa
              else
                let/ epa = send epa#role_B#ping default_payload in
                (* let/ epa = send epa default_payload in *)
                (* let/ `pong((), epa) = receive epa#role_B in *)
                (* let inp, epa = receive epa in *)
                let inp, epa = receive epa#role_B in
                let/ () = inp in
                loop epa (cnt-1)
            in
            loop epa cnt
          end)
end

module BLwtChannelVectorManualLessWrap2 = struct
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

  let test_iteration =
    fun cnt ->
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb cnt);
        Lwt_main.run begin
            let rec loop epa cnt =
              if cnt=0 then
                Lwt.return epa
              else
                (* let/ epa = send epa#role_B#ping default_payload in *)
                let/ epa = send epa default_payload in
                let/ `pong((), epa) = receive epa in
                (* let/ `pong((), epa) = receive epa#role_B in *)
                (* let inp, epa = receive epa in *)
                (* let inp, epa = receive epa#role_B in *)
                (* let/ () = inp in *)
                loop epa (cnt-1)
            in
            loop epa cnt
          end)
end

module BLwtChannelVectorManualLessWrap3 = struct
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

  let test_iteration =
    fun cnt ->
    let epa, epb = create () in
    Core.Staged.stage (fun () ->
        Lwt.async (fun () -> server_iter epb cnt);
        Lwt_main.run begin
            let rec loop epa cnt =
              if cnt=0 then
                Lwt.return epa
              else
                (* let/ epa = send epa#role_B#ping default_payload in *)
                let/ epa = send epa default_payload in
                (* let/ `pong((), epa) = receive epa in *)
                (* let/ `pong((), epa) = receive epa#role_B in *)
                let inp, epa = receive epa in
                (* let inp, epa = receive epa#role_B in *)
                let/ () = inp in
                loop epa (cnt-1)
            in
            loop epa cnt
          end)
end
