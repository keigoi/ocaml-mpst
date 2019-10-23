open Bench_util.Util
open Bench_util.Testbase

module Base =  Mpst.M.Base
module Common = Mpst.M.Common

let default_payload = snd @@ List.nth big_arrays 1

module BEventIdeal : TEST = struct
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
        let _ = Event.sync (Event.receive ch_unit) in
        ())
end

module BEventWrap : TEST = struct
  let ch_arr = Event.new_channel ()
  let ch_unit = Event.new_channel ()

  let _ : Thread.t =
    Thread.create (fun () ->
        let rec loop () =
          let `First(_) = Event.sync (Event.wrap (Event.receive ch_arr) (fun x -> `First(x))) in
          Event.sync (Event.send ch_unit ());
          loop ()
        in loop ()) ()

  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage (fun () ->
        Event.sync (Event.send ch_arr payload);
        let `Next(_) = Event.sync (Event.wrap (Event.receive ch_unit) (fun x -> `Next(x))) in
        ())
end

(* module BEventUntyped : TEST = struct
 *   let ch = Event.new_channel ()
 *   let _:Thread.t =
 *     Thread.create (fun () ->
 *         let rec loop () =
 *           let _ = Event.sync (Event.receive ch) in
 *           Event.sync (Event.send ch (Obj.repr ()));
 *           loop ()
 *         in loop ()) ()
 *
 *   let runtest param =
 *     let payload = List.assoc param big_arrays in
 *     Core.Staged.stage (fun () ->
 *         Event.sync (Event.send ch (Obj.repr payload));
 *         let _:Obj.t = Event.sync (Event.receive ch) in
 *         ()
 *       )
 * end *)


module BEventCPS(Flag:Mpst.S.FLAG) : TEST = struct

  let new_channel () =
    let ch = Event.new_channel () in
    (Flag.create (), ch), (Flag.create (), ch)
  let send (lin, ch) v = Flag.use lin; Event.sync (Event.send ch v)
  let receive (lin, ch) = Flag.use lin; Event.sync (Event.receive ch)
  let init_srv,init_cli = new_channel ()

  let _:Thread.t =
    Thread.create (fun () ->
        let rec loop ch =
          let arr_, `First(ch) = receive ch in
          let next,next0 = new_channel () in
          send ch ((),`Next(next));
          loop next0
        in loop init_srv) ()

  let stored = ref init_cli
  let runtest param =
    let payload = List.assoc param big_arrays in
    Core.Staged.stage @@
      fun () ->
      let ch = !stored in
      let next,next0 = new_channel () in
      send ch (payload, `First(next));
      let ((),`Next(ch)) = receive next0 in
      stored := ch
end

module type LWT_CHAN = sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit Lwt.t
  val receive : 'a t -> 'a Lwt.t
end
module MpstLwtStream : LWT_CHAN = struct
  module S = Mpst_lwt.M.Lwt_stream_opt
  type 'a t = 'a S.t
  let create () = S.create ()
  let send t v = S.send t v
  let receive t = S.receive t
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

module BLwtIdeal(Chan:LWT_CHAN)() : TEST = struct
  let (let/) = Lwt.bind

  type 'a seq = Seq of 'a * 'a seq lazy_t

  let ch_arr = Chan.create ()
  let ch_unit = Chan.create ()

  let server_step () =
    fun () ->
    let/ arr_ = Chan.receive ch_arr in
    let/ () = Chan.send ch_unit () in
    Lwt.return_unit

  let client_step () =
    let payload = default_payload in
    fun () ->
    let/ () = Chan.send ch_arr payload in
    let/ () = Chan.receive ch_unit in
    Lwt.return_unit

  let runtest _ =
    let server_step = server_step () in
    let client_step = client_step () in
    Core.Staged.stage (fun () ->
        Lwt.async server_step;
        Lwt_main.run (client_step ()))
end

module BLwtWrap() : TEST = struct
  module M = Mpst_lwt.M.Mstream
  let (let/) = Lwt.bind

  let pingpong _ =
    let ch1 = M.create ~num:1 in
    let ch2 = M.create ~num:1 in
    let rec ch1wrap =
      lazy (M.wrap ch1 (fun x -> `First(x, (Lazy.force s2,Lazy.force s1))))
    and ch2wrap =
      lazy (M.wrap ch2 (fun x -> `Next(x, (Lazy.force c1,Lazy.force c2))))
    and s1 =
      lazy (snd (Lazy.force ch1wrap))
    and s2 =
      lazy (fst (Lazy.force ch2wrap))
    and c1 =
      lazy (fst (Lazy.force ch1wrap))
    and c2 =
      lazy (snd (Lazy.force ch2wrap))
    in
    (Lazy.force s1, (Lazy.force c1, Lazy.force c2))

  let server_step init =
    let stored = ref init in
    fun () ->
    let s1 = !stored in
    let/ `First(arr_,(s2,s1)) = M.receive s1 in
    let/ () = M.send s2 () in
    stored := s1;
    Lwt.return_unit

  let client_step init =
    let stored = ref init in
    let payload = default_payload in
    fun () ->
    let c1,c2 = !stored in
    let/ () = M.send c1 payload in
    let/ `Next((),(c1,c2)) = M.receive c2 in
    stored := (c1,c2);
    Lwt.return_unit

  let runtest _ =
    let s1,c1 = pingpong 1 in
    let server_step = server_step s1 in
    let client_step = client_step c1 in
    Core.Staged.stage (fun () ->
        Lwt.async server_step;
        Lwt_main.run (client_step ()))
end

module BLwtCPS(Chan:LWT_CHAN)(Flag:Mpst.S.FLAG)() : TEST = struct

  let create () =
    let ch = Chan.create () in
    (Flag.create (), ch), (Flag.create (), ch)
  let send (lin, ch) v = Flag.use lin; Chan.send ch v
  let receive (lin, ch) = Flag.use lin; Chan.receive ch

  let (let/) = Lwt.bind

  let server_step init =
    let stored = ref init in
    fun () ->
    let ch = !stored in
    let/ `First(arr_,ch) = receive ch in
    let next,next0 = create () in
    let/ () = send ch (`Next((),next)) in
    stored := next0;
    Lwt.return_unit

  let client_step param init =
    let stored = ref init in
    let payload = List.assoc param big_arrays in
    fun () ->
    let ch = !stored in
    let next,next0 = create () in
    let/ () = send ch (`First(payload,next)) in
    let/ `Next((),ch) = receive next0 in
    stored := ch;
    Lwt.return_unit

  let runtest =
    fun param ->
    let init_srv,init_cli = create () in
    let server_step = server_step init_srv in
    let client_step = client_step param init_cli in
    Core.Staged.stage (fun () ->
        Lwt.async server_step;
        Lwt_main.run (client_step ()))


end

module Make_IPC(M:PERIPHERAL)() : TEST = struct
  module Dpipe = Common.Make_dpipe(M)(M.Serial)
  module C = M.Serial

  let ch = Dpipe.new_dpipe ()

  let (let/) = M.bind

  let () =
    ignore @@ M.Serial.fork_child (fun () ->
        let ch = Dpipe.flip_dpipe ch in
        let rec loop () =
          let/ _ = C.input_value ch.Dpipe.me.inp in
          let/ _ = C.output_value ch.Dpipe.me.out () in
          let/ () = C.flush ch.Dpipe.me.out in
          loop ()
        in M.run (loop ()))

  let runtest param =
    Lwt_io.set_default_buffer_size (max (param*16) default_buffer_size);
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


module BLwtChannelVectorManual() : TEST = struct
  module Chan = Mpst_lwt.M.Mstream
  let (let/) = Lwt.bind

  let ch1 = Chan.create 1
  let ch2 = Chan.create 1
  let raw_receive = Chan.receive
  let raw_send = Chan.send
  (* let ch1 = Lwt_mvar.create_empty ()
   * let ch2 = Lwt_mvar.create_empty ()
   * let receive m = Lwt_mvar.take m
   * let send m v = Lwt_mvar.put m v *)

  let create () =
    let rec ch1wrap =
      lazy (Chan.wrap ch1 (fun x -> `ping(x, Lazy.force epb1)))
    and ch2wrap =
      lazy (Chan.wrap ch2 (fun x -> `pong(x, Lazy.force epa1)))
    and ch1out = lazy (fst (Lazy.force ch1wrap))
    and ch1in = lazy (snd (Lazy.force ch1wrap))
    and ch2out = lazy (fst (Lazy.force ch2wrap))
    and ch2in = lazy (snd (Lazy.force ch2wrap))
    and epa =
      lazy begin
          object method role_B =
              object method ping =
                  (Lazy.force ch1out, Lazy.force epa1)
              end
          end
        end
    and epa1 =
      lazy begin
          object method role_B =
              Lazy.force ch2in
          end
        end
    and epb =
      lazy begin
          object method role_A =
              Lazy.force ch1in
          end
        end
    and epb1 =
      lazy begin
          object method role_A =
              object method pong =
                  (Lazy.force ch2out, Lazy.force epb)
              end
          end
        end
    in
    let _ =
      ignore (Lazy.force epa1);
      ignore (Lazy.force epb1);
      ignore (Lazy.force ch1out);
      ignore (Lazy.force ch1in);
      ignore (Lazy.force ch2out);
      ignore (Lazy.force ch2in);
    in
    (Lazy.force epa, Lazy.force epb)

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont


  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          let/ `ping(_arr, epb) = Chan.receive epb#role_A in
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
            let/ `pong((), epa) = Chan.receive epa#role_B in
            Lwt.return_unit
          end)
end

module BLwtChannelVectorManualNoObjWrap() : TEST = struct
  module Chan = Mpst_lwt.M.Mstream
  let (let/) = Lwt.bind

  let ch1 = Chan.create 1
  let ch2 = Chan.create 1
  let raw_receive = Chan.receive
  let raw_send = Chan.send

  let create () =
    let rec ch1wrap =
      lazy (Chan.wrap ch1 (fun x -> `ping(x, Lazy.force epb1)))
    and ch2wrap =
      lazy (Chan.wrap ch2 (fun x -> `pong(x, Lazy.force epa1)))
    and ch1out = lazy (fst (Lazy.force ch1wrap))
    and ch1in = lazy (snd (Lazy.force ch1wrap))
    and ch2out = lazy (fst (Lazy.force ch2wrap))
    and ch2in = lazy (snd (Lazy.force ch2wrap))
    and epa =
      lazy begin
          (* object method role_B = *)
              (* object method ping = *)
                  (Lazy.force ch1out, Lazy.force epa1)
          (*     end
           * end *)
        end
    and epa1 =
      lazy begin
          (* object method role_B = *)
              Lazy.force ch2in
          (* end *)
        end
    and epb =
      lazy begin
          (* object method role_A = *)
              Lazy.force ch1in
          (* end *)
        end
    and epb1 =
      lazy begin
          (* object method role_A =
           *     object method pong = *)
                  (Lazy.force ch2out, Lazy.force epb)
          (*     end
           * end *)
        end
    in
    let _ =
      ignore (Lazy.force epa1);
      ignore (Lazy.force epb1);
      ignore (Lazy.force ch1out);
      ignore (Lazy.force ch1in);
      ignore (Lazy.force ch2out);
      ignore (Lazy.force ch2in);
    in
    (Lazy.force epa, Lazy.force epb)

  let send (ch,cont) v =
    let/ () = raw_send ch v in
    Lwt.return cont


  let server_iter epb cnt =
    let rec loop epb cnt =
      if cnt = 0 then
        Lwt.return epb
      else begin
          let/ `ping(_arr, epb) = Chan.receive epb in
          let/ epb = send epb () in
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
            let/ epa = send epa payload in
            let/ `pong((), epa) = Chan.receive epa in
            Lwt.return_unit
          end)
end
