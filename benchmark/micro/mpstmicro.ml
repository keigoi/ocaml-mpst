open Util

open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M

module MakeDyn
         (M:PERIPHERAL)
         (Med:MEDIUM)
         ()
       :
sig
  val test_msgsize : int -> (unit -> unit) Core.Staged.t
  val test_iteration : int -> (unit -> unit) Core.Staged.t
end
  = struct

  (* module Local = Local.Make(Mpst.M.Nocheck.Nodyncheck)(Mpst.M.Nocheck.Noflag)(M)(M.Event)
   * module Global = Global.Make (Mpst.M.Nocheck.Nodyncheck)(M)(M.Event)(M.Serial)(NoLin)
   * module Util = Util.Make(Mpst.M.Nocheck.Nodyncheck) *)

  (* module DynamicNano = Mpst.M.Dyncheck_ep.Make(Dyncheck_nanomutex.NanoMutexFlag)
   * module Local = Local.Make(DynamicNano)(Dyncheck_nanomutex.NanoMutexFlag)(M)(M.Event)
   * module Global = Global.Make (DynamicNano)(M)(M.Event)(M.Serial)(NoLin)
   * module Util = Util.Make(DynamicNano) *)

  module Local = Local.Make(Mpst.M.Dyncheck)(Mpst.LinFlag)(M)(M.Event)
  module Global = Global.Make(Mpst.M.Dyncheck)(M)(M.Event)(M.Serial)(NoLin)
  module Util = Util.Make(Mpst.M.Dyncheck)

  open Global
  open Local
  open Util

  let prot =
    fix (fun t ->
        (a --> b) ping @@
          (b --> a) pong @@ t)

  let sa, sb =
    let g = gen_with_kinds [Med.medium;Med.medium;] prot in
    Global.get_ep a g, Global.get_ep b g


  let (let/) = M.bind

  let testbody =
    let stored = ref sa in
    fun arr ->
      let sa = !stored in
      let/ sa = send sa#role_B#ping arr in
      let/ `pong((), sa) = receive sa#role_B in
      stored := sa;
      M.return_unit

  let server_step =
    let stored = ref sb
    in
    fun () ->
      let sb = !stored in
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      stored := sb;
      M.return_unit

  let server_loop sb =
    let rec loop sb =
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      loop sb
    in
    loop sb

  let test_msgsize i =
    let arr = List.assoc i big_arrays in
    Core.Staged.stage
    @@ fun () ->
       if Med.medium <> `IPCProcess && not M.is_direct then begin
           M.async server_step
         end;
       M.run (testbody arr)

  (* let server_step sb =
   *   let/ `ping(_,sb) = receive sb#role_A in
   *   let/ sb = send sb#role_A#pong () in
   *   M.return sb
   *
   * let test_iter cnt =
   *   if
   *   Core.Staged.stage @@
   *     fun () ->
   *     i *)


  let server_iter =
    let stored = ref sb in
    fun cnt ->
    let sb = !stored in
    let rec loop sb cnt =
      if cnt = 0 then
        M.return sb
      else begin
          let/ `ping(_,sb) = receive sb#role_A in
          let/ sb = send sb#role_A#pong () in
          (loop[@tailcall]) sb (cnt-1)
        end
    in
    let/ sb = loop sb cnt in
    stored := sb;
    M.return_unit


  let test_iter_body =
    let stored = ref sa in
    fun cnt ->
    let sa = !stored in
    let rec loop sa cnt =
      if cnt = 0 then
        M.return sa
      else begin
          let/ sa = send sa#role_B#ping default_payload in
          let/ `pong((),sa) = receive sa#role_B in
          (loop[@tailcall]) sa (cnt-1)
        end
    in
    let/ sa = loop sa cnt in
    stored := sa;
    M.return_unit


  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        if Med.medium <> `IPCProcess && not M.is_direct then begin
            M.async (fun () -> server_iter cnt)
          end;
        M.run (test_iter_body cnt))

  (* let prot_n cnt =
   *   let rec loop n t =
   *     if n = 0 then
   *       t
   *     else
   *       (a --> b) ping @@ (b --> a) pong @@ loop (n-1) t
   *   in
   *   fix (fun t -> loop cnt t)
   *
   * let test_statecoutn cnt =
   *     fun () ->
   *     let g = gen (prot_n cnt) in
   *     Core.Staged.stage @@
   *       fun () -> *)




  let () =
    if Med.medium = `IPCProcess then begin
        fork (fun () -> M.run (server_loop sb)) ();
      end else if M.is_direct then begin
        ignore (thread (fun () -> M.run (server_loop sb)) ());
      end

end

module MakeStatic
         (M:PERIPHERAL_LIN)
         (Med:MEDIUM)
         ()
       :
sig
  val test_msgsize : int -> (unit -> unit) Core.Staged.t
  val test_iteration : int -> (unit -> unit) Core.Staged.t
end
  = struct
  module Local = Mpst_monad.Local_monad.Make(M)(M.Event)(M.Linocaml)
  module Global = Mpst_monad.Global_monad.Make(M)(M.Event)(M.Serial)(M.Linocaml)
  module Util = Util.Make(Mpst.M.Nocheck.Nodyncheck)
  open Global
  open Local
  open Util

  let prot =
    fix (fun t ->
        (a --> b) ping @@
          (b --> a) pong @@ t)

  let sa, sb =
    let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
    Global.raw_get_ep a g, Global.raw_get_ep b g


  let (let/) = M.bind

  let s = Linocaml.Zero

  let testbody =
    let stored = ref sa in
    let open M.Linocaml in
    fun arr ->
    put_linval s !stored >>= fun () ->
    let%lin #s = s <@ send (fun x->x#role_B#ping) arr in
    let%lin `pong({Linocaml.data=()}, #s) = s <@ receive (fun x -> x#role_B) in
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}

  let server_step =
    let stored = ref sb
    in
    fun () ->
    let sb = !stored in
    let open M.Linocaml in
    put_linval s sb >>= fun () ->
    let%lin `ping(_,#s) = s <@ receive (fun x->x#role_A) in
    let%lin #s = s <@ send (fun x-> x#role_A#pong) () in
    stored := sb;
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}

  let server_loop =
    let open M.Linocaml in
    let stored = ref sb in
    fun cnt ->
    let rec loop cnt =
      if cnt=Some 0 then begin
          return ()
        end else begin
          let%lin `ping(_,#s) = s <@ receive (fun x ->x#role_A) in
          let%lin #s = s <@ send (fun x -> x#role_A#pong) () in
          loop (Mpst.M.Common.map_option (fun x->x-1) cnt)
        end
    in
    let sb = !stored in
    put_linval s sb >>= fun () ->
    loop cnt >>= fun () ->
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).Linocaml.__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}

  let test_msgsize i =
    let arr = List.assoc i big_arrays in
    Core.Staged.stage (fun () ->
        if Med.medium <> `IPCProcess && not M.is_direct then begin
            M.async (M.Linocaml.run' server_step)
          end;
        M.run (M.Linocaml.run' testbody arr))

  let test_iter_body =
    let open M.Linocaml in
    let stored = ref sa in
    fun cnt ->
    let rec loop cnt =
      if cnt = 0 then
        return ()
      else begin
          let%lin #s = s <@ send (fun x->x#role_B#ping) default_payload in
          let%lin `pong({Linocaml.data=()},#s) = s <@ receive (fun x->x#role_B) in
          (loop[@tailcall]) (cnt-1)
        end
    in
    put_linval s (!stored) >>= fun () ->
    loop cnt >>= fun () ->
    {__m=(fun pre ->
       stored := (Linocaml.lens_get s pre).__lin;
       M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})
    )}


  let test_iteration cnt =
    Core.Staged.stage (fun () ->
        if Med.medium <> `IPCProcess && not M.is_direct then begin
            M.async (fun () -> M.Linocaml.run' server_loop (Some cnt))
          end;
        M.run (M.Linocaml.run' test_iter_body cnt)
      )


  let () =
    if Med.medium = `IPCProcess then begin
        fork (fun () -> M.run (M.Linocaml.run' server_loop None)) ();
      end else if M.is_direct then begin
        ignore (thread (fun () -> M.run (M.Linocaml.run' server_loop None)) ());
      end

end

module BRefImpl
       :
sig
  val test_msgsize : int -> (unit -> unit) Core.Staged.t
  val test_iteration : int -> (unit -> unit) Core.Staged.t
end
  = struct

  open Mpst_ref

  let prot =
    fix (fun t ->
        (a --> b) ping @@
          (b --> a) pong @@ t)

  let sa, sb =
    let g = gen prot in
    Global.get_ep a g, Global.get_ep b g


  let (let/) m f = f m

  let testbody =
    let stored = ref sa in
    fun arr ->
    let sa = !stored in
    let/ sa = send sa#role_B#ping arr in
    let/ `pong((), sa) = receive sa#role_B in
    stored := sa;
    ()

  let server_loop sb =
    let rec loop sb =
      let/ `ping(_,sb) = receive sb#role_A in
      let/ sb = send sb#role_A#pong () in
      loop sb
    in
    loop sb

  let test_msgsize i =
    Core.Staged.stage (fun () ->
        let arr = List.assoc i big_arrays in
        testbody arr
      )

  let test_iteration =
    let stored = ref sa in
    fun cnt ->
    Core.Staged.stage (fun () ->
        let sa = !stored in
        let rec loop sa cnt =
          if cnt=0 then
            sa
          else
            let/ sa = send sa#role_B#ping default_payload in
            let/ `pong((), sa) = receive sa#role_B in
            loop sa (cnt-1)
        in
        let sa = loop sa (cnt) in
        stored := sa
      )

  let () =
    ignore (Thread.create (fun () -> server_loop sb) ());
end
