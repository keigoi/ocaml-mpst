open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util
open Bare_pingpong_body

(* array size parameters for ipc payloads *)
let args = array_sizes

let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

let test_bare_pingpong = [

    (* Here, we compare continuation_passing style vs. channel_vector based communication.
     * For Event module, differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
     * more words than two_channel communication.
     *)
    create ~name:"ev_bare" @@ run BEvent.runtest;
    create ~name:"ev_bare_cps" @@ run BEventCont.runtest;

    create ~name:"lwt_bare" (let module M = BLwtTwoChan(MpstLwtStream)() in run M.runtest);
    create ~name:"lwt_bare_cps" (let module M = BLwtCont(MpstLwtStream)() in run M.runtest);

    (* Checking the cause of slowdown. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
     * Nano_mutex does not cause much slow down.  *)
    (* channel vector by hand *)
    (* create ~name:"lwt_bare_mimic-mpst" (let module M = BLwtChannelVectorManual(MpstLwtStream) in run M.runtest); *)
    (* less closure: no polyvar wrap (but have object wrap- with extra closures on reception) *)
    (* create ~name:"lwt_bare_less_overhead1" (let module M = BLwtChannelVectorManualLessWrap(MpstLwtStream) in run M.runtest); *)
    (* less closure: no polyvar wrap (but have object wrap) *)
    (* create ~name:"lwt_bare_less_overhead2" (let module M = BLwtChannelVectorManualLessWrap1_5(MpstLwtStream) in run M.runtest); *)
    (* no object wrap (but have polyvar wrap) *)
    (* create ~name:"lwt_bare_less_overhead3" (let module M = BLwtChannelVectorManualLessWrap2(MpstLwtStream) in run M.runtest); *)
    (* almost same as twochan *)
    (* create ~name:"lwt_bare_less_overhead4" (let module M = BLwtChannelVectorManualLessWrap3(MpstLwtStream) in run M.runtest); *)

    (* as we optimised Lwt_stream, let's see the speedup from the original one *)
    (* create ~name:"lwt_orig_bare" (let module M = BLwtTwoChan(LwtStream)() in run M.runtest);
     * create ~name:"lwt_orig_bare_cps" (let module M = BLwtCont(LwtStream)() in run M.runtest); *)

    (* It seems that Lwt_mvar is the fastest in CPS_style communication _ as for Lwt version 4.2.1.
     * Note that MVars are 1_bounded; hence- they are not suitable for chvec MPST implementation
     *)
    (* create ~name:"lwt_mvar_bare" (let module M = BLwtTwoChan(LwtMVar)() in run M.runtest);
     * create ~name:"lwt_mvar_bare_cps" (let module M = BLwtCont(LwtMVar)() in run M.runtest);
     * create ~name:"lwt_bstream_bare" (let module M = BLwtTwoChan(LwtBoundedStream)() in run M.runtest);
     * create ~name:"lwt_bstream_bare_cps" (let module M = BLwtCont(LwtBoundedStream)() in run M.runtest); *)

    (* ipc (almost meaningless) *)
    (* create_indexed ~args ~name:"lwt_ipc-bare_ideal" (let module M = Make_IPC(LwtMonad)() in M.runtest);
     * create_indexed ~args ~name:"ipc-bare_ideal" (let module M = Make_IPC(Direct)() in M.runtest); *)
  ]

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_bare_pingpong
