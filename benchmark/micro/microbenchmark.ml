open Core_bench.Bench
open Core_bench.Bench.Test
open Bench_util
open Bench_util.Util

(* array size parameters for ipc payloads *)
let args = array_sizes

let run f = Core.Staged.unstage (f (List.nth array_sizes 0))

open Ocamlmicro

let test_bare_pingpong = [
    create ~name:"ev-OCaml_ideal" @@ run BEvent.runtest;

    (* Here, we compare continuation_passing style vs. channel_vector based communication.
     * For Event module, differences are almost negligible (CPS is around 1 % slower), but apparently CPS allocates
     * more words than two_channel communication.
     *)
    create ~name:"ev-OCaml_cps" @@ run BEventCont.runtest;

    create ~name:"lwt-OCaml_ideal" (let module M = BLwtTwoChan(LwtStream)() in run M.runtest);

    create ~name:"lwt_opt-OCaml_ideal" (let module M = BLwtTwoChan(LwtOptStream)() in run M.runtest);

    (* For Lwt, CPS is slower than two_channel communication (around 5 %)
     * (It seems that Lwt_mvar is the fastest in CPS_style communication _ as for Lwt version 4.2.1.
     *  Note that MVars are 1_bounded; hence- they are not suitable for chvec MPST implementation)
     *)

    create ~name:"lwt-OCaml_cps" (let module M = BLwtCont(LwtStream)() in run M.runtest);
    create ~name:"lwt_opt-OCaml_cps" (let module M = BLwtCont(LwtOptStream)() in run M.runtest);
    create ~name:"lwt_mvar-OCaml_ideal" (let module M = BLwtTwoChan(LwtMVar)() in run M.runtest);
    create ~name:"lwt_bstream-OCaml_ideal" (let module M = BLwtTwoChan(LwtBoundedStream)() in run M.runtest);
    create ~name:"lwt_wake-OCaml_ideal" (let module M = BLwtTwoChan(LwtWait)() in run M.runtest);
    create ~name:"lwt_mvar-OCaml_cps" (let module M = BLwtCont(LwtMVar)() in run M.runtest);
    create ~name:"lwt_bstream-OCaml_cps" (let module M = BLwtCont(LwtBoundedStream)() in run M.runtest);
    create ~name:"lwt_wake-OCaml_cps" (let module M = BLwtCont(LwtWait)() in run M.runtest);

    (* ipc *)
    create_indexed ~args ~name:"lwt_ipc-OCaml_ideal" (let module M = Make_IPC(LwtMonad)() in M.runtest);
    create_indexed ~args ~name:"ipc-OCaml_ideal" (let module M = Make_IPC(Direct)() in M.runtest);

    (* Chcek why it exactly is slow. Closures around endpoints incur a huge cost (~ 20 %) in a tight loop.
     * Nano_mutex does not cause much slow down.  *)
    (* channel vector by hand *)
    create ~name:"lwt_opt-OCaml_overhead" (let module M = BLwtChannelVectorManual(LwtOptStream) in run M.runtest);
    (* less closure- no polyvar wrap (but have object wrap- with extra closures on reception) *)
    create ~name:"lwt_opt-OCaml_less_overhead" (let module M = BLwtChannelVectorManualLessWrap(LwtOptStream) in run M.runtest);
    (* less closure- no polyvar wrap (but have object wrap) *)
    create ~name:"lwt_opt-OCaml_less_overhead1_5" (let module M = BLwtChannelVectorManualLessWrap1_5(LwtOptStream) in run M.runtest);
    (* no object wrap (but have polyvar wrap) *)
    create ~name:"lwt_opt-OCaml_less_overhead2" (let module M = BLwtChannelVectorManualLessWrap2(LwtOptStream) in run M.runtest);
    (* almost same as twochan *)
    create ~name:"lwt_opt-OCaml_less_overhead3" (let module M = BLwtChannelVectorManualLessWrap3(LwtOptStream) in run M.runtest);
  ]

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_bare_pingpong
