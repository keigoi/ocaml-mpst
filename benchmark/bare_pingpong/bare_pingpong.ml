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
    create ~name:"ev_bare" @@ run BEventIdeal.runtest;
    create ~name:"ev_bare_cps" @@ (let module M = BEventCPS in run M.runtest);

    create ~name:"lwt_bare" (let module M = BLwtWrap() in run M.runtest);
    create ~name:"lwt_bare_cps" (let module M = BLwtCPS(MpstLwtStream)() in run M.runtest);
  ]

let () =
  Core.Command.run @@
    Core_bench.Bench.make_command
      test_bare_pingpong
