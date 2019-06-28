open Mpst.M
open Mpst.M.Base
module ML = Mpst_lwt.M

let fork f x =
  Common.fork_child (fun () ->
      print_endline "process forked";
      f ()) ()

let thread f x =
  ignore (Thread.create (fun () ->
              print_endline "thread started";
              (f x:unit)) ())

(* array size parameters *)
let array_sizes = [1; 100; 1000; 10000; 100000; 1000000]

(* actual array that is passed around threads/processes *)
let big_arrays =
  List.map (fun size ->
      (size, Bigarray.(Array1.create Int16_unsigned C_layout size)))
    array_sizes

let iteration_counts =
  [1; 100; 1000; 10000]
let default_payload = snd @@ List.nth big_arrays 1


module type PERIPHERAL = sig
  include S.MONAD
  val run : 'a t -> 'a
  val is_direct : bool

  module Event : S.EVENT with type 'a monad = 'a t
  module Serial : S.SERIAL with type 'a monad = 'a t
end


module Direct : PERIPHERAL with type 'a t = 'a  = struct
  include P.Pure
  let run x = x
  let is_direct = true
  module Event = P.Event
  module Serial = P.Serial
end
module LwtMonad : PERIPHERAL with type 'a t = 'a Lwt.t = struct
  include ML.P.Lwt
  let run = Lwt_main.run
  let is_direct = false
  module Event = ML.P.LwtEvent
  module Serial = ML.P.LwtSerial
end

module type PERIPHERAL_LIN = sig
  include PERIPHERAL

  module Event : S.EVENT with type 'a monad = 'a t
  module Serial : S.SERIAL with type 'a monad = 'a t
  module Linocaml : Linocaml.S.S with type 'a IO.io = 'a t
end

module LinDirect : PERIPHERAL_LIN with type 'a t = 'a = struct
  include Direct
  module Linocaml = Linocaml.Direct
end

module LinLwtMonad : PERIPHERAL_LIN with type 'a t = 'a Lwt.t = struct
  include LwtMonad
  module Linocaml = Linocaml_lwt
end


module type MEDIUM = sig
  val medium : [`Local | `IPCProcess | `Untyped]
end
module Shmem = struct
  let medium = `Local
end
module IPC = struct
  let medium = `IPCProcess
end
module Untyped = struct
  let medium = `Untyped
end

let ping_or_fini =
  {obj_merge=(fun l r -> object method ping=l#ping method fini=r#fini end);
   obj_splitL=(fun lr -> (lr :> <ping : _>));
   obj_splitR=(fun lr -> (lr :> <fini : _>));
  }
