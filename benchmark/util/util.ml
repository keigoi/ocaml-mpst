(* open Mpst.M *)

let thread f x =
  ignore (Thread.create (fun () ->
              (f x:unit)) ())

(* let array_sizes = [1; 10; 100; 1000; 10000; 100000] *)
let array_sizes = [1; 10; 100; 1000; 5000; 10000]

(* actual array that is passed around threads/processes *)
let big_arrays =
  List.map (fun size ->
      (size, Bigarray.(Array1.create Int16_unsigned C_layout size)))
    array_sizes

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
