open Concur_shims
module type LOCAL = sig
  type 'a out
  type 'a inp
  type 'a scatter
  type 'a gather

  val merge_out : 'a out -> 'a out -> 'a out
  val merge_inp : 'a inp -> 'a inp -> 'a inp

  val send : 'a out -> 'a -> unit IO.io
  val receive : 'a inp -> 'a IO.io

  val merge_scatter : 'a scatter -> 'a scatter -> 'a scatter
  val send_many : 'a scatter -> (int -> 'a) -> unit IO.io

  val merge_gather : 'a gather -> 'a gather -> 'a gather
  val receive_many : 'a gather -> 'a IO.io
end
