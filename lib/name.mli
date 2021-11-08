open Concur_shims

type 'a out
type 'a inp
type 'a out_many
type 'a inp_many

val merge_out : 'a out -> 'a out -> 'a out
val merge_inp : 'a inp -> 'a inp -> 'a inp
val send : 'a out -> 'a -> unit IO.io
val receive : 'a inp -> 'a IO.io
val merge_out_many : 'a out_many -> 'a out_many -> 'a out_many
val send_many : 'a out_many -> (int -> 'a) -> unit IO.io
val merge_inp_many : 'a inp_many -> 'a inp_many -> 'a inp_many
val receive_many : 'a inp_many -> 'a IO.io
val create : ('a -> 'b) -> 'a out * 'b inp
val create_out_many : int -> (int -> 'a -> 'b) -> 'a out_many * 'b inp list
val create_inp_many : int -> ('a list -> 'b) -> 'a out list * 'b inp_many
