type -'a out
type +'a inp
type 'a out_ref
type 'a inp_ref

val create_with : wrap_inp:('a -> 'b) -> 'a out_ref * 'b inp_ref
val merge_inp : 'a inp_ref -> 'a inp_ref -> 'a inp_ref
val merge_out : 'a out_ref -> 'a out_ref -> 'a out_ref

val get_out : 'a out_ref -> 'a out
val get_inp : 'a inp_ref -> 'a inp

val send : 'a out -> 'a -> unit Lwt.t
val receive : 'a inp -> 'a Lwt.t
