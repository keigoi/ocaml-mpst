module Context = State.Context

type _ extchoice_item

val determinise : State.context -> 'a extchoice_item -> 'a extchoice_item
val to_string : State.context -> 'a extchoice_item -> string
val force : State.context -> 'a extchoice_item -> unit
val make : ('var, 't) Rows.constr -> 't State.t -> 'var extchoice_item

val merge_items :
  State.context ->
  'a extchoice_item list ->
  'a extchoice_item list ->
  'a extchoice_item list

val match_item : 'var extchoice_item -> int * 'var