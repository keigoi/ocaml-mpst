type 'a name

val make : unit -> 'a name
val unify_name : 'a name -> 'a name -> unit
val finalise_names : 'a name -> 'a Event.channel
