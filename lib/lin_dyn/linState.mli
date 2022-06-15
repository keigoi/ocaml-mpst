type 'a t = 'a Lin.gen PowState.t

val lin_op : 'a State.op -> 'a Lin.lin State.op
val gen_op : 'a State.op -> 'a Lin.gen State.op
val unit : unit Lin.gen PowState.t
