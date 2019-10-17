
module type MONAD = sig
  type +'a t
  val return : 'a -> 'a t
  val return_unit : unit t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val iteriM : (int -> 'a -> unit t) -> 'a list -> unit t
  val mapM : ('a -> 'b t) -> 'a list -> 'b list t
  val async : (unit -> 'a t) -> unit
end

module type EVENT = sig
  type 'a event
  type 'a channel
  val new_channel : unit -> 'a channel
  val flip_channel : 'a channel -> 'a channel
  val receive : 'a channel -> 'a event
  val send : 'a channel -> 'a -> unit event
  val guard : (unit -> 'a event) -> 'a event
  val always : 'a -> 'a event
  val receive_list : 'a channel list -> 'a list event

  (* needs refactoring *)
  type 'a inp
  val inp : 'a channel -> 'a inp
  val receive_inp : 'a inp -> 'a event
  val merge_inp : 'a inp -> 'a inp -> 'a inp
  val wrap_inp : 'a inp -> ('a -> 'b) -> 'b inp

  type +'a monad
  val sync : 'a event -> 'a monad
end

module type SERIAL = sig
  type +'a monad
  type out_channel
  type in_channel
  val output_tagged : out_channel -> Base.tag * Obj.t -> unit monad
  val output_value : out_channel -> 'v -> unit monad
  val input_tagged : in_channel -> (Base.tag * Obj.t) monad
  val input_value : in_channel -> 'v monad
  val input_value_list : in_channel list -> 'v list monad
  val flush : out_channel -> unit monad
  val pipe : unit -> (in_channel * out_channel)
  val fork_child : (unit -> unit) -> int
end

module type FLAG = sig
  type t
  val create : unit -> t
  val use : t -> unit
end

module type LIN = sig
  (** linear type constructor *)
  type 'a lin

  (** extract the value. raises LinFlag.InvalidEndpoint if the endpoint is already consumed *)
  val use : 'a lin -> 'a

  (** a generator for linear values *)
  type 'a gen

  (** create a generator *)
  val create : 'a -> 'a lin gen
  val create_nolin : 'a -> 'a gen

  (** create a dummy (for LinOCaml's pattern maching only) *)
  val create_dummy : 'a -> 'a lin

  (** generate a fresh linear value (possibly wrapped by objects) *)
  val fresh : 'a gen -> 'a

  val map_gen : ('a -> 'b) -> 'a gen -> 'b gen
  val merge_gen : ('a -> 'a -> 'a) -> 'a lin gen -> 'a lin gen -> 'a lin gen
  val lift_disj_merge : ('lr,'l,'r) Base.disj_merge -> ('lr gen, 'l gen, 'r gen) Base.disj_merge
end

module type ENDPOINTS = sig
  type 'a lin
  type 'a t (* = 'a Lin.gen list Mergeable.t *)
  val use : 'a lin -> 'a
  val make_lin : hook:unit lazy_t -> mergefun:('a -> 'a -> 'a) -> values:'a list -> 'a lin t
  val make_simple : 'a list -> 'a t
  val wrap_label : ('o, 'v) Base.method_ -> 'v t -> 'o t
  val fresh : 'a t -> int -> 'a
  val fresh_all : 'a t -> 'a list
  val force_merge : 'a t -> unit

  val make_recvar : 'a t lazy_t -> 'a t
  val make_disj_merge : ('lr,'l,'r) Base.disj_merge -> 'l t -> 'r t -> 'lr t
  val make_merge : 'a t -> 'a t -> 'a t
  val make_merge_list : 'a t list -> 'a t
end

module type LOCAL = sig
  type +'a monad
  type 't out
  type 't inp
  type 't lin
  val send : ('t Base.one * 'u) out lin -> 't -> 'u monad
  val sendmany : ('t list * 'u) out lin -> (int -> 't) -> 'u monad
  val receive : 't inp lin -> 't monad
  val close : Base.close -> unit
end
