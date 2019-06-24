
module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

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
  val choose : 'a event list -> 'a event
  val wrap : 'a event -> ('a -> 'b) -> 'b event
  val always : 'a -> 'a event
  val receive_list : 'a channel list -> 'a list event

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
end

module type LIN_FLAG = sig
  type t
  val create     : unit -> t
  val use        : t -> unit
  exception InvalidEndpoint
end

module type LIN_EP = sig
  type once
  type 'a t
  val make : (once -> 'a) -> 'a t
  val unrestricted : 'a list -> 'a t list
  val map_merge : ('a -> 'a -> 'a) -> 'a t list -> 'a t list -> 'a t list
  val out : 'a t list -> 'a list
  val map : ('a -> 'b) -> 'a t list -> 'b t list
  val map2 : ('a -> 'b -> 'c) -> 'a t list -> 'b t list -> 'c t list
end

module type LOCAL = sig
  type +'a monad
  type 't out
  type 't inp
  val send : ('t Base.one * 'u) out -> 't -> 'u monad
  val sendmany : ('t list * 'u) out -> (int -> 't) -> 'u monad
  val receive : 't inp -> 't monad
  val close : Base.close -> unit
end
